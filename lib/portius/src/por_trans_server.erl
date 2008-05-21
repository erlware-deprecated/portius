%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2008, Martin Logan
%%% @doc
%%%  Handles the transfer of packages from one repo location to another.
%%% @end
%%% Created :  1 Jan 2008 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(por_trans_server).

-behaviour(gen_server).

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Prevent warnings on dynamically called functions
-export([
	 fetch_doc_spec/1,
	 fetch_signatures/1,
	 fetch_no_doc_list/1
	]).

-include("macros.hrl").
-include("portius.hrl").

-define(SERVER, ?MODULE). 

-record(state, {transition_spec, inspection_frequency, last_tree}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server.
%%
%% @spec start_link(TransitionId::atom(), FromRepoDirPath, ToRepoDirPath, SignType) -> 
%%       {ok, Pid} | ignore | {error, Error}
%% where
%%  SignType = signed | unsigned
%% @end
%%--------------------------------------------------------------------
start_link(TransitionId, FromRepoDirPath, ToRepoDirPath, SignType) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [TransitionId, FromRepoDirPath, ToRepoDirPath, SignType], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([TransitionId, FromRepoDirPath, ToRepoDirPath, SignType]) ->
    {ok, Timeout} = gas:get_env(portius, inspection_frequency),
    ok = ewl_file:mkdir_p(ToRepoDirPath),
    ToTree = por_file_tree:create_tree(ToRepoDirPath),
    ?INFO_MSG("created initial tree from ~s~n", [ToRepoDirPath]),

    TransitionSpec = #transition_spec{
      transition_id = TransitionId,
      from_repo = FromRepoDirPath, 
      to_repo   = ToRepoDirPath,
      sign_type = SignType,
      children  = fetch_children(TransitionId)
     },

    ?INFO_MSG("TransitionSpec ~p~n", [TransitionSpec]),
    
    por_doc_builder:build_index_docs(fs_lists:get_val(doc_spec, TransitionSpec#transition_spec.children)),						     
    State = #state{transition_spec      = TransitionSpec, 
		   inspection_frequency = Timeout, 
		   last_tree            = ToTree},

    {ok, State, Timeout}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    #state{transition_spec      = TransitionSpec, 
	   inspection_frequency = Timeout, 
	   last_tree            = LastTree} = State,

    #transition_spec{transition_id = TransitionId,
		     from_repo = FR,
		     children = Children} = TransitionSpec,
    
    Tree     = por_file_tree:create_tree(FR),
    TreeDiff = por_file_tree:find_additions(LastTree, Tree),
    handle_transitions(TreeDiff, TransitionSpec),

    case TreeDiff of
	[] -> ok;
	_  -> por_doc_builder:build_index_docs(fs_lists:get_val(doc_spec, Children))
    end,
    
    {noreply,
     State#state{last_tree = Tree, transition_spec = TransitionSpec#transition_spec{children = fetch_children(TransitionId)}},
     Timeout}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc handle the transitions of all packages from one repo to another
%% @end
%%--------------------------------------------------------------------
handle_transitions([], _TransitionSpec) ->
    ok;
handle_transitions(TreeDiff, TransitionSpec) ->
    NewFiles = lists:map(fun(Path) -> 
				 {ok, {_, Rest}} = fs_lists:separate_by_token(Path, "/"),
				 Rest
			 end, por_file_tree:file_paths(TreeDiff)),
    ?INFO_MSG("Files to be transfered ~p~n",[NewFiles]),
    lists:foreach(fun(FilePath) ->
			  case regexp:match(FilePath, ".*Meta.*") of
			      {match, _, _} -> 
				  ok;
			      _ -> 
				  case catch handle_transition(FilePath, TransitionSpec) of
				      ok    -> ok;
				      Error -> ?ERROR_MSG("handle transition returned error ~p~n", [Error])
				  end
			  end
		  end, NewFiles).
 				  
handle_transition(PackageFileSuffix, TransitionSpec) ->
    case regexp:match(PackageFileSuffix, "/erts.") of
	{match, _, _} ->
	    transition_erts(PackageFileSuffix, TransitionSpec);
	_ ->
	    ?INFO_MSG("transitioning ~p~n", [PackageFileSuffix]),
	    Elements    = ewr_repo_paths:decompose_suffix(PackageFileSuffix),
	    ErtsVsn     = fs_lists:get_val(erts_vsn, Elements),
	    Side        = fs_lists:get_val(side, Elements),
	    Area        = fs_lists:get_val(area, Elements),
	    PackageName = fs_lists:get_val(package_name, Elements),
	    PackageVsn  = fs_lists:get_val(package_vsn, Elements),
	    case Side of
		"lib" ->
		    transition_app(ErtsVsn, Area, Side, PackageName, PackageVsn, TransitionSpec);
		"releases" ->
		    transition_release(ErtsVsn, Area, Side, PackageName, PackageVsn, TransitionSpec)
	    end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Transition an erts package from the FromRepo to the ToRepo.
%% @end
%%--------------------------------------------------------------------
transition_erts(PackageFileSuffix, TransitionSpec) ->
    #transition_spec{
		 from_repo  = FromRepoDirPath, 
		 to_repo    = ToRepoDirPath
		} = TransitionSpec,
    
    PackageFilePath = ewl_file:join_paths(FromRepoDirPath, PackageFileSuffix), 
    ?INFO_MSG("Transitioning ~s from ~s to ~s~n", [PackageFilePath, FromRepoDirPath, ToRepoDirPath]),
    Elements = ewr_repo_paths:decompose_suffix(PackageFileSuffix),
    ErtsVsn  = fs_lists:get_val(erts_vsn, Elements),
    Area     = fs_lists:get_val(area, Elements),

    PackageFileSuffix     = ewr_repo_paths:erts_package_suffix(ErtsVsn, Area),
    FromPackagePath   = ewl_file:join_paths(FromRepoDirPath, PackageFileSuffix),
    TmpPackageDirPath = epkg_util:unpack_to_tmp(FromPackagePath),

    case epkg_validation:is_package_erts(TmpPackageDirPath) of
	true ->
	    case por_auth:validate_signature(erts, PackageFileSuffix, TransitionSpec) of
		ok     -> copy_over_erts(ErtsVsn, Area, FromRepoDirPath, ToRepoDirPath);
		_Error -> ok
	    end;
	false ->
	    ?ERROR_MSG("~s failed validation~n", [FromPackagePath])
    end.

copy_over_erts(ErtsVsn, Area, FromRepo, ToRepo) ->
    ErtsSuffix       = ewr_repo_paths:erts_package_suffix(ErtsVsn, Area),
    FromErtsFilePath = ewl_file:join_paths(FromRepo, ErtsSuffix),
    ToErtsFilePath   = ewl_file:join_paths(ToRepo, ErtsSuffix),
    
    ?INFO_MSG("copy dir from ~s to ~s~n", [FromErtsFilePath, ToErtsFilePath]),
    
    ewl_file:mkdir_p(filename:dirname(ToErtsFilePath)),
    ewl_file:copy_dir(FromErtsFilePath, ToErtsFilePath).
    

%%--------------------------------------------------------------------
%% @private
%% @doc Transition an app from the FromRepo to the ToRepo.
%% @end
%%--------------------------------------------------------------------
transition_app(ErtsVsn, Area, "lib" = Side, PackageName, PackageVsn, TransitionSpec) ->
    FromRepo = TransitionSpec#transition_spec.from_repo,
    ToRepo   = TransitionSpec#transition_spec.to_repo,
    Children = TransitionSpec#transition_spec.children,
    PackageFileSuffix     = ewr_repo_paths:package_suffix(ErtsVsn, Area, Side, PackageName, PackageVsn),
    FromPackagePath   = ewl_file:join_paths(FromRepo, PackageFileSuffix),
    TmpPackageDirPath = epkg_util:unpack_to_tmp(FromPackagePath),

    case epkg_validation:is_package_an_app(TmpPackageDirPath) of
	true ->
	    %% @todo right now docs are optional - in the future we can email the package owner with a notification
	    case por_auth:validate_signature(Side, PackageFileSuffix, TransitionSpec) of
		ok -> 
		    DocSpec = fs_lists:get_val(doc_spec, Children),						     
		    build_app_docs(PackageName, TmpPackageDirPath, ErtsVsn, DocSpec, Children),
		    copy_over_app(ErtsVsn, Area, Side, PackageName, PackageVsn, FromRepo, ToRepo);
		_Error ->
		    ok
	    end;
	false ->
	    ?ERROR_MSG("~s failed validation~n", [FromPackagePath])
    end.

build_app_docs(PackageName, TmpPackageDirPath, ErtsVsn, DocSpec, Children) ->
    NoDocList = fs_lists:get_val(no_doc_list, Children),						     
    case catch lists:member({app, list_to_atom(PackageName)}, NoDocList) of
	true  ->
	    ?INFO_MSG("~p is in the no doc list; skipping~n", [PackageName]),
	    ok;
	false ->
	    (catch por_doc_builder:build_app_docs(TmpPackageDirPath, ErtsVsn, DocSpec))
    end.
    
copy_over_app(ErtsVsn, Area, "lib" = Side, PackageName, PackageVsn, FromRepo, ToRepo) ->
    PackageFileSuffix      = ewr_repo_paths:package_suffix(ErtsVsn, Area, Side, PackageName, PackageVsn),
    DotAppFileSuffix   = ewr_repo_paths:dot_app_file_suffix(ErtsVsn, PackageName, PackageVsn),
    FromPackagePath    = ewl_file:join_paths(FromRepo, PackageFileSuffix),
    ToPackagePath      = ewl_file:join_paths(ToRepo, PackageFileSuffix),
    FromDotAppFilePath = ewl_file:join_paths(FromRepo, DotAppFileSuffix),
    ToDotAppFilePath   = ewl_file:join_paths(ToRepo, DotAppFileSuffix),
    
    ?INFO_MSG("copy dir from ~s to ~s~n", [FromPackagePath, ToPackagePath]),
    ?INFO_MSG("copy dir from ~s to ~s~n", [FromDotAppFilePath, ToDotAppFilePath]),

    ewl_file:mkdir_p(filename:dirname(ToPackagePath)),
    ewl_file:mkdir_p(filename:dirname(ToDotAppFilePath)),
    ewl_file:copy_dir(FromPackagePath, ToPackagePath),
    ewl_file:copy_dir(FromDotAppFilePath, ToDotAppFilePath).

%%--------------------------------------------------------------------
%% @private
%% @doc Transition a release package from the FromRepo to the ToRepo.
%% @end
%%--------------------------------------------------------------------
transition_release(ErtsVsn, Area, Side, PackageName, PackageVsn, TransitionSpec) ->
    FromRepo = TransitionSpec#transition_spec.from_repo,
    ToRepo   = TransitionSpec#transition_spec.to_repo,
    Children = TransitionSpec#transition_spec.children,
    PackageFileSuffix     = ewr_repo_paths:package_suffix(ErtsVsn, Area, Side, PackageName, PackageVsn),
    FromPackagePath   = ewl_file:join_paths(FromRepo, PackageFileSuffix),
    TmpPackageDirPath = epkg_util:unpack_to_tmp(FromPackagePath),

    case epkg_validation:is_package_a_release(TmpPackageDirPath) of
	true ->
	    case por_auth:validate_signature(Side, PackageFileSuffix, TransitionSpec) of
		ok -> 
		    DocSpec = fs_lists:get_val(doc_spec, Children),						     
		    build_release_docs(PackageName, TmpPackageDirPath, ErtsVsn, DocSpec, Children),
		    copy_over_release(ErtsVsn, Area, Side, PackageName, PackageVsn, FromRepo, ToRepo);
		_Error ->
		    ok
	    end;
	false ->
	    ?ERROR_MSG("~s failed validation~n", [FromPackagePath])
    end.

build_release_docs(PackageName, TmpPackageDirPath, ErtsVsn, DocSpec, Children) ->
    NoDocList = fs_lists:get_val(no_doc_list, Children),						     
    case catch lists:member({release, list_to_atom(PackageName)}, NoDocList) of
	true  ->
	    ?INFO_MSG("~p is in the no doc list; skipping~n", [PackageName]),
	    ok;
	false ->
	    (catch por_doc_builder:build_release_docs(TmpPackageDirPath, ErtsVsn, DocSpec))
    end.

copy_over_release(ErtsVsn, Area, "releases" = Side, PackageName, PackageVsn, FromRepo, ToRepo) ->
    PackageFileSuffix       = ewr_repo_paths:package_suffix(ErtsVsn, Area, Side, PackageName, PackageVsn),
    DotRelFileSuffix    = ewr_repo_paths:dot_rel_file_suffix(ErtsVsn, PackageName, PackageVsn),
    ControlFileSuffix   = ewr_repo_paths:release_control_file_suffix(ErtsVsn, PackageName, PackageVsn),
    FromPackagePath     = ewl_file:join_paths(FromRepo, PackageFileSuffix),
    ToPackagePath       = ewl_file:join_paths(ToRepo, PackageFileSuffix),
    FromDotRelFilePath  = ewl_file:join_paths(FromRepo, DotRelFileSuffix),
    ToDotRelFilePath    = ewl_file:join_paths(ToRepo, DotRelFileSuffix),
    FromControlFilePath = ewl_file:join_paths(FromRepo, ControlFileSuffix),
    ToControlFilePath   = ewl_file:join_paths(ToRepo, ControlFileSuffix),

    
    ?INFO_MSG("copy dir from ~s to ~s~n", [FromPackagePath, ToPackagePath]),
    ?INFO_MSG("copy dir from ~s to ~s~n", [FromDotRelFilePath, ToDotRelFilePath]),
    ?INFO_MSG("copy dir from ~s to ~s~n", [FromControlFilePath, ToControlFilePath]),

    ewl_file:mkdir_p(filename:dirname(ToPackagePath)),
    ewl_file:mkdir_p(filename:dirname(ToDotRelFilePath)),
    ewl_file:mkdir_p(filename:dirname(ToControlFilePath)),
    ewl_file:copy_dir(FromPackagePath, ToPackagePath),
    ewl_file:copy_dir(FromDotRelFilePath, ToDotRelFilePath),
    ewl_file:copy_dir(FromControlFilePath, ToControlFilePath).

	    
fetch_children(TransitionId) ->
    lists:foldl(fun(Key, Acc) ->
			F = list_to_atom(lists:flatten(["fetch_", atom_to_list(Key)])),
			case ?MODULE:F(TransitionId) of
			    undefined -> Acc;
			    Value     -> [{Key, Value}|Acc]
			end
		end,
		[],
		[signatures, doc_spec, no_doc_list]).
				
			
fetch_doc_spec(TransitionId) ->
    case gas:get_env(portius, doc_specs) of
	{ok, DocSpecs} ->
	    case lists:keysearch(TransitionId, 1, DocSpecs) of
		{value, {TransitionId, W, G, Asrc, A, Rsrc, R}} ->
		    #doc_spec{webserver_doc_root      = W,
			      generated_docs_base_dir = G,
			      app_index_file_src      = Asrc,
			      app_index_file          = A,
			      release_index_file_src  = Rsrc,
			      release_index_file      = R
			     };
		_ ->
		    undefined
	    end;
	_ ->
	    undefined
    end.

fetch_no_doc_list(TransitionId) ->
    case gas:get_env(portius, no_doc_list) of
	{ok, NoDocLists} ->
	    case lists:keysearch(TransitionId, 1, NoDocLists) of
		{value, {_TransitionID, NoDocList}} ->
		    NoDocList;
		_ ->
		    undefined
	    end;
	_ ->
	    undefined
    end.

fetch_signatures(TransitionId) ->
    case gas:get_env(portius, signatures) of
	{ok, Signatures} ->
	    [E || E <- Signatures, element(1, E) == TransitionId];
	_ ->
	    undefined
    end.

			
