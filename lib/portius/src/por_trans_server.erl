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
-export([start_link/6]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("macros.hrl").

-define(SERVER, ?MODULE). 

-record(state, {from_repo, 
		to_repo, 
		doc_dir, 
		inspection_frequency, 
		last_tree = {}, 
		app_index_file_path, 
		release_index_file_path,
	        doc_root}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(FromRepoDirPath, ToRepoDirPath, DocDirPath, AppIndexFilePath, DocRoot) -> 
%%       {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(FromRepoDirPath, ToRepoDirPath, DocDirPath, AppIndexFilePath, ReleaseIndexFilePath, DocRoot) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, 
			  [FromRepoDirPath, 
			   ToRepoDirPath, 
			   DocDirPath, 
			   AppIndexFilePath, 
			   ReleaseIndexFilePath, 
			   DocRoot], []).

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
init([FromRepoDirPath, ToRepoDirPath, DocDirPath, AppIndexFilePath, ReleaseIndexFilePath, DocRoot]) ->
    {ok, Timeout} = gas:get_env(portius, inspection_frequency),
    ?INFO_MSG("initializing with:~n - from repo ~s~n - to repo ~s~n - doc path ~s~n - inspection frequency ~p~n" ++ 
	      " - web server doc root ~s~n",
	      [FromRepoDirPath, ToRepoDirPath, DocDirPath, Timeout, DocRoot]),
    ok = ewl_file:mkdir_p(DocDirPath),
    ok = ewl_file:mkdir_p(ToRepoDirPath),
    ?INFO_MSG("ensured that ~s and ~s are present~n", [DocDirPath, ToRepoDirPath]),
    ToTree        = por_file_tree:create_tree(ToRepoDirPath),
    ?INFO_MSG("created initial tree from ~s~n", [ToRepoDirPath]),
    AIR = (catch por_app_template:create_app_index_page(AppIndexFilePath, DocDirPath, DocRoot)),
    ?INFO_MSG("result of create app index page call ~p~n", [AIR]),
    State = #state{from_repo                = FromRepoDirPath, 
		   to_repo                  = ToRepoDirPath, 
		   doc_dir                  = DocDirPath, 
		   inspection_frequency     = Timeout, 
		   last_tree                = ToTree,
		   app_index_file_path      = AppIndexFilePath, 
		   release_index_file_path  = ReleaseIndexFilePath, 
		   doc_root                 = DocRoot},
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
    #state{from_repo               = FR,
	   to_repo                 = TR,
	   inspection_frequency    = Timeout, 
	   last_tree               = LastTree,
	   doc_dir                 = DocDirPath,
	   app_index_file_path     = AppIndexFilePath, 
	   release_index_file_path = ReleaseIndexFilePath, 
	   doc_root                = DocRoot} = State,
    
    Tree     = por_file_tree:create_tree(FR),
    TreeDiff = por_file_tree:find_additions(LastTree, Tree),
    handle_transitions(TreeDiff, FR, TR, DocDirPath, AppIndexFilePath, ReleaseIndexFilePath, DocRoot),
    {noreply, State#state{last_tree = Tree}, Timeout}.


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
handle_transitions([], _FromRepo, _ToRepo, _DocDirPath, _AppIndexFilePath, _ReleaseIndexFilePath, _DocRoot) ->
    ok;
handle_transitions(TreeDiff, FromRepo, ToRepo, DocDirPath, AppIndexFilePath, ReleaseIndexFilePath, DocRoot) ->
    ?INFO_MSG("tree diff ~p~ngoing to ~s~n", [TreeDiff, ToRepo]),
    NewFiles = lists:map(fun(Path) -> 
				 {ok, {_, Rest}} = fs_lists:separate_by_token(Path, "/"),
				 Rest
			 end, por_file_tree:file_paths(TreeDiff)),
    ?INFO_MSG("Files to be transfered ~p~n",[NewFiles]),
    lists:foreach(fun(FilePath) ->
			  case regexp:match(FilePath, ".*Meta.*") of
			      {match, _, _} -> 
				  ok;
			      _             -> 
				  case catch handle_transition(FilePath, FromRepo, ToRepo, DocDirPath, 
							       AppIndexFilePath, ReleaseIndexFilePath, DocRoot) of
				      ok    -> ok;
				      Error -> ?ERROR_MSG("handle transition returned error ~p~n", [Error])
				  end
			  end
		  end, NewFiles).
 				  
%%--------------------------------------------------------------------
%% @private
%% @doc handle the transition of a package from one repo to another
%% @end
%%--------------------------------------------------------------------
handle_transition(PackageFileSuffix, FromRepo, ToRepo, DocDirPath, AppIndexFilePath, ReleaseIndexFilePath, DocRoot) ->
    PackageFilePath = ewl_file:join_paths(FromRepo, PackageFileSuffix), 
    ?INFO_MSG("Transitioning ~s from ~s to ~s~n", [PackageFilePath, FromRepo, ToRepo]),

    case regexp:match(PackageFilePath, "/erts.") of
	{match, _, _} ->
	    ?INFO_MSG("transitioning erts ~p~n", [PackageFilePath]),
	    Elements = ewr_repo_paths:decompose_suffix(PackageFileSuffix),
	    ErtsVsn  = fs_lists:get_val(erts_vsn, Elements),
	    Area     = fs_lists:get_val(area, Elements),
	    transition_erts(ErtsVsn, Area, FromRepo, ToRepo);
	_ ->
	    ?INFO_MSG("transitioning ~p~n", [PackageFilePath]),
	    Elements    = ewr_repo_paths:decompose_suffix(PackageFileSuffix),
	    ErtsVsn     = fs_lists:get_val(erts_vsn, Elements),
	    Side        = fs_lists:get_val(side, Elements),
	    Area        = fs_lists:get_val(area, Elements),
	    PackageName = fs_lists:get_val(package_name, Elements),
	    PackageVsn  = fs_lists:get_val(package_vsn, Elements),
	    case Side of
		"lib" ->
		    transition_app(ErtsVsn, Area, Side, PackageName, PackageVsn, FromRepo, 
				   ToRepo, DocDirPath, AppIndexFilePath, DocRoot);
		"releases" ->
		    transition_release(ErtsVsn, Area, Side, PackageName, PackageVsn, FromRepo, 
				       ToRepo, DocDirPath, ReleaseIndexFilePath, DocRoot)
	    end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Transition an erts package from the FromRepo to the ToRepo.
%% @end
%%--------------------------------------------------------------------
transition_erts(ErtsVsn, Area, FromRepo, ToRepo) ->
    PackageSuffix     = ewr_repo_paths:erts_package_suffix(ErtsVsn, Area),
    FromPackagePath   = ewl_file:join_paths(FromRepo, PackageSuffix),
    TmpPackageDirPath = epkg_util:unpack_to_tmp(FromPackagePath),

    case epkg_validation:is_package_erts(TmpPackageDirPath) of
	true ->
	    copy_over_erts(ErtsVsn, Area, FromRepo, ToRepo);
	false ->
	    ?ERROR_MSG("~s failed validation~n", [FromPackagePath])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Copy over an erts package from one repo to another.
%% @end
%%--------------------------------------------------------------------
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
transition_app(ErtsVsn, Area, "lib" = Side, PackageName, PackageVsn, FromRepo, ToRepo, DocDirPath, AppIndexFilePath, DocRoot) ->
    PackageSuffix     = ewr_repo_paths:package_suffix(ErtsVsn, Area, Side, PackageName, PackageVsn),
    FromPackagePath   = ewl_file:join_paths(FromRepo, PackageSuffix),
    TmpPackageDirPath = epkg_util:unpack_to_tmp(FromPackagePath),

    case epkg_validation:is_package_an_app(TmpPackageDirPath) of
	true ->
	    %% @todo right now docs are optional - in the future we can email the package owner with a notification
	    (catch build_app_docs(TmpPackageDirPath, DocDirPath, ErtsVsn)),
	    AIR = (catch por_template:create_app_index_page(AppIndexFilePath, DocDirPath, DocRoot)),
	    ?INFO_MSG("result of create app index page call ~p~n", [AIR]),
	    copy_over_app(ErtsVsn, Area, Side, PackageName, PackageVsn, FromRepo, ToRepo);
	false ->
	    ?ERROR_MSG("~s failed validation~n", [FromPackagePath])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Copy over an app package from one repo to another.
%% @end
%%--------------------------------------------------------------------
copy_over_app(ErtsVsn, Area, "lib" = Side, PackageName, PackageVsn, FromRepo, ToRepo) ->
    PackageSuffix      = ewr_repo_paths:package_suffix(ErtsVsn, Area, Side, PackageName, PackageVsn),
    DotAppFileSuffix   = ewr_repo_paths:dot_app_file_suffix(ErtsVsn, PackageName, PackageVsn),
    FromPackagePath    = ewl_file:join_paths(FromRepo, PackageSuffix),
    ToPackagePath      = ewl_file:join_paths(ToRepo, PackageSuffix),
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
transition_release(ErtsVsn, Area, Side, PackageName, PackageVsn, FromRepo, ToRepo, DocDirPath, ReleaseIndexFilePath, DocRoot) ->
    PackageSuffix     = ewr_repo_paths:package_suffix(ErtsVsn, Area, Side, PackageName, PackageVsn),
    FromPackagePath   = ewl_file:join_paths(FromRepo, PackageSuffix),
    TmpPackageDirPath = epkg_util:unpack_to_tmp(FromPackagePath),

    case epkg_validation:is_package_a_release(TmpPackageDirPath) of
	true ->
	    build_release_docs(TmpPackageDirPath, DocDirPath, ErtsVsn),
	    RIR = (catch por_template:create_app_index_page(ReleaseIndexFilePath, DocDirPath, DocRoot)),
	    ?INFO_MSG("result of create release index page call ~p~n", [RIR]),
	    copy_over_release(ErtsVsn, Area, Side, PackageName, PackageVsn, FromRepo, ToRepo);
	false ->
	    ?ERROR_MSG("~s failed validation~n", [FromPackagePath])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Copy over an release package from one repo to another.
%% @end
%%--------------------------------------------------------------------
copy_over_release(ErtsVsn, Area, "releases" = Side, PackageName, PackageVsn, FromRepo, ToRepo) ->
    PackageSuffix       = ewr_repo_paths:package_suffix(ErtsVsn, Area, Side, PackageName, PackageVsn),
    DotRelFileSuffix    = ewr_repo_paths:dot_rel_file_suffix(ErtsVsn, PackageName, PackageVsn),
    ControlFileSuffix   = ewr_repo_paths:release_control_file_suffix(ErtsVsn, PackageName, PackageVsn),
    FromPackagePath     = ewl_file:join_paths(FromRepo, PackageSuffix),
    ToPackagePath       = ewl_file:join_paths(ToRepo, PackageSuffix),
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

%%--------------------------------------------------------------------
%% @private
%% @doc Build the documentation for an application.
%% @spec build_app_docs(PackageDirPath, DocDirPath, ErtsVsn) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
build_app_docs(PackageDirPath, DocDirPath, ErtsVsn) ->
    {ok, {AppName, AppVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(PackageDirPath),
    case catch edoc:application(list_to_atom(AppName), PackageDirPath, []) of
	ok -> 
	    GeneratedDocDirPath = ewl_file:join_paths(PackageDirPath, "doc"),
	    LibDocDirPath       = ewl_file:join_paths(ewl_file:join_paths(DocDirPath, ewl_file:join_paths("lib", ErtsVsn)), 
									  AppName ++ "-" ++ AppVsn),
	    ewl_file:mkdir_p(LibDocDirPath),
	    ?INFO_MSG("copy doc dir from ~s to ~s~n", [GeneratedDocDirPath, LibDocDirPath]),
	    ewl_file:copy_dir(GeneratedDocDirPath, LibDocDirPath);
	Error  -> 
	    ?ERROR_MSG("doc failed for ~s-~s wieh ~p~n", [AppName, AppVsn, Error]),
	    {error, {doc_failed, AppName, AppVsn, Error}}
    end.
	    
%%--------------------------------------------------------------------
%% @private
%% @doc Build the documentation for an release.
%% @spec build_release_docs(PackageDirPath, DocDirPath, ErtsVsn) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
build_release_docs(PackageDirPath, DocDirPath, ErtsVsn) ->
    {ok, {RelName, RelVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(PackageDirPath),
    ControlFilePath = ewl_file:join_paths(PackageDirPath, "control"),
    case filelib:is_file(PackageDirPath) of
	true -> 
	    RelDocDirPath = ewl_file:join_paths(ewl_file:join_paths(DocDirPath, "releases"), RelName ++ "-" ++ RelVsn),
	    ewl_file:mkdir_p(RelDocDirPath),
	    ?INFO_MSG("copy doc dir from ~s to ~s~n", [ControlFilePath, RelDocDirPath]),
	    file:copy(ControlFilePath, RelDocDirPath),
	    por_release_template:generate_release_doc(RelDocDirPath, ErtsVsn);
	false  -> 
	    ?ERROR_MSG("doc failed for ~s-~s because the release has no control file~n", [RelName, RelVsn]),
	    {error, {doc_failed, RelName, RelVsn}}
    end.
	    

    
