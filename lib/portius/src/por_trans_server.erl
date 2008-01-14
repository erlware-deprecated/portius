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
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("macros.hrl").

-define(SERVER, ?MODULE). 

-record(state, {from_repo, to_repo, doc_dir, inspection_frequency, last_tree = {}}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(FromRepoDirPath, ToRepoDirPath, DocDirPath) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(FromRepoDirPath, ToRepoDirPath, DocDirPath) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [FromRepoDirPath, ToRepoDirPath, DocDirPath], []).

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
init([FromRepoDirPath, ToRepoDirPath, DocDirPath]) ->
    ok            = ewl_file:mkdir_p(DocDirPath),
    ok            = ewl_file:mkdir_p(ToRepoDirPath),
    {ok, Timeout} = gas:get_env(portius, inspection_frequency),
    ToTree        = por_file_tree:create_tree(ToRepoDirPath),
    State = #state{from_repo            = FromRepoDirPath, 
		   to_repo              = ToRepoDirPath, 
		   doc_dir              = DocDirPath, 
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
handle_info(_Info, #state{from_repo = FR, to_repo = TR, inspection_frequency = Timeout, 
			  last_tree = LastTree, doc_dir = DocDirPath} = State) ->
    Tree     = por_file_tree:create_tree(FR),
    TreeDiff = por_file_tree:find_additions(LastTree, Tree),
    ?INFO_MSG("tree diff ~p~ngoing to ~s~n", [TreeDiff, TR]),
    handle_transitions(TreeDiff, FR, TR, DocDirPath),
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
handle_transitions([], _FromRepo, _ToRepo, _DocDirPath) ->
    ok;
handle_transitions(TreeDiff, FromRepo, ToRepo, DocDirPath) ->
    NewFiles = lists:map(fun(Path) -> 
				 {ok, {_, Rest}} = fs_lists:separate_by_token(Path, "/"),
				 Rest
			 end, por_file_tree:file_paths(TreeDiff)),
    lists:foreach(fun(FilePath) ->
			  case regexp:match(FilePath, ".*Meta.*") of
			      {match, _, _} -> 
				  ok;
			      _             -> 
				  case catch handle_transition(FilePath, FromRepo, ToRepo, DocDirPath) of
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
handle_transition(PackageFileSuffix, FromRepo, ToRepo, DocDirPath) ->
    PackageFilePath = ewl_file:join_paths(FromRepo, PackageFileSuffix), 
    ?INFO_MSG("Transitioning ~s from ~s to ~s~n", [PackageFilePath, FromRepo, ToRepo]),

    Elements    = ewr_repo_paths:decompose_suffix(PackageFileSuffix),
    ErtsVsn     = fs_lists:get_val(erts_vsn, Elements),
    Side        = fs_lists:get_val(side, Elements),
    Area        = fs_lists:get_val(area, Elements),
    PackageName = fs_lists:get_val(package_name, Elements),
    PackageVsn  = fs_lists:get_val(package_vsn, Elements),
    transition(ErtsVsn, Area, Side, PackageName, PackageVsn, FromRepo, ToRepo, DocDirPath).

%%--------------------------------------------------------------------
%% @private
%% @doc Transition apps on the lib side and releases on the releases side.
%% @spec transition(ErtsVsn, Area, Side, PackageName, PackageVsn, FromRepo, ToRepo, DocDirPath) -> ok
%% @end
%%--------------------------------------------------------------------
transition(ErtsVsn, Area, "lib" = Side, PackageName, PackageVsn, FromRepo, ToRepo, DocDirPath) ->
    PackageSuffix     = ewr_repo_paths:package_suffix(ErtsVsn, Area, Side, PackageName, PackageVsn),
    FromPackagePath   = ewl_file:join_paths(FromRepo, PackageSuffix),
    TmpPackageDirPath = epkg_util:unpack_to_tmp(FromPackagePath),
    case epkg_validation:is_package_an_app(TmpPackageDirPath) of
	true ->
	    %% @todo right now docs are optional - in the future we can email the package owner with a notification
	    (catch build_app_docs(TmpPackageDirPath, DocDirPath)),
	    copy_over_app(ErtsVsn, Area, Side, PackageName, PackageVsn, FromRepo, ToRepo);
	false ->
	    ?ERROR_MSG("~s failed validation~n", [FromPackagePath])
    end;
transition(ErtsVsn, Area, "releases" = Side, PackageName, PackageVsn, FromRepo, ToRepo, _DocDirPath) ->
    PackageSuffix   = ewr_repo_paths:package_suffix(ErtsVsn, Area, Side, PackageName, PackageVsn),
    FromPackagePath = ewl_file:join_paths(FromRepo, PackageSuffix),
    ToPackagePath   = ewl_file:join_paths(ToRepo, PackageSuffix),

    ?INFO_MSG("ewl_file:copy_dir(~p, ~p)", [FromPackagePath, ToPackagePath]),

    ewl_file:mkdir_p(filename:dirname(ToPackagePath)),
    ewl_file:copy_dir(FromPackagePath, ToPackagePath).

%%--------------------------------------------------------------------
%% @private
%% @doc Copy over an app package from one repo to another.
%% @spec copy_over_app(ErtsVsn, Area, Side, PackageName, PackageVsn, FromRepo, ToRepo) -> ok | exit()
%% @end
%%--------------------------------------------------------------------
copy_over_app(ErtsVsn, Area, "lib" = Side, PackageName, PackageVsn, FromRepo, ToRepo) ->
    PackageSuffix      = ewr_repo_paths:package_suffix(ErtsVsn, Area, Side, PackageName, PackageVsn),
    DotAppFileSuffix   = ewr_repo_paths:dot_app_file_suffix(ErtsVsn, PackageName, PackageVsn),
    FromPackagePath    = ewl_file:join_paths(FromRepo, PackageSuffix),
    ToPackagePath      = ewl_file:join_paths(ToRepo, PackageSuffix),
    FromDotAppFilePath = ewl_file:join_paths(FromRepo, DotAppFileSuffix),
    ToDotAppFilePath   = ewl_file:join_paths(ToRepo, DotAppFileSuffix),

    
    ?INFO_MSG("ewl_file:copy_dir(~p, ~p)", [FromPackagePath, ToPackagePath]),
    ?INFO_MSG("ewl_file:copy_dir(~p, ~p)", [FromDotAppFilePath, ToDotAppFilePath]),

    ewl_file:mkdir_p(filename:dirname(ToPackagePath)),
    ewl_file:mkdir_p(filename:dirname(ToDotAppFilePath)),
    ewl_file:copy_dir(FromPackagePath, ToPackagePath),
    ewl_file:copy_dir(FromDotAppFilePath, ToDotAppFilePath).

%%--------------------------------------------------------------------
%% @private
%% @doc Build the documentation for an application.
%% @spec build_app_docs(PackageDirPath, DocDirPath) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
build_app_docs(PackageDirPath, DocDirPath) ->
    {ok, {AppName, AppVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(PackageDirPath),
    ?INFO_MSG("edoc:application(~p, ~p, [])~n", [list_to_atom(AppName), PackageDirPath]),
    case catch edoc:application(list_to_atom(AppName), PackageDirPath, []) of
	ok -> 
	    GeneratedDocDirPath = ewl_file:join_paths(PackageDirPath, "doc"),
	    LibDocDirPath       = ewl_file:join_paths(ewl_file:join_paths(DocDirPath, "lib"), AppName ++ "-" ++ AppVsn),
	    ewl_file:mkdir_p(LibDocDirPath),
	    ewl_file:copy_dir(GeneratedDocDirPath, LibDocDirPath);
	_  -> 
	    ?ERROR_MSG("doc failed for ~s-~s~n", [AppName, AppVsn]),
	    {error, {doc_failed, AppName, AppVsn}}
    end.
	    


    
