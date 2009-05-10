%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2009, Martin Logan
%%% @doc
%%%
%%% @end
%%% Created : 21 Apr 2009 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(dd_doc_server).

-behaviour(gen_server).

%% API
-export([start_link/1, notify/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {doc_spec}).
-include("doc_daemon.hrl").
-include("macros.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(DocSpec) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(DocSpec) ->
    gen_server:start_link(?MODULE, DocSpec, []).

%%--------------------------------------------------------------------
%% @doc callback for the rd_trans_server to notify this process when there
%%      have been files added to the repo.
%% @spec (Pid, ChangedFiles) -> void()
%% @end
%%--------------------------------------------------------------------
notify(Pid, ChangedFiles) ->
    gen_server:cast(Pid, {notify, ChangedFiles}).
   
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

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
init(DocSpec) ->
    Self = self(),
    rd_trans_server:subscribe(DocSpec#doc_spec.transition_id, fun(Files) -> notify(Self, Files) end),

    ok   = ewl_file:mkdir_p(DocSpec#doc_spec.repo_dir_path),

    dd_doc_builder:build_index_docs(DocSpec),

    {ok, #state{doc_spec = DocSpec}}.

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
handle_cast({notify, ChangedFiles}, #state{doc_spec = DocSpec} = State) ->
    handle_generations(ChangedFiles, DocSpec),
    dd_doc_builder:build_index_docs(DocSpec),
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
handle_info(_Msg, State) ->
    {noreply, State}.

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

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc Build docs for various packages in the repos
%% @end
%%--------------------------------------------------------------------
handle_generations([], _DocSpec) ->
    ok;
handle_generations(NewFiles, DocSpec) ->
    ?INFO_MSG("Packages to generate docs for ~p~n",[NewFiles]),

    lists:foreach(fun(FilePath) ->
			  try
			      {ok, Spec} = handle_generation(FilePath, DocSpec),
			      ?INFO_MSG("inserting spec ~p~n", [Spec]),
			      dd_store:insert(Spec)
			  catch
			      _C:Error ->
				  ?ERROR_MSG("documenation generation error for ~p: ~p~n", [FilePath, Error])
			  end
		  end, NewFiles).

handle_generation(PackageFileSuffix, DocSpec) ->
    case re:run(PackageFileSuffix, "/erts.") of
	{match, _} ->
	    % No docs for erts 
	    throw(erts_skipped);
	_ ->
	    ?INFO_MSG("Generating docs for ~p~n", [PackageFileSuffix]),
	    Elements    = ewr_repo_paths:decompose_suffix(PackageFileSuffix),
	    ErtsVsn     = fs_lists:get_val(erts_vsn, Elements),
	    Side        = fs_lists:get_val(side, Elements),
	    PackageName = fs_lists:get_val(package_name, Elements),
	    PackageVsn  = fs_lists:get_val(package_vsn, Elements),

	    FromPackagePath   = ewl_file:join_paths(DocSpec#doc_spec.repo_dir_path, PackageFileSuffix),
	    TmpPackageDirPath = epkg_util:unpack_to_tmp(FromPackagePath),
	    
	    Spec = 
		case Side of
		    "lib" ->
			RawSpec = #app_spec{name          = PackageName,
					    version       = PackageVsn,
					    transition_id = DocSpec#doc_spec.transition_id,
					    package_path  = FromPackagePath,
					    erts_vsn      = ErtsVsn},
			{ok, DocPath} = build_app_docs(TmpPackageDirPath, RawSpec, DocSpec),
			RawSpec#app_spec{doc_path = DocPath};
		    "releases" ->
			RawSpec = #release_spec{name          = PackageName,
						version       = PackageVsn,
						transition_id = DocSpec#doc_spec.transition_id,
						package_path  = FromPackagePath,
						erts_vsn      = ErtsVsn},
			{ok, DocPath} = build_release_docs(TmpPackageDirPath, RawSpec, DocSpec),
			RawSpec#release_spec{doc_path = DocPath}
		end,
	    
	    ewl_file:delete_dir(filename:dirname(TmpPackageDirPath)),
	    {ok, Spec}
    end.

build_app_docs(TmpPackageDirPath, #app_spec{name = PackageName} = Spec, DocSpec) ->
    NoDocList = DocSpec#doc_spec.no_doc_list,
    case lists:member({app, list_to_atom(PackageName)}, NoDocList) of
	true  ->
	    ?INFO_MSG("~p is in the no doc list; skipping~n", [PackageName]),
	    throw({in_no_doc_list, PackageName});
	false ->
	    build_app_docs2(TmpPackageDirPath, Spec, DocSpec)
    end.

build_app_docs2(PackageDirPath, _Spec, undefined) ->
    ?INFO_MSG("Doc transition spec undefined for ~p. Skipping doc building.~n", [PackageDirPath]),
    throw(skipped);
build_app_docs2(PackageDirPath, #app_spec{erts_vsn = ErtsVsn}, #doc_spec{generated_docs_base_dir = DocDirPath}) ->
    {ok, {AppName, AppVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(PackageDirPath),
    case catch edoc:application(list_to_atom(AppName), PackageDirPath, []) of
	ok -> 
	    GeneratedDocDirPath = filename:join([PackageDirPath, "doc"]),
	    LibDocDirPath       = filename:join([DocDirPath, "lib", ErtsVsn, AppName ++ "-" ++ AppVsn]),
	    ewl_file:mkdir_p(LibDocDirPath),
	    ?INFO_MSG("copy doc dir from ~s to ~s~n", [GeneratedDocDirPath, LibDocDirPath]),
	    ewl_file:copy_dir(GeneratedDocDirPath, LibDocDirPath),
	    {ok, LibDocDirPath};
	Error -> 
	    ?ERROR_MSG("doc failed for ~s-~s with ~p~n", [AppName, AppVsn, Error]),
	    throw({doc_failed, AppName, AppVsn, Error})
    end.

build_release_docs(TmpPackageDirPath, #release_spec{name = PackageName} = Spec, DocSpec) ->
    NoDocList = DocSpec#doc_spec.no_doc_list,
    case catch lists:member({release, list_to_atom(PackageName)}, NoDocList) of
	true  ->
	    ?INFO_MSG("~p is in the no doc list; skipping~n", [PackageName]),
	    throw({in_no_doc_list, PackageName});
	false ->
	    build_release_docs2(TmpPackageDirPath, Spec, DocSpec)
    end.

build_release_docs2(PackageDirPath, _Spec, undefined) ->
    ?INFO_MSG("Doc transition spec undefined for ~p. Skipping doc building.~n", [PackageDirPath]),
    throw(skipped);
build_release_docs2(PackageDirPath, #release_spec{erts_vsn = ErtsVsn}, #doc_spec{generated_docs_base_dir = DocDirPath}) ->
    {ok, {RelName, RelVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(PackageDirPath),
    ControlFilePath = ewl_file:join_paths(PackageDirPath, "control"),
    case filelib:is_file(ControlFilePath) of
	true -> 
	    RelDocBaseDirPath = ewl_file:join_paths(DocDirPath, "releases"),
	    RelDocDirPath = ewl_file:join_paths(RelDocBaseDirPath, RelName ++ "-" ++ RelVsn),
	    ewl_file:mkdir_p(RelDocDirPath),
	    ?INFO_MSG("copy doc dir from ~s to ~s~n", [ControlFilePath, RelDocDirPath]),
	    file:copy(ControlFilePath, ewl_file:join_paths(RelDocDirPath, "control")),
	    ok = dd_release_template:generate_release_doc(RelDocBaseDirPath, RelDocDirPath, ErtsVsn),
	    {ok, RelDocDirPath};
	false  -> 
	    ?ERROR_MSG("doc failed for ~s-~s because the release has no control file~n", [RelName, RelVsn]),
	    throw({doc_failed, RelName, RelVsn})
    end.
