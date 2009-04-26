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
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {doc_spec, inspection_frequency, last_tree}).
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
    {ok, Timeout} = gas:get_env(doc_daemon, inspection_frequency),

    RepoDirPath = DocSpec#doc_spec.repo_dir_path,
    ok   = ewl_file:mkdir_p(RepoDirPath),
    Tree = rd_file_tree:create_empty_tree(filename:basename(RepoDirPath)),
    ?INFO_MSG("created initial tree from ~s~n", [RepoDirPath]),

    dd_doc_builder:build_index_docs(DocSpec),

    {ok, #state{doc_spec = DocSpec, inspection_frequency = Timeout, last_tree = Tree}, Timeout}.

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
handle_info(timeout, State) ->
    #state{doc_spec             = DocSpec, 
	   inspection_frequency = Timeout, 
	   last_tree            = LastTree} = State,

    Tree     = rd_file_tree:create_tree(DocSpec#doc_spec.repo_dir_path),
    TreeDiff = rd_file_tree:find_additions(LastTree, Tree),
    handle_transitions(TreeDiff, DocSpec),

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

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc Build docs for various packages in the repos
%% @end
%%--------------------------------------------------------------------
handle_transitions([], _DocSpec) ->
    ok;
handle_transitions(TreeDiff, DocSpec) ->
    NewFiles = lists:map(fun(Path) -> 
				 {ok, {_, Rest}} = fs_lists:separate_by_token(Path, "/"),
				 Rest
			 end,
			 rd_file_tree:file_paths(TreeDiff)),
    ?INFO_MSG("Packages to generate docs for ~p~n",[NewFiles]),
    lists:foreach(fun(FilePath) ->
			  case regexp:match(FilePath, "(.*Meta.*|checksum|signature)") of
			      {match, _, _} -> 
				  ok;
			      _ -> 
				  case catch handle_transition(FilePath, DocSpec) of
				      ok    -> ok;
				      Error -> ?ERROR_MSG("documenation generation error for ~p: ~p~n", [FilePath, Error])
				  end
			  end
		  end, NewFiles).

handle_transition(PackageFileSuffix, DocSpec) ->
    case regexp:match(PackageFileSuffix, "/erts.") of
	{match, _, _} ->
	    % No docs for erts 
	    ok;
	_ ->
	    ?INFO_MSG("Transitioning ~p~n", [PackageFileSuffix]),
	    Elements    = ewr_repo_paths:decompose_suffix(PackageFileSuffix),
	    ErtsVsn     = fs_lists:get_val(erts_vsn, Elements),
	    Side        = fs_lists:get_val(side, Elements),
	    PackageName = fs_lists:get_val(package_name, Elements),

	    FromPackagePath   = ewl_file:join_paths(DocSpec#doc_spec.repo_dir_path, PackageFileSuffix),
	    TmpPackageDirPath = epkg_util:unpack_to_tmp(FromPackagePath),
	    
	    case Side of
		"lib" ->
		    build_app_docs(PackageName, TmpPackageDirPath, ErtsVsn, DocSpec);
		"releases" ->
		    build_release_docs(PackageName, TmpPackageDirPath, ErtsVsn, DocSpec)
	    end
    end.

build_app_docs(PackageName, TmpPackageDirPath, ErtsVsn, DocSpec) ->
    NoDocList = DocSpec#doc_spec.no_doc_list,
    case catch lists:member({app, list_to_atom(PackageName)}, NoDocList) of
	true  ->
	    ?INFO_MSG("~p is in the no doc list; skipping~n", [PackageName]),
	    ok;
	false ->
	    (catch dd_doc_builder:build_app_docs(TmpPackageDirPath, ErtsVsn, DocSpec))
    end.

build_release_docs(PackageName, TmpPackageDirPath, ErtsVsn, DocSpec) ->
    NoDocList = DocSpec#doc_spec.no_doc_list,
    case catch lists:member({release, list_to_atom(PackageName)}, NoDocList) of
	true  ->
	    ?INFO_MSG("~p is in the no doc list; skipping~n", [PackageName]),
	    ok;
	false ->
	    (catch dd_doc_builder:build_release_docs(TmpPackageDirPath, ErtsVsn, DocSpec))
    end.

%create_tree(RepoDirPath) ->
%    Fun = 
%	fun(FromDirPath) ->
%		try
%		    case regexp:match(FromDirPath, "erts\..*") of
%			{match, _, _} ->
%			    {ok, {Prefix, Suffix}} = chop_to_erts_vsn(FromDirPath),
%			    Elements = ewr_repo_paths:decompose_suffix(Suffix),
%			    ErtsVsn  = fs_lists:get_val(erts_vsn, Elements),
%			    Area     = fs_lists:get_val(area, Elements),
%			    [_|CheckSumFileSuffix] = ewr_repo_paths:erts_checksum_file_suffix(ErtsVsn, Area),
%			    CheckSumFilePath = filename:join(Prefix, CheckSumFileSuffix),
%			    filelib:is_file(CheckSumFilePath);
%			nomatch ->
%			    {ok, {Prefix, Suffix}} = chop_to_erts_vsn(FromDirPath),
%			    Elements           = ewr_repo_paths:decompose_suffix(Suffix),
%			    ErtsVsn            = fs_lists:get_val(erts_vsn, Elements),
%			    Side               = fs_lists:get_val(side, Elements),
%			    PackageName        = fs_lists:get_val(package_name, Elements),
%			    PackageVsn         = fs_lists:get_val(package_vsn, Elements),
%			    [_|CheckSumFileSuffix] =
%				ewr_repo_paths:checksum_file_suffix(ErtsVsn, Side, PackageName, PackageVsn),
%			    CheckSumFilePath = filename:join(Prefix, CheckSumFileSuffix),
%			    filelib:is_file(CheckSumFilePath)
%		    end
%		catch
%		    _C:_E ->
%			true
%		end
%	end,
%    rd_file_tree:create_tree(RepoDirPath, Fun).
%				      

%chop_to_erts_vsn(RepoDirPath) ->
%    Tokens = string:tokens(RepoDirPath, "/"),
%    chop_to_erts_vsn2(Tokens, "").
%
%chop_to_erts_vsn2([ErtsVsn|T], Front) ->
%    case regexp:match(ErtsVsn, "^[0-9]+\.[0-9]+\(\.[0-9]+\)?") of
%	{match, _, _} ->
%	    {ok, {Front, string:join([ErtsVsn|T], "/")}};
%	nomatch ->
%	    chop_to_erts_vsn2(T, Front ++ "/" ++ ErtsVsn)
%    end;
%chop_to_erts_vsn2([], _) ->
%    {error, bad_suffix}.
%
%%%====================================================================
%%% Test functions
%%%====================================================================
%chop_to_erts_vsn_test() ->
%    {ok, {Prefix, Suffix}} =
%	chop_to_erts_vsn("/Users/martinjlogan/repo/writable/5.6.3/Generic/lib/faxien/0.37.2.0/faxien.tar.gz"),
%    ?assertMatch("/Users/martinjlogan/repo/writable", Prefix),
%    ?assertMatch("5.6.3/Generic/lib/faxien/0.37.2.0/faxien.tar.gz", Suffix),
%    Elements           = ewr_repo_paths:decompose_suffix(Suffix),
%    ErtsVsn            = fs_lists:get_val(erts_vsn, Elements),
%    ?assertMatch("5.6.3", ErtsVsn),
%    Side               = fs_lists:get_val(side, Elements),
%    ?assertMatch("lib", Side),
%    PackageName        = fs_lists:get_val(package_name, Elements),
%    ?assertMatch("faxien", PackageName),
%    PackageVsn         = fs_lists:get_val(package_vsn, Elements),
%    ?assertMatch("0.37.2.0", PackageVsn),
%    [_|CheckSumFileSuffix] = ewr_repo_paths:checksum_file_suffix(ErtsVsn, Side, PackageName, PackageVsn),
%    ?assertMatch("5.6.3/Meta/faxien/0.37.2.0/checksum", CheckSumFileSuffix),
%    CheckSumFilePath   = filename:join([Prefix, CheckSumFileSuffix]),
%    ?assertMatch("/Users/martinjlogan/repo/writable/5.6.3/Meta/faxien/0.37.2.0/checksum", CheckSumFilePath).
%
