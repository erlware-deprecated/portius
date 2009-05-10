%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2008, Martin Logan
%%% @doc
%%%  Handles the transfer of packages from one repo location to another.
%%% @end
%%% Created :  1 Jan 2008 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(rd_trans_server).

-behaviour(gen_server).

%% API
-export([start_link/1, subscribe/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("fslib/include/macros.hrl").
-include("repo_daemon.hrl").
-include("eunit.hrl").


-define(SERVER, ?MODULE). 

-record(state, {transition_spec, inspection_frequency, last_tree, email, subscription_funs = []}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server.
%%
%% @spec start_link(TransitionSpec) -> 
%%       {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(TransitionSpec) ->
    gen_server:start_link({local, TransitionSpec#transition_spec.transition_id}, ?MODULE, [TransitionSpec], []).

%%--------------------------------------------------------------------
%% @doc
%% Subscribe to changes. When new files are moved over the subscription fun
%% will be called. The subscription fun takes a single argument which is
%% a list of the files that have been added.
%%
%% @spec subscribe(TransitionId, SubscriptionFun) -> void()
%% @end
%%--------------------------------------------------------------------
subscribe(TransitionId, SubscriptionFun) ->
    gen_server:cast(TransitionId, {subscribe, SubscriptionFun}).

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
init([TransitionSpec]) ->
    {ok, Timeout} = gas:get_env(repo_daemon, inspection_frequency),
    {ok, Email}   = gas:get_env(repo_daemon, email, undefined),

    ToRepoDirPath = TransitionSpec#transition_spec.to_repo,
    ok = ewl_file:mkdir_p(ToRepoDirPath),
    ToTree = rd_file_tree:create_empty_tree(ToRepoDirPath),
    ?INFO_MSG("initialized to move packages to ~p~n", [ToRepoDirPath]),

    State = #state{transition_spec      = TransitionSpec, 
		   inspection_frequency = Timeout, 
		   last_tree            = ToTree,
		   email                = Email},

    {ok, State, 1000}.

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
handle_cast({subscribe, SubscriptionFun}, State) ->
    ?INFO_MSG("adding the subscription ~p~n", [SubscriptionFun]),
    NewFuns = [SubscriptionFun|State#state.subscription_funs],
    {noreply, State#state{subscription_funs = NewFuns}, State#state.inspection_frequency}.

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
	   last_tree            = LastTree,
	   email                = Email,
	   subscription_funs    = SubscriptionFuns} = State,

    Tree     = create_tree(TransitionSpec#transition_spec.from_repo),
    TreeDiff = rd_file_tree:find_additions(LastTree, Tree),
    handle_transitions(TreeDiff, TransitionSpec, Email, SubscriptionFuns),

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
handle_transitions([], _TransitionSpec, _Email, _SubscriptionFuns) ->
    ok;
handle_transitions(TreeDiff, TransitionSpec, Email, SubscriptionFuns) ->
    NewFiles = lists:foldl(fun(Path, Acc) -> 
				   {ok, {_, FilePath}} = fs_lists:separate_by_token(Path, "/"),
				   case re:run(FilePath, "(.*Meta.*|checksum|signature)") of
				       {match, _} -> 
					   Acc;
				       _ -> 
					   [FilePath|Acc]
				   end
			   end,
			   [],
			   rd_file_tree:file_paths(TreeDiff)),

    ?INFO_MSG("Files to be transfered ~p~n",[NewFiles]),

    lists:foreach(fun(FilePath) ->
			  PackageName = filename:basename(FilePath),
			  handle_results((catch handle_transition(FilePath, TransitionSpec)), PackageName, Email)
		  end, NewFiles),

    lists:foreach(fun(SubscriptionFun) -> SubscriptionFun(NewFiles) end, SubscriptionFuns).
			  

handle_results(true, PackageName, Email) ->
    Subject = "Publish success for " ++ PackageName,
    send_email(Email, Subject, "Success");
handle_results(Error, PackageName, Email) ->
    Subject = "Publish failure for " ++ PackageName,
    Body    = lists:flatten(io_lib:fwrite("~p~n", [Error])),
    send_email(Email, Subject, Body),
    ?ERROR_MSG("handle transition returned error ~p~n", [Error]).

send_email(undefined, _Subject, _Body) ->
    ok;
send_email(To, Subject, Body) ->
    fs_email:send_async("no-reply-erlware@erlware.com", To, Subject, Body).
    
 				  
handle_transition(PackageFileSuffix, TransitionSpec) ->
    case re:run(PackageFileSuffix, "/erts.") of
	{match, _} ->
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
		    ok = transition_app(ErtsVsn, Area, Side, PackageName, PackageVsn, TransitionSpec),
		    AppSpec = #app_spec{name           = PackageName,
					version        = PackageVsn,
					transition_id  = TransitionSpec#transition_spec.transition_id,
					package_suffix = PackageFileSuffix},
		    rd_store:insert(AppSpec);
		"releases" ->
		    ok = transition_release(ErtsVsn, Area, Side, PackageName, PackageVsn, TransitionSpec),
		    RelSpec = #release_spec{name           = PackageName,
					    version        = PackageVsn,
					    transition_id  = TransitionSpec#transition_spec.transition_id,
					    package_suffix = PackageFileSuffix},
		    rd_store:insert(RelSpec)
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

    FromPackagePath   = ewl_file:join_paths(FromRepoDirPath, PackageFileSuffix),
    TmpPackageDirPath = epkg_util:unpack_to_tmp(FromPackagePath),

    try
	true =  epkg_validation:is_package_erts(TmpPackageDirPath),
	copy_over_erts(ErtsVsn, Area, FromRepoDirPath, ToRepoDirPath)
    catch
	_C:E ->
	    ?ERROR_MSG("~s failed validation or copy with ~p~n", [FromPackagePath, E]),
	    throw({failed_transition, FromPackagePath, E})
    after
	ewl_file:delete_dir(filename:dirname(TmpPackageDirPath))
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
    PackageFileSuffix = ewr_repo_paths:package_suffix(ErtsVsn, Area, Side, PackageName, PackageVsn),
    FromPackagePath   = ewl_file:join_paths(FromRepo, PackageFileSuffix),
    ToPackagePath = ewl_file:join_paths(ToRepo, PackageFileSuffix),
    case filelib:is_file(ToPackagePath) of
	true ->
	    ?INFO_MSG("app already in place ~p~n", [ToPackagePath]);
	false ->
	    TmpPackageDirPath = epkg_util:unpack_to_tmp(FromPackagePath),
	    
	    try
		true =  epkg_validation:is_package_an_app(TmpPackageDirPath),
		copy_over_app(ErtsVsn, Area, Side, PackageName, PackageVsn, FromRepo, ToRepo)
	    catch
		_C:E ->
		    ?ERROR_MSG("~s failed validation or copy with ~p~n", [FromPackagePath, E]),
		    throw({failed_transition, FromPackagePath, E})
	    after
		ewl_file:delete_dir(filename:dirname(TmpPackageDirPath))
	    end
    end.

copy_over_app(ErtsVsn, Area, "lib" = Side, PackageName, PackageVsn, FromRepo, ToRepo) ->
    PackageFileSuffix  = ewr_repo_paths:package_suffix(ErtsVsn, Area, Side, PackageName, PackageVsn),
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
    PackageFileSuffix = ewr_repo_paths:package_suffix(ErtsVsn, Area, Side, PackageName, PackageVsn),
    FromPackagePath   = ewl_file:join_paths(FromRepo, PackageFileSuffix),
    ToPackagePath = ewl_file:join_paths(ToRepo, PackageFileSuffix),
    case filelib:is_file(ToPackagePath) of
	true ->
	    ?INFO_MSG("release already in place ~p~n", [ToPackagePath]);
	false ->
	    TmpPackageDirPath = epkg_util:unpack_to_tmp(FromPackagePath),
	    
	    try
		true = epkg_validation:is_package_a_release(TmpPackageDirPath), 
		copy_over_release(ErtsVsn, Area, Side, PackageName, PackageVsn, FromRepo, ToRepo)
	    catch
		_C:E ->
		    ?ERROR_MSG("~s failed validation or copy with ~p~n", [FromPackagePath, E]),
		    throw({failed_transition, FromPackagePath, E})
	    after
		ewl_file:delete_dir(filename:dirname(TmpPackageDirPath))
	    end
    end.

copy_over_release(ErtsVsn, Area, "releases" = Side, PackageName, PackageVsn, FromRepo, ToRepo) ->
    PackageFileSuffix   = ewr_repo_paths:package_suffix(ErtsVsn, Area, Side, PackageName, PackageVsn),
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

create_tree(FromRepoDirPath) ->
    Fun = 
	fun(FromDirPath) ->
		try
		    case re:run(FromDirPath, "erts\..*") of
			{match, _} ->
			    {ok, {Prefix, Suffix}} = chop_to_erts_vsn(FromDirPath),
			    Elements = ewr_repo_paths:decompose_suffix(Suffix),
			    ErtsVsn  = fs_lists:get_val(erts_vsn, Elements),
			    Area     = fs_lists:get_val(area, Elements),
			    [_|CheckSumFileSuffix] = ewr_repo_paths:erts_checksum_file_suffix(ErtsVsn, Area),
			    CheckSumFilePath = filename:join(Prefix, CheckSumFileSuffix),
			    filelib:is_file(CheckSumFilePath);
			_NoMatch ->
			    {ok, {Prefix, Suffix}} = chop_to_erts_vsn(FromDirPath),
			    Elements           = ewr_repo_paths:decompose_suffix(Suffix),
			    ErtsVsn            = fs_lists:get_val(erts_vsn, Elements),
			    Side               = fs_lists:get_val(side, Elements),
			    PackageName        = fs_lists:get_val(package_name, Elements),
			    PackageVsn         = fs_lists:get_val(package_vsn, Elements),
			    [_|CheckSumFileSuffix] =
				ewr_repo_paths:checksum_file_suffix(ErtsVsn, Side, PackageName, PackageVsn),
			    CheckSumFilePath = filename:join(Prefix, CheckSumFileSuffix),
			    filelib:is_file(CheckSumFilePath)
		    end
		catch
		    _C:_E ->
			true
		end
	end,
    rd_file_tree:create_tree(FromRepoDirPath, Fun).
				      

chop_to_erts_vsn(FromRepoDirPath) ->
    Tokens = string:tokens(FromRepoDirPath, "/"),
    chop_to_erts_vsn2(Tokens, "").

chop_to_erts_vsn2([ErtsVsn|T], Front) ->
    case re:run(ErtsVsn, "^[0-9]+\.[0-9]+\(\.[0-9]+\)?") of
	{match, _} ->
	    {ok, {Front, string:join([ErtsVsn|T], "/")}};
	_NoMatch ->
	    chop_to_erts_vsn2(T, Front ++ "/" ++ ErtsVsn)
    end;
chop_to_erts_vsn2([], _) ->
    {error, bad_suffix}.

%%====================================================================
%% Test functions
%%====================================================================
chop_to_erts_vsn_test() ->
    {ok, {Prefix, Suffix}} =
	chop_to_erts_vsn("/Users/martinjlogan/repo/writable/5.6.3/Generic/lib/faxien/0.37.2.0/faxien.tar.gz"),
    ?assertMatch("/Users/martinjlogan/repo/writable", Prefix),
    ?assertMatch("5.6.3/Generic/lib/faxien/0.37.2.0/faxien.tar.gz", Suffix),
    Elements           = ewr_repo_paths:decompose_suffix(Suffix),
    ErtsVsn            = fs_lists:get_val(erts_vsn, Elements),
    ?assertMatch("5.6.3", ErtsVsn),
    Side               = fs_lists:get_val(side, Elements),
    ?assertMatch("lib", Side),
    PackageName        = fs_lists:get_val(package_name, Elements),
    ?assertMatch("faxien", PackageName),
    PackageVsn         = fs_lists:get_val(package_vsn, Elements),
    ?assertMatch("0.37.2.0", PackageVsn),
    [_|CheckSumFileSuffix] = ewr_repo_paths:checksum_file_suffix(ErtsVsn, Side, PackageName, PackageVsn),
    ?assertMatch("5.6.3/Meta/faxien/0.37.2.0/checksum", CheckSumFileSuffix),
    CheckSumFilePath   = filename:join([Prefix, CheckSumFileSuffix]),
    ?assertMatch("/Users/martinjlogan/repo/writable/5.6.3/Meta/faxien/0.37.2.0/checksum", CheckSumFilePath).
