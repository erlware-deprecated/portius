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
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("macros.hrl").

-define(SERVER, ?MODULE). 

-record(state, {from_repo, to_repo, inspection_frequency, last_tree = {}}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(FromRepoDirPath, ToRepoDirPath) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(FromRepoDirPath, ToRepoDirPath) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [FromRepoDirPath, ToRepoDirPath], []).

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
init([FromRepoDirPath, ToRepoDirPath]) ->
    {ok, Timeout} = gas:get_env(portius, inspection_frequency),
    ToTree        = por_file_tree:create_tree(ToRepoDirPath),
    {ok, #state{from_repo = FromRepoDirPath, to_repo = ToRepoDirPath, inspection_frequency = Timeout, last_tree = ToTree}, 
     Timeout}.

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
handle_info(_Info, #state{from_repo = FR, to_repo = TR, inspection_frequency = Timeout, last_tree = LastTree} = State) ->
    Tree     = por_file_tree:create_tree(FR),
    TreeDiff = por_file_tree:find_additions(LastTree, Tree),
    ?INFO_MSG("tree diff ~p~ngoing to ~s~n", [TreeDiff, TR]),
    handle_transitions(TreeDiff, FR, TR),
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
handle_transitions([], _FromRepo, _ToRepo) ->
    ok;
handle_transitions(TreeDiff, FromRepo, ToRepo) ->
    NewFiles = lists:map(fun(Path) -> 
				 {ok, {_, Rest}} = fs_lists:separate_by_token(Path, "/"),
				 Rest
			 end, por_file_tree:file_paths(TreeDiff)),
    lists:foreach(fun(FilePath) ->
			  case regexp:match(FilePath, ".*Meta.*") of
			      {match, _, _} -> ok;
			      _             -> handle_transition(FilePath, FromRepo, ToRepo)
			  end
		  end, NewFiles).
 				  
%%--------------------------------------------------------------------
%% @private
%% @doc handle the transition of a package from one repo to another
%% @end
%%--------------------------------------------------------------------
handle_transition(PackageFileSuffix, FromRepo, ToRepo) ->
    PackageFilePath = ewl_file:join_paths(FromRepo, PackageFileSuffix), 
    ?INFO_MSG("Transitioning ~s from ~s to ~s~n", [PackageFilePath, FromRepo, ToRepo]),

    Elements    = ewr_repo_paths:decompose_suffix(PackageFileSuffix),
    ErtsVsn     = fs_lists:get_val(erts_vsn, Elements),
    Side        = fs_lists:get_val(side, Elements),
    Area        = fs_lists:get_val(area, Elements),
    PackageName = fs_lists:get_val(package_name, Elements),
    PackageVsn  = fs_lists:get_val(package_vsn, Elements),
    transition(ErtsVsn, Area, Side, PackageName, PackageVsn, FromRepo, ToRepo).

transition(ErtsVsn, Area, "lib" = Side, PackageName, PackageVsn, FromRepo, ToRepo) ->
    %TmpPackagePath     = epkg_util:unpack_to_tmp(PackageFilePath),
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
    ewl_file:copy_dir(FromDotAppFilePath, ToDotAppFilePath);
transition(ErtsVsn, Area, "releases" = Side, PackageName, PackageVsn, FromRepo, ToRepo) ->
    PackageSuffix      = ewr_repo_paths:package_suffix(ErtsVsn, Area, Side, PackageName, PackageVsn),
    FromPackagePath    = ewl_file:join_paths(FromRepo, PackageSuffix),
    ToPackagePath      = ewl_file:join_paths(ToRepo, PackageSuffix),

    ?INFO_MSG("ewl_file:copy_dir(~p, ~p)", [FromPackagePath, ToPackagePath]),

    ewl_file:mkdir_p(filename:dirname(ToPackagePath)),
    ewl_file:copy_dir(FromPackagePath, ToPackagePath).
    
