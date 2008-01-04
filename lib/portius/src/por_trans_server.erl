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

-record(state, {from_repo, to_repo, inspection_frequency}).

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
    {ok, #state{from_repo = FromRepoDirPath, to_repo = ToRepoDirPath, inspection_frequency = Timeout}, Timeout}.

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
handle_info(_Info, #state{from_repo = FR, to_repo = TR, inspection_frequency = CF} = State) ->
    find_files(FR),
    {noreply, State, CF}.

find_files(FromRepo) ->
    Files = ewl_file:find(FromRepo, "\.tar\.gz"),
    ?INFO_MSG("Files found in ~s are :~p~n", [FromRepo, Files]),
    Files.

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
%%% Internal functions
%%====================================================================
%%-------------------------------------------------------------------
%% @doc
%%  Create a filesystem tree of the repo
%%
%% @spec create_tree(Root) -> list()
%% @end
%%-------------------------------------------------------------------
create_tree([]) ->
    [];
create_tree(FromDir) ->
    case filelib:is_dir(FromDir) of
	false ->
	    {file, FromDir};
	true ->
	    {dir, FromDir, lists:foldl(fun(CheckFromDir, Acc) when CheckFromDir == FromDir -> 
					       Acc;
					  (ChildFromDir, Acc) -> 
					       case create_tree(ChildFromDir) of
						   []  -> Acc;
						   Res -> [Res|Acc]
					       end
				       end, [], filelib:wildcard(FromDir ++ "/*"))}
    end.
