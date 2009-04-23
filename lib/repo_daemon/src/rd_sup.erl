%%%-------------------------------------------------------------------
%%% @author  : Martin Logan
%%%
%%% @doc 
%%% @copyright 2008 Martin Logan, Eric Merritt, Erlware
%%% @end
%%% Created : 27 Nov 2007 by Martin Logan <martinjlogan@sixfoe>
%%%-------------------------------------------------------------------
-module(rd_sup).

-behaviour(supervisor).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start_link/0
        ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
	 init/1
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @spec start_link() -> {ok, pid()} | Error
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Server functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy    = one_for_one,
    MaxRestarts        = 1000,
    MaxTimeBetRestarts = 3600,
    
    SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetRestarts},

    {ok, ToFromList} = gas:get_env(repo_daemon, transitions),
    
    ChildSpecs = 
	lists:map(fun({TransitionId, FromRepoDirPath, ToRepoDirPath, SignType}) ->
			  Args = [TransitionId, FromRepoDirPath, ToRepoDirPath, SignType],
			  {TransitionId,
			   {rd_trans_server, start_link, Args},
			   permanent,
			   1000,
			   worker,
			   [rd_trans_server]}
		  end,
		  ToFromList),
    
    {ok,{SupFlags,ChildSpecs}}.


%%====================================================================
%% Internal functions
%%====================================================================
