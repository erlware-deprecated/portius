%%%-------------------------------------------------------------------
%%% @author  : Martin Logan
%%%
%%% @doc the top level supervisor for RSS to Email
%%% @copyright 2008 Martin Logan, Eric Merritt, Erlware
%%% @end
%%% Created : 27 Nov 2007 by Martin Logan <martinjlogan@sixfoe>
%%%-------------------------------------------------------------------
-module(por_sup).

-behaviour(supervisor).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start_link/1
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
%% @spec start_link(StartArgs) -> {ok, pid()} | Error
%% @end
%%--------------------------------------------------------------------
start_link(_StartArgs) ->
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
    
    {ok, ToFromList} = gas:get_env(portius, transitions),
    
    {ChildSpecs, _} = 
	lists:foldl(fun({FromRepoDirPath, ToRepoDirPath, DocDirPath}, {Specs, Count}) ->
			    {[create_trans_server_child_spec(list_to_atom("s" ++ integer_to_list(Count)), 
							     FromRepoDirPath, ToRepoDirPath, DocDirPath)|Specs], Count + 1}
		    end,
		    {[], 1}, ToFromList),
    
    {ok,{SupFlags,ChildSpecs}}.


%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Create a trans server child spec.
%% @spec create_trans_server_child_spec(Key, FromRepoDirPath, ToRepoDirPath, DocDirPath) -> ChildSpec
%% where
%%  Key = atom()
%%  FromRepoDirPath = string()
%%  ToRepoDirPath = string()
%%  DocDirPath = string()
%% @end
%%--------------------------------------------------------------------
create_trans_server_child_spec(Key, FromRepoDirPath, ToRepoDirPath, DocDirPath) ->
    {Key,
     {por_trans_server, start_link, [FromRepoDirPath, ToRepoDirPath, DocDirPath]},
     permanent,
     1000,
     worker,
     [por_feed_server]}.
