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
-include("repo_daemon.hrl").

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
    {ok, DocumentRoot} = gas:get_env(web_interface, document_root, "/var/www"),
    DefaultTransitions = create_default_transitions(DocumentRoot),
    {ok, Transitions}  = gas:get_env(repo_daemon, transitions, DefaultTransitions),
    TransitionSpecs    = make_transition_spec_records(Transitions),
    supervisor:start_link({local, ?SERVER}, ?MODULE, [TransitionSpecs]).

%%====================================================================
%% Server functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%--------------------------------------------------------------------
init([TransitionSpecs]) ->
    RestartStrategy    = one_for_one,
    MaxRestarts        = 1000,
    MaxTimeBetRestarts = 3600,
    
    SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetRestarts},

    
    ChildSpecs = lists:map(fun(TransitionSpec) -> 
				   {TransitionSpec#transition_spec.transition_id,
				    {rd_trans_server, start_link, [TransitionSpec]},
				    permanent,
				    1000,
				    worker,
				    [rd_trans_server]}
			   end,
			   TransitionSpecs),
    
    {ok,{SupFlags,ChildSpecs}}.


%%====================================================================
%% Internal functions
%%====================================================================
make_transition_spec_records(Transitions) ->
    lists:map(fun({TransitionId, FromRepoDirPath, ToRepoDirPath, SignType}) ->
		      #transition_spec{transition_id = TransitionId,
				       from_repo = FromRepoDirPath, 
				       to_repo   = ToRepoDirPath,
				       sign_type = SignType}
	      end, Transitions).

create_default_transitions(DocumentRoot) ->
    [
     {pub,
      filename:join(DocumentRoot, "writable"),
      filename:join(DocumentRoot, "pub"),
      none}
    ].
    
