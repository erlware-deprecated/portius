%%%-------------------------------------------------------------------
%%% @author  : Martin Logan
%%%
%%% @doc 
%%% @copyright 2008 Martin Logan, Eric Merritt, Erlware
%%% @end
%%% Created : 27 Nov 2007 by Martin Logan <martinjlogan@sixfoe>
%%%-------------------------------------------------------------------
-module(dd_sup).

-behaviour(supervisor).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("doc_daemon.hrl").

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

    {ok, DocSpecs} = gas:get_env(doc_daemon, doc_specs),

    
    ChildSpecs = 
	lists:map(fun(DocSpecTuple) ->
			  TransitionId = element(1, DocSpecTuple),
			  DocSpec =
			      #doc_spec{transition_id           = TransitionId,
					repo_dir_path           = element(2, DocSpecTuple),
					webserver_doc_root      = element(3, DocSpecTuple),
					generated_docs_base_dir = element(4, DocSpecTuple),
					app_index_file_src      = element(5, DocSpecTuple),
					app_index_file          = element(6, DocSpecTuple),
					release_index_file_src  = element(7, DocSpecTuple),
					release_index_file      = element(8, DocSpecTuple),
				        no_doc_list             = fetch_no_doc_list(TransitionId)},
			  {TransitionId,
			   {dd_doc_server, start_link, [DocSpec]},
			   permanent,
			   1000,
			   worker,
			   [dd_doc_server]}
		  end,
		  DocSpecs),
    
    {ok,{SupFlags,ChildSpecs}}.


%%====================================================================
%% Internal functions
%%====================================================================
fetch_no_doc_list(TransitionId) ->
    case gas:get_env(doc_daemon, no_doc_list) of
	{ok, NoDocLists} ->
	    case lists:keysearch(TransitionId, 1, NoDocLists) of
		{value, {_TransitionID, NoDocList}} ->
		    NoDocList;
		_ ->
		    undefined
	    end;
	_ ->
	    undefined
    end.
