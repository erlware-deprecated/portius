%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2009, Martin Logan
%%% @doc
%%% Possible configuration values for this application.
%%%
%%% Transitions define a move from one repo to another.
%%% They are of the form
%%% &lt;transition-id&gt; &lt;from-repo&gt;, &lt;to-repo&gt;, &lt;signed or unsigned&gt;
%%% <pre>
%%% {transitions,  
%%%    [
%%%     {pub, "/var/repo/writable", "/tmp/repo/pub", unsigned} 
%%%    ]
%%%   }
%%%
%%%   {inspection_frequency, 5000},
%%%   {email, "martinjlogan@erlware.org"}
%%% </pre>
%%% @end
%%% Created : 21 Apr 2009 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(rd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).



%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    rd_store:init(),
    case rd_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
