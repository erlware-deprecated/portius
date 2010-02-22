%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2009, Martin Logan
%%% @doc
%%%
%%% Configuration
%%%
%%% Doc specs are optional.  They define how to create web ready
%%% documentation for your repo server.
%%%
%%% &lt;transition-id&gt;, # This is the id that will tie this config back to a
%%%                  # transition.
%%% &lt;repo-directory-path&gt;, 
%%% &lt;webserver-doc-root&gt;, 
%%% &lt;generated-docs-base-dir&gt;, # The root dir that will contain all generated
%%%                              docs 
%%% &lt;app-index-file-src&gt;, # The template file that will be used to generate
%%%                       # the &lt;app-index-file&gt;
%%%                       # This may be the same as the &lt;app-index-file&gt; in
%%%                       # which case the original will be copied out to a 
%%%                       # file of the same name with the suffix .src for
%%%                       # safe keeping prior to rendering the final page.
%%% &lt;app-index-file&gt;, # The page that will index all app docs.  Must be
%%%                   # somewhere under the &lt;web-server-doc-root&gt;
%%% &lt;release-index-file-src&gt; # The template file that will be used to
%%%                          # generate the &lt;release-index-file&gt;
%%%                          # This may be the same as the &lt;app-index-file&gt;
%%%                          # in which case the original will be copied out
%%%                          # to a file of the same name with the suffix
%%%                          # .src for safe keeping prior to rendering the
%%%                          # final page
%%% &lt;release-index-file&gt; # The page that will index all release docs.  Must
%%%                      # be somewhere under the &lt;web-server-doc-root&gt;
%%% <pre>
%%% {doc_specs, 
%%%  [
%%%   {pub, 
%%%    "/tmp/repo/pub",
%%%    "/tmp/repo",
%%%    "/tmp/repo/repo-doc", 
%%%    "/tmp/repo/repo-doc/lib_index.html.src", 
%%%    "/tmp/repo/repo-doc/lib_index.html", 
%%%    "/tmp/repo/repo-doc/releases_index.html.src",
%%%    "/tmp/repo/repo-doc/releases_index.html"}
%%%  ]
%%% },
%%%
%%% No doc lists instruct portius not to render the docs for a particular
%%% package. The lists are of the form:
%%%
%%% {&lt;transition-id&gt;, [{&lt;release|app&gt;, &lt;package-name&gt;}]}
%%%
%%% Examples:
%%%
%%%   {no_doc_list,
%%%    [
%%%     {pub,
%%%      [
%%%       {app, appmon}
%%%      ]
%%%     }]}
%%% </pre>
%%% @end
%%% Created : 21 Apr 2009 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(dd_app).

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
    dd_store:init(),
    case dd_sup:start_link() of
	{ok, Pid} -> {ok, Pid};
	Error     -> Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.
