%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2008, Martin Logan
%%% @doc
%%%  functions for building documentation. 
%%% @end
%%% Created :  8 May 2008 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(dd_doc_builder).

%% API
-export([
	 build_index_docs/1
	]).

-include_lib("fslib/include/macros.hrl").
-include("doc_daemon.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc use a doc spec to build the index files for app and release documentation.
%% @spec build_index_docs(DocSpec) -> ok
%% @end
%%--------------------------------------------------------------------
build_index_docs(undefined) ->
    ?INFO_MSG("Doc transition spec undefined. Skipping doc building.~n", []),
    ok;
build_index_docs(DocSpec) ->
    AIR = (catch dd_app_template:create_app_index_page(DocSpec)),
    RIR = (catch dd_release_template:create_release_index_page(DocSpec)),
    ?INFO_MSG("Result of create app index page call ~p~n", [AIR]),
    ?INFO_MSG("Result of create release index page call ~p~n", [RIR]).


%%%===================================================================
%%% Internal functions
%%%===================================================================
