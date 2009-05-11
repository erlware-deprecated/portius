%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@erlware.org>
%%% @copyright (C) 2009, Martin Logan
%%% @doc
%%%  holds assorted storage functions for the doc server. 
%%% @end
%%% Created : 11 Jan 2009 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(dd_store).

%% API
-export([
	 init/0,
	 insert/1,
	 delete/1,
	 lookup_app/2,
	 lookup_release/2
	]).

-define(TABLE_ID, ?MODULE).
-include("doc_daemon.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc This initializes the registrar.
%% @spec init() -> void()
%% @end
%%--------------------------------------------------------------------
init() ->
    ets:new(app_spec, [public, named_table, bag, {keypos, 2}]),
    ets:new(rel_spec, [public, named_table, bag, {keypos, 2}]),
    ok.
    
%%--------------------------------------------------------------------
%% @doc Find a an application given it's name.
%% @spec lookup_app(Name, Version) -> {ok, Record} | {error, not_found}
%% @end
%%--------------------------------------------------------------------
lookup_app(Name, Version) ->
    case ets:lookup(app_spec, Name) of
	[]      -> {error, not_found};
	Records -> {ok, hd([Record || Record <- Records, Record#app_spec.version == Version])}
    end.

%%--------------------------------------------------------------------
%% @doc Find a release given it's name.
%% @spec lookup_release(Name, Version) -> {ok, Record} | {error, not_found}
%% @end
%%--------------------------------------------------------------------
lookup_release(Name, Version) ->
    case ets:lookup(release_spec, Name) of
	[]      -> {error, not_found};
	Records -> {ok, hd([Record || Record <- Records, Record#release_spec.version == Version])}
    end.

%%--------------------------------------------------------------------
%% @doc Insert an app spec or a release spec record. 
%% @spec insert(Record) -> void()
%% @end
%%--------------------------------------------------------------------
insert(#app_spec{} = Record) ->
    ets:insert(app_spec, Record);
insert(#release_spec{} = Record) ->
    ets:insert(release_spec, Record).

%%--------------------------------------------------------------------
%% @doc delete a record
%% @spec delete(Record) -> void()
%% @end
%%--------------------------------------------------------------------
delete(#app_spec{} = Record) ->
    ets:delete(app_spec, Record);
delete(#release_spec{} = Record) ->
    ets:delete(release_spec, Record).
