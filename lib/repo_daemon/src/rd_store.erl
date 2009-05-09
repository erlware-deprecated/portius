%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@erlware.org>
%%% @copyright (C) 2009, Martin Logan
%%% @doc
%%%  holds assorted storage functions for the repo server. 
%%% @end
%%% Created : 11 Jan 2009 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(rd_store).

%% API
-export([
	 init/0,
	 insert/1,
	 delete/1,
	 lookup_app/2,
	 lookup_release/2,
	 lookup_apps/1,
	 lookup_releases/1
	]).

-define(TABLE_ID, ?MODULE).
-include("repo_daemon.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc This initializes the registrar.
%% @spec init() -> void()
%% @end
%%--------------------------------------------------------------------
init() ->
    ets:new(rd_app_spec, [public, named_table, bag, {keypos, 2}]),
    ets:new(rd_release_spec, [public, named_table, bag, {keypos, 2}]),
    ok.
    
%%--------------------------------------------------------------------
%% @doc Find a an application given it's name and version.
%% @spec lookup_app(Name) -> {ok, Record} | {error, not_found}
%% @end
%%--------------------------------------------------------------------
lookup_app(Name, Version) ->
    case lookup_apps(Name) of
	[]      -> {error, not_found};
	Records ->
	    case [Record || Record <- Records, Record#app_spec.version == Version] of
		[]       -> {error, not_found};
		[Record] -> {ok, Record}
	    end
    end.

%%--------------------------------------------------------------------
%% @doc Find all applications of a particular name.
%% @spec lookup_apps(Name) -> {ok, Record} | {error, not_found}
%% @end
%%--------------------------------------------------------------------
lookup_apps(Name) ->
    ets:lookup(rd_app_spec, Name).   

%%--------------------------------------------------------------------
%% @doc Find a release given it's name.
%% @spec lookup_release(Name) -> {ok, Record} | {error, not_found}
%% @end
%%--------------------------------------------------------------------
lookup_release(Name, Version) ->
    case lookup_releases(Name) of
	[] ->
	    {error, not_found};
	Records -> 
	    case [Record || Record <- Records, Record#release_spec.version == Version] of
		[]       -> {error, not_found};
		[Record] -> {ok, Record}
	    end
    end.

%%--------------------------------------------------------------------
%% @doc Find all releases of a particular name.
%% @spec lookup_app(Name) -> {ok, Record} | {error, not_found}
%% @end
%%--------------------------------------------------------------------
lookup_releases(Name) ->
    ets:lookup(rd_release_spec, Name).

%%--------------------------------------------------------------------
%% @doc Insert a key and pid.
%% @spec insert(Key, Pid) -> void()
%% @end
%%--------------------------------------------------------------------
insert(#app_spec{name = Name, version = Version} = Record) ->
    case lookup_app(Name, Version) of
	{error, not_found} ->
	    ets:insert(rd_app_spec, Record);
	_ ->
	    throw({duplicate_app_not_inserting, Record})
    end;
insert(#release_spec{name = Name, version = Version} = Record) ->
    case lookup_release(Name, Version) of
	{error, not_found} ->
	    ets:insert(rd_release_spec, Record);
	_ ->
	    throw({duplicate_release_not_inserting, Record})
    end.

%%--------------------------------------------------------------------
%% @doc delete a record
%% @spec delete(Record) -> void()
%% @end
%%--------------------------------------------------------------------
delete(#app_spec{} = Record) ->
    ets:delete(rd_app_spec, Record);
delete(#release_spec{} = Record) ->
    ets:delete(rd_release_spec, Record).
