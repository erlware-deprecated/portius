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
	 lookup_app/3,
	 lookup_apps/2,
	 lookup_apps/1,
	 lookup_release/3,
	 lookup_releases/2,
	 lookup_releases/1
	]).

-define(TABLE_ID, ?MODULE).
-include("repo_daemon.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

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
%% @spec lookup_app(Name, Version, TransitionId) -> {ok, Record} | {error, not_found}
%% @end
%%--------------------------------------------------------------------
lookup_app(Name, Version, TransitionId) ->
    Name2 = epkg_util:if_atom_or_integer_to_string(Name),
    Records = select_apps(ets:fun2ms(fun(Record) when Record#app_spec.name == Name2,
			     Record#app_spec.version == Version,
			     Record#app_spec.transition_id == TransitionId -> Record end)),
    case Records of
	[Record] -> {ok, Record};
	[]       -> {error, not_found}
    end.
	    

%%--------------------------------------------------------------------
%% @doc Find all applications of a particular name and transition id.
%% @spec lookup_apps(Name, TransitionId) -> {ok, Record} | {error, not_found}
%% @end
%%--------------------------------------------------------------------
lookup_apps(Name, TransitionId) ->
    Name2 = epkg_util:if_atom_or_integer_to_string(Name),
    select_releases(ets:fun2ms(fun(Record) when Record#app_spec.name == Name2,
		       Record#app_spec.transition_id == TransitionId -> Record end)).
%%--------------------------------------------------------------------
%% @doc Find all applications of a particular name.
%% @spec lookup_apps(Name) -> {ok, Record} | {error, not_found}
%% @end
%%--------------------------------------------------------------------
lookup_apps(Name) ->
    Name2 = epkg_util:if_atom_or_integer_to_string(Name),
    ets:lookup(rd_app_spec, Name2).   

%%--------------------------------------------------------------------
%% @doc Select from the apps table using a match spec. 
%% @spec select_apps(Fun) -> {ok, Record} | {error, not_found}
%% @end
%%--------------------------------------------------------------------
select_apps(MatchSpec) ->
    ets:select(rd_app_spec, MatchSpec).

%%--------------------------------------------------------------------
%% @doc Find a specific release
%% @spec lookup_release(Name, Version, TransitionId) -> {ok, Record} | {error, not_found}
%% @end
%%--------------------------------------------------------------------
lookup_release(Name, Version, TransitionId) ->
    Name2 = epkg_util:if_atom_or_integer_to_string(Name),
    Records = select_releases(ets:fun2ms(fun(Record) when Record#release_spec.name == Name2,
				 Record#release_spec.version == Version,
				 Record#release_spec.transition_id == TransitionId -> Record end)),
    case Records of
	[Record] -> {ok, Record};
	[]       -> {error, not_found}
    end.

%%--------------------------------------------------------------------
%% @doc Find all releases of a particular name and transition id.
%% @spec lookup_releases(Name, TransitionId) -> {ok, Record} | {error, not_found}
%% @end
%%--------------------------------------------------------------------
lookup_releases(Name, TransitionId) ->
    Name2 = epkg_util:if_atom_or_integer_to_string(Name),
    select_releases(ets:fun2ms(fun(Record) when Record#release_spec.name == Name2,
		       Record#release_spec.transition_id == TransitionId -> Record end)).
    
%%--------------------------------------------------------------------
%% @doc Find all releases of a particular name.
%% @spec lookup_releases(Name) -> {ok, Record} | {error, not_found}
%% @end
%%--------------------------------------------------------------------
lookup_releases(Name) ->
    Name2 = epkg_util:if_atom_or_integer_to_string(Name),
    ets:lookup(rd_release_spec, Name2).

%%--------------------------------------------------------------------
%% @doc Select from the releases table using a match spec. 
%% @spec select_releases(MatchSpec) -> {ok, Record} | {error, not_found}
%% @end
%%--------------------------------------------------------------------
select_releases(MatchSpec) ->
    ets:select(rd_release_spec, MatchSpec).

%%--------------------------------------------------------------------
%% @doc Insert a package spec.
%% @spec (AppSpecRecord) -> void()
%% @end
%%--------------------------------------------------------------------
insert(#app_spec{name = Name, version = Version, transition_id = TransitionId} = Record) ->
    Name2 = epkg_util:if_atom_or_integer_to_string(Name),
    case lookup_app(Name2, Version, TransitionId) of
	{error, not_found} ->
	    ets:insert(rd_app_spec, Record);
	_ ->
	    throw({duplicate_app_not_inserting, Record})
    end;
insert(#release_spec{name = Name, version = Version, transition_id = TransitionId} = Record) ->
    Name2 = epkg_util:if_atom_or_integer_to_string(Name),
    case lookup_release(Name2, Version, TransitionId) of
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
