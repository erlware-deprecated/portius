%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@erlware.org>
%%% @copyright (C) 2008, Erlware
%%% @doc
%%%  Portius management functions go here. For example configuration modification functions.
%%% @end
%%%-------------------------------------------------------------------
-module(por_manage).

%% API
-export([
	 add_signature/5
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Add a signature spec to config.
%% Example Signature: {pub, release, faxien, 12123232, 435433}
%% @spec add_signature(TransitionId, Type, PackageName, Modulus, Exponent) -> ok
%% @end
%%--------------------------------------------------------------------
add_signature(TransitionId, Type, PackageName, Modulus, Exponent) -> 
    add_to_config_list(signatures, {TransitionId, Type, PackageName, Modulus, Exponent}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc Add an element to a config tuple whose value is a list.
%% @spec add_to_config_list(Key, ValueToAdd) -> ok | {error, Reason}
%% where
%%  Reason = no_such_config_entry
%% @end
%%--------------------------------------------------------------------
add_to_config_list(Key, ValueToAdd) ->
    ConfigFilePath = epkg_installed_paths:find_config_file_path(portius, epkg_util:get_current_release_version(portius)),
    gas:modify_config_value(ConfigFilePath, portius, Key, fun(Value) -> [ValueToAdd|Value] end).

