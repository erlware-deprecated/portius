%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2008, Martin Logan
%%% @doc
%%%   contains functions pertaining to the validation and authentication
%%    of packages.
%%% @end
%%% Created : 24 Apr 2008 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(por_auth).

%% API
-export([
	 validate_signature/3
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc fetch the contents of the digital signature file.
%% @spec get_signature(Type, PackageFileSuffix, TransitionSpec) -> {ok, {Modulus, Exponent, Signature}} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
validate_signature(Type, PackageFileSuffix, TransitionSpec) ->
    case fetch_and_manage_signatures(Type, PackageFileSuffix, TransitionSpec) of
	{ok, {Modulus, Exponent, Signature}} ->
	    {ok, {PackageName, PackageVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(PackageFileSuffix),
	    Sum = lists:foldl(fun(N, Acc) -> Acc + N end, 0, PackageName),
	    case cg_rsa:decrypt(Signature, Modulus, Exponent) of
		Sum ->
		    ok;
		_NoMatch ->
		    ?ERROR_MSG("bad signature for ~s-~s~n", [PackageName, PackageVsn]),
		    {error, bad_signature}
	    end;
	Error ->
	    Error
    end.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
fetch_and_manage_signatures(Type, PackageFileSuffix, TransitionSpec) ->
    {ok, {Modulus, Exponent, Signature}} = get_signature_from_file(Type, PackageFileSuffix, TransitionSpec),
    case fetch_stored_mod_exp(Type, PackageFileSuffix, TransitionSpec) of
	{ok, {Modulus, Exponent}} ->
	    {ok, {Modulus, Exponent, Signature}};
	{ok, {OtherModulus, OtherExponent}} ->
	    ?INFO_MSG("The modulus and exponent, ~p ~p, do not match those cached for ~p~n", [OtherModulus, OtherExponent]),
	    {error, incorrect_keys};
	{error, bad_config_with_no_signatures_tuple} = Error ->
	    ?INFO_MSG("bad config containing no signatures tuple~n", []),
	    Error;
	{error, signature_not_found} ->
	    {ok, {PackageName, _PackageVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(PackageFileSuffix),
	    #transition_spec{transition_id = TransitionId} = TransitionSpec,
	    por_manage:add_signature(TransitionId, Type, PackageName, Modulus, Exponent),
	    {ok, {Modulus, Exponent, Signature}}
    end.
    
%%--------------------------------------------------------------------
%% @doc fetch the contents of the digital signature file.
%% @spec get_signature(Type, PackageFileSuffix, TransitionSpec) -> {ok, {Modulus, Exponent, Signature}} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
get_signature_from_file(Type, PackageFileSuffix, TransitionSpec) ->
    #transition_spec{from_repo  = FromRepoDirPath} = TransitionSpec,

    Elements = ewr_repo_paths:decompose_suffix(PackageFileSuffix),
    ErtsVsn  = fs_lists:get_val(erts_vsn, Elements),
    SigFilePath = signature_file_path(Type, PackageFileSuffix, TransitionSpec),
    case file:consult(SigFilePath) of
	{ok, [{signature, {Modulus, Exponent, Message}}]} ->
	    {ok, {Modulus, Exponent, Message}};
	Error ->
	    Error
    end.

signature_file_path(Type, PackageFileSuffix, TransitionSpec) ->
    case Type of
	erts -> 
	    Area        = fs_lists:get_val(area, Elements),
	    SigSuffix   = ewr_repo_paths:erts_signature_file_suffix(ErtsVsn, Area),
	    ewl_file:join_paths(FromRepoDirPath, SigSuffix);
	Type ->
	    Side        = fs_lists:get_val(side, Elements),
	    PackageName = fs_lists:get_val(package_name, Elements),
	    PackageVsn  = fs_lists:get_val(package_vsn, Elements),
	    SigSuffix   = ewr_repo_paths:signature_file_suffix(ErtsVsn, Side, PackageName, PackageVsn),
	    ewl_file:join_paths(FromRepoDirPath, SigSuffix)
    end.

%%--------------------------------------------------------------------
%% @doc fetch the modulus and exponent that may or may not be stored in config.
%% @spec (Type, PackageFileSuffix, TransitionSpec) -> {ok, {Modulus, Exponent}} | {error, Reason}
%% where
%%  Reason = bad_config_with_no_signatures_tuple | signature_not_found
%% @end
%%--------------------------------------------------------------------
fetch_stored_mod_exp(Type, PackageFileSuffix, TransitionSpec) ->
    #transition_spec{transition_id = TransitionId, children = Children} = TransitionSpec,
    {ok, {PackageName, _PackageVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(PackageFileSuffix),
    case fs_lists:get_val(signatures, Children) of
	undefined ->
	    {error, bad_config_with_no_signatures_tuple};
	Signatures ->
	    Signature = lists:filter(
			  fun({TransitionId_, Type_, PackageName_, _, _}) ->
				  TransitionId == TransitionId_ andalso Type == Type_ andalso PackageName == PackageName_
			  end,
			  Signatures),
	    case Signature of
		[] ->
		    {error, signature_not_found}
		[{TransitionId, Type, PackageName, Modulus, Exponent}] ->
		    {ok, {Modulus, Exponent}}
	    end
    end.

