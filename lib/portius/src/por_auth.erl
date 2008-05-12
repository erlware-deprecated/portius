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

-include("portius.hrl").
-include("macros.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc fetch the contents of the digital signature file.
%% @spec validate_signature(Type, PackageFileSuffix, TransitionSpec) -> {ok, {Modulus, Exponent, Signature}} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
validate_signature(_Type, _PackageFileSuffix, #transition_spec{sign_type = unsigned}) ->
    ok;
validate_signature(Type, PackageFileSuffix, TransitionSpec) ->
    case fetch_and_manage_signatures(Type, PackageFileSuffix, TransitionSpec) of
	{ok, {Signature, Modulus, Exponent}} ->
	    Elements    = ewr_repo_paths:decompose_suffix(PackageFileSuffix),
	    PackageName = fs_lists:get_val(package_name, Elements),
	    PackageVsn  = fs_lists:get_val(package_vsn, Elements),
	    Sum = lists:foldl(fun(N, Acc) -> Acc + N end, 0, PackageVsn),
	    case cg_rsa:decrypt(Signature, Modulus, Exponent) of
		Sum ->
		    ?INFO_MSG("signature for ~s-~s is valid~n", [PackageName, PackageVsn]),
		    ok;
		_NoMatch ->
		    ?ERROR_MSG("signature for ~s-~s is invalid~n", [PackageName, PackageVsn]),
		    {error, bad_signature}
	    end;
	Error ->
	    Error
    end.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
fetch_and_manage_signatures(Type, PackageFileSuffix, TransitionSpec) ->
    {ok, {Signature, Modulus, Exponent}} = get_signature_from_file(Type, PackageFileSuffix, TransitionSpec),
    case fetch_stored_mod_exp(Type, PackageFileSuffix, TransitionSpec) of
	{ok, {Modulus, Exponent}} ->
	    {ok, {Signature, Modulus, Exponent}};
	{ok, {OtherModulus, OtherExponent}} ->
	    ?INFO_MSG("The modulus and exponent, ~p ~p, do not match those cached for ~p~n", [OtherModulus, OtherExponent]),
	    {error, incorrect_keys};
	{error, _Reason} ->
	    ?INFO_MSG("writing out a signature for package ~p~n", [PackageFileSuffix]),
	    Elements    = ewr_repo_paths:decompose_suffix(PackageFileSuffix),
	    PackageName = fs_lists:get_val(package_name, Elements),
	    Elements = ewr_repo_paths:decompose_suffix(PackageFileSuffix),
	    Area     = fs_lists:get_val(area, Elements),
	    #transition_spec{transition_id = TransitionId} = TransitionSpec,
	    por_manage:add_signature(TransitionId, Type, Area, PackageName, Modulus, Exponent),
	    {ok, {Signature, Modulus, Exponent}}
    end.
    
%%--------------------------------------------------------------------
%% @doc fetch the contents of the digital signature file.
%% @spec get_signature_from_file(Type, PackageFileSuffix, TransitionSpec) ->
%%        {ok, {Signature, Modulus, Exponent}} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
get_signature_from_file(Type, PackageFileSuffix, TransitionSpec) ->
    SigFilePath = signature_file_path(Type, PackageFileSuffix, TransitionSpec),
    case file:consult(SigFilePath) of
	{ok, [{signature, Signature, Modulus, Exponent}]} ->
	    {ok, {Signature, Modulus, Exponent}};
	Error ->
	    Error
    end.

signature_file_path(Type, PackageFileSuffix, TransitionSpec) ->
    #transition_spec{from_repo  = FromRepoDirPath} = TransitionSpec,
    Elements = ewr_repo_paths:decompose_suffix(PackageFileSuffix),
    ErtsVsn  = fs_lists:get_val(erts_vsn, Elements),
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
    Elements    = ewr_repo_paths:decompose_suffix(PackageFileSuffix),
    Area        = fs_lists:get_val(area, Elements),
    PackageName = fs_lists:get_val(package_name, Elements),
    case fs_lists:get_val(signatures, Children) of
	undefined ->
	    {error, bad_config_with_no_signatures_tuple};
	Signatures ->
	    Signature = lists:filter(
			  fun({TransitionId_, Type_, Area_, PackageName_, _, _} = Sig_) ->
				  ?INFO_MSG("looking at signature ~p~n", [Sig_]),
				  TransitionId == TransitionId_ andalso Type == Type_ andalso PackageName == PackageName_
				  andalso Area == Area_
			  end,
			  Signatures),
	    case Signature of
		[] ->
		    ?INFO_MSG("no stored signature found for ~p~n", [{TransitionId, Type, Area, PackageName}]),
		    {error, signature_not_found};
		[{TransitionId, Type, Area, PackageName, Modulus, Exponent}] ->
		    {ok, {Modulus, Exponent}}
	    end
    end.

