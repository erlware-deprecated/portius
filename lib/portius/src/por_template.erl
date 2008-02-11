%%%-------------------------------------------------------------------
%%% @author Samuel Rivas <samuelrivas@gmail.com>
%%% @copyright (C) 2008, Samuel Rivas
%%% @doc This module compiles templates with the information about
%%% applications comming form other portius modules
%%%
%%% @type app_info() = {AppName, [AppVersionInfo]}
%%%                    AppName = atom()
%%%                    AppVersionInfo = {Version, [ErtsVersion]}
%%%                    Version = string()
%%%                    ErtsVresion = string()
%%% @end
%%% Created :  28 Jan 2008 by Samuel Rivas <samuelrivas@gmail.com>
%%%-------------------------------------------------------------------
-module(por_template).

%% API
-export([
	 create_app_index_page/2
	]).

-include("eunit.hrl").

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Create app index file.
%% @spec create_app_index_page(AppIndexTemplateFilePath, DocRootDirPath) -> ok
%% where
%%  AppIndexTemplateFilePath = string()
%%  DocRootDirPath = string()
%% @end
%%--------------------------------------------------------------------
create_app_index_page(AppIndexTemplateFilePath, DocRootDirPath) -> 
    {ok, CompiledAppIndexTemplateFile} = sgte:compile_file(AppIndexTemplateFilePath),
    {ok, Map}                          = sgte:compile("$map li apps$"),
    AppSpecs = gather_app_specs(DocRootDirPath),
    NewPage  = sgte:render_string(Map, [{apps, CompiledAppIndexTemplateFile}, AppSpecs]),
    {ok, IOD} = file:open(AppIndexTemplateFilePath, [write]),
    ok = io:fwrite(IOD, "~s", [NewPage]).

gather_app_specs(DocRootDirPath) ->
    dict:to_list(lists:foldl(fun(ErtsDirPath, Dict) ->
				     AppPaths = filelib:wildcard(ewl_file:join_paths(ErtsDirPath, "*")),
				     ErtsVsn  = filename:basename(ErtsDirPath),
				     populate_dict(AppPaths, ErtsVsn, Dict) 
			     end,
			     dict:new(),
			     filelib:wildcard(ewl_file:join_paths(DocRootDirPath, "*")))).

populate_dict([AppPath|T], ErtsVsn, Dict) ->    
    {ok, {AppName, AppVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(AppPath),
    populate_dict(T, ErtsVsn,  dict:append(AppName, {AppVsn, ErtsVsn, AppPath}, Dict));
populate_dict([], _ErtsVsn, Dict) ->    
    Dict.
