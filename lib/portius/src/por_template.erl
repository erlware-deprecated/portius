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
-export([erlware_site/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Builds erlware-site documentation links page
%% @spec erlware_site([app_info()]) -> ok
%% @end
%%--------------------------------------------------------------------
erlware_site(AppNames) ->
    apps_template(AppNames).

%%====================================================================
%% Internal functions
%%====================================================================
apps_template(AppNames) ->
    Apps = [{app, AppName} || AppName <- AppNames],
    {ok, AppTemplate} = sgte:compile(" * $app_name$~n"),
    {ok, AppsTemplate} = sgte:compile("$map app app_names$"),
    sgte:render_str(
      AppsTemplate,
      [{app, AppTemplate}, {app_names, [{app_name, foo}, {app_name, bar}]}]).
