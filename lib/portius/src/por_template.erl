%%%-------------------------------------------------------------------
%%% @author Samuel Rivas <samuelrivas@gmail.com> and Martin Logan
%%% @copyright (C) 2008, Erlware
%%% @doc This module compiles templates with the information about
%%% applications comming form other portius modules
%%%
%%% @type app_info() = {AppName, [AppVersionInfo]}
%%%                    AppName = atom()
%%%                    AppVersionInfo = {Version, [ErtsVersion]}
%%%                    Version = string()
%%%                    ErtsVresion = string()
%%% @end
%%%-------------------------------------------------------------------
-module(por_template).

%% API
-export([
	 create_app_index_page/3
	]).

-include("eunit.hrl").
-include("macros.hrl").

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Create app index file.
%% @spec create_app_index_page(BlankAppIndexTemplateFilePath, RenderedAppIndexTemplateFilePath, DocRootDirPath) -> ok
%% where
%%  BlankAppIndexTemplateFilePath = string()
%%  RenderedAppIndexTemplateFilePath = string()
%%  DocRootDirPath = string()
%% @end
%%--------------------------------------------------------------------
create_app_index_page(BlankAppIndexFilePath, RenderedAppIndexFilePath, DocRootDirPath) -> 
    ?INFO_MSG("~p ~p~n", [BlankAppIndexFilePath, DocRootDirPath]),
    AppSpecs                       = gather_app_specs(DocRootDirPath),
    ?INFO_MSG("app specs ~p~n", [AppSpecs]),
    {ok, CompiledAppIndexTemplate} = sgte:compile_file(BlankAppIndexFilePath),
    NewPage = render_page(CompiledAppIndexTemplate, AppSpecs),
    {ok, IOD} = file:open(RenderedAppIndexFilePath, [write]),
    ok = io:fwrite(IOD, "~s", [NewPage]).

render_page(CompiledAppIndexTemplate, AppSpecs) ->
    {ok, AppTemplate} = sgte:compile("<li>$appspec.name$ <a href=\"$appspec.path$\">$appspec.vsn$</a> $appspec.erts_vsn$</li>"),
    {ok, Map} = sgte:compile("$map li apps$"),
    Str = sgte:render_str(Map, [{li, AppTemplate}, {apps, massage_app_specs(AppSpecs)}]),
    sgte:render_str(CompiledAppIndexTemplate, [{app_list, Str}]).
    
massage_app_specs([{AppName, Attributes}|T]) ->
    RenderableAppSpec = lists:map(
			  fun({AppVsn, ErtsVsn, AppPath}) ->
				  {appspec, [{name, AppName}, {vsn, AppVsn}, {erts_vsn, ErtsVsn}, {path, AppPath}]}
			  end,
			  Attributes),
     [RenderableAppSpec|massage_app_specs(T)];
massage_app_specs([]) ->
    [].
    

gather_app_specs(DocRootDirPath) ->
    dict:to_list(lists:foldl(fun(ErtsDirPath, Dict) ->
				     AppPaths = filelib:wildcard(ewl_file:join_paths(ErtsDirPath, "lib/*")),
				     ErtsVsn  = filename:basename(ErtsDirPath),
				     populate_dict(AppPaths, ErtsVsn, Dict) 
			     end,
			     dict:new(),
			     filelib:wildcard(ewl_file:join_paths(DocRootDirPath, "*")))).

populate_dict([AppPath|T], ErtsVsn, Dict) ->    
    try
	?INFO_MSG("split name and version out of ~p~n", [AppPath]),
	{ok, {AppName, AppVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(AppPath),
	populate_dict(T, ErtsVsn,  dict:append(AppName, {AppVsn, ErtsVsn, AppPath}, Dict))
    catch 
	_C:_E ->
	    ?ERROR_MSG("failed on ~p~n", [AppPath]),
	    populate_dict(T, ErtsVsn,  Dict)
    end;
populate_dict([], _ErtsVsn, Dict) ->    
    Dict.

%%====================================================================
%% Test Functions
%%====================================================================
render_page_test() ->
    {ok, CompiledTemplate} = sgte:compile("<li>$app_spec$</li>"),
    Result = "<li>yo</li>\n<li>jo</li>",

    ?assertMatch(Result, render_page(CompiledTemplate, [{app_spec, "yo"}, {app_spec, "jo"}])).
