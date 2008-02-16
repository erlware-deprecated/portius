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
	 create_app_index_page/4
	]).

-include("eunit.hrl").
-include("macros.hrl").

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Create app index file.
%% @spec create_app_index_page(BlankAppIndexTemplateFilePath, RenderedAppIndexTemplateFilePath, DocRootDirPath, DocRoot) -> ok
%% where
%%  BlankAppIndexTemplateFilePath = string()
%%  RenderedAppIndexTemplateFilePath = string()
%%  DocRootDirPath = string()
%%  DocRoot = string()
%% @end
%%--------------------------------------------------------------------
create_app_index_page(BlankAppIndexFilePath, RenderedAppIndexFilePath, DocRootDirPath, DocRoot) -> 
    ?INFO_MSG("~p ~p~n", [BlankAppIndexFilePath, DocRootDirPath]),
    AppSpecs                       = gather_app_specs(DocRootDirPath),
    ?INFO_MSG("app specs ~p~n", [AppSpecs]),
    {ok, CompiledAppIndexTemplate} = get_compiled_template(BlankAppIndexFilePath),
    NewPage = render_page(CompiledAppIndexTemplate, AppSpecs, DocRoot),
    {ok, IOD} = file:open(RenderedAppIndexFilePath, [write]),
    ok = io:fwrite(IOD, "~s", [NewPage]).

get_compiled_template(BlankAppIndexFilePath) ->
    case sgte:compile_file(BlankAppIndexFilePath) of
	{ok, _} = Resp -> 
	    Resp;
	{error, enoent} ->
	    ?ERROR_MSG("Could not find blank app index file at ~s. Creating one~n", [BlankAppIndexFilePath]),
	    {ok, IOD} = file:open(BlankAppIndexFilePath, [write]),
	    ok = io:fwrite(IOD, "~s", ["<html>\n <head></head>\n <body>\n  <h1>App Docs</h1>\n  <ul>\n" ++
				       "$app_list$\n  </ul>\n <small>powered by Erlware Portius</small>\n </body>\n</html>"]),
	    get_compiled_template(BlankAppIndexFilePath)
    end.

render_page(CompiledAppIndexTemplate, AppSpecs, DocRoot) ->
    {ok, AppTemplate} = sgte:compile("<li>$appspec.name$ <a href=\"$appspec.path$\">$appspec.vsn$</a> $appspec.erts_vsn$</li>"),
    {ok, Map} = sgte:compile("$map li apps$"),
    Str = sgte:render_str(Map, [{li, AppTemplate}, {apps, massage_app_specs(AppSpecs, DocRoot)}]),
    sgte:render_str(CompiledAppIndexTemplate, [{app_list, Str}]).
    
massage_app_specs([{AppName, Attributes}|T], DocRoot) ->
    RenderableAppSpec = lists:map(
			  fun({AppVsn, ErtsVsn, AppPath}) ->
				  {appspec, [{name, AppName}, {vsn, AppVsn}, {erts_vsn, ErtsVsn}, {path, string:substr(AppPath, length(DocRoot) + 1, length(AppPath))}]}
			  end,
			  Attributes),
     [RenderableAppSpec|massage_app_specs(T, DocRoot)];
massage_app_specs([], _DocRoot) ->
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

