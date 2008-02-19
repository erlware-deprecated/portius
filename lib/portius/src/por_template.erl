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

-define(VERSION_HISTORY_DEPTH, 5).

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
    {ok, IndexTemplate} = get_index_template(BlankAppIndexFilePath),
    {ok, PartTemplate}  = get_part_template(BlankAppIndexFilePath),
    NewPage             = render_page(IndexTemplate, PartTemplate, gather_app_specs(DocRootDirPath), DocRoot),
    case NewPage of
	NewPage when is_list(NewPage) -> 
	    ?INFO_MSG("Page Rendered~n", []),
	    {ok, IOD} = file:open(RenderedAppIndexFilePath, [write]),
	    ok = io:fwrite(IOD, "~s", [NewPage]);
	Error ->
	    ?ERROR_MSG("~p failed to render with ~p~n", [RenderedAppIndexFilePath, Error])
    end.


%%--------------------------------------------------------------------
%% @private 
%% @doc fetch the app index template. If it does not exist create it and then fetch it. 
%% @end
%%--------------------------------------------------------------------
get_index_template(BlankAppIndexFilePath) ->
    case sgte:compile_file(BlankAppIndexFilePath) of
	{ok, _} = Resp -> 
	    Resp;
	{error, enoent} ->
	    ?ERROR_MSG("Could not find blank app index file at ~s. Creating one~n", [BlankAppIndexFilePath]),
	    {ok, IOD} = file:open(BlankAppIndexFilePath, [write]),
	    ok = io:fwrite(IOD, "~s", ["<html>\n <head></head>\n <body>\n  <h1>App Docs</h1>\n  <ul>\n" ++
				       "$app_list$\n  </ul>\n <small>powered by Erlware Portius</small>\n </body>\n</html>"]),
	    get_index_template(BlankAppIndexFilePath)
    end.

%%--------------------------------------------------------------------
%% @private 
%% @doc fetch the app part template. If it does not exist create it and then fetch it. 
%% @end
%%--------------------------------------------------------------------
get_part_template(BlankAppIndexFilePath) ->
    PartFilePath = ewl_file:join_paths(filename:dirname(BlankAppIndexFilePath), "app_index.part"),
    case sgte:compile_file(PartFilePath) of
	{ok, _} = Resp -> 
	    Resp;
	{error, enoent} ->
	    ?ERROR_MSG("Could not find app index .part file at ~s. Creating one~n", [PartFilePath]),
	    {ok, IOD} = file:open(PartFilePath, [write]),
	    ok = io:fwrite(IOD, "~s", ["<li>$appspec.name$ <a href=\"$appspec.path$\">$appspec.vsn$" ++ 
				       "</a> (Erts: $appspec.erts_vsn$) "++
				       "<br><small>Older Versions: $appspec.back_vsns$</small></li>"]),
	    get_part_template(BlankAppIndexFilePath)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Given the correct template and the app data collected from the file system return a rendered app index page.
%% @end
%%--------------------------------------------------------------------
render_page(IndexTemplate, PartTemplate, AppSpecs, DocRoot) ->
    {ok, Map} = sgte:compile("$map li apps$"),
    Str = sgte:render_str(Map, [{li, PartTemplate}, {apps, create_renderable_specs(AppSpecs, DocRoot)}]),
    sgte:render_str(IndexTemplate, [{app_list, Str}]).
    
create_renderable_specs(Specs, DocRoot) ->
    lists:flatten(massage_app_specs(Specs, DocRoot)).

massage_app_specs([{AppName, Attributes}|T], DocRoot) ->
    [{HighAppVsn, HighErtsVsn, HighAppPath}|AggregatedAttributes] = group_erts_vsns(sort_by_version(Attributes)),

    SpecList = [
		{name, AppName}, 
		{vsn, HighAppVsn}, 
		{erts_vsn, HighErtsVsn}, 
		{path, renderable_path(HighAppPath, DocRoot)}
	       ],

    Links = 
	lists:flatten(
	  lists:sublist(
	    lists:map(fun({AppVsn, _ErtsVsn, AppPath}) -> 
			      lists:flatten([" <a href=\"", renderable_path(AppPath, DocRoot),"\">", AppVsn, "</a> |"]) 
		      end, 
		      AggregatedAttributes), ?VERSION_HISTORY_DEPTH)),

    FullSpecList = case Links of
		       "--"    -> SpecList;
		       Links -> [{back_vsns, lists:flatten(["(", string:strip(Links , right, $|), ")"])}|SpecList]
		   end,
    
    [{appspec, FullSpecList}|massage_app_specs(T, DocRoot)];
massage_app_specs([], _DocRoot) ->
    [].

group_erts_vsns(Attributes) -> lists:reverse(group_erts_vsns(Attributes, [])).

group_erts_vsns([{AppVsn, ErtsVsn, _}|T], [{AppVsn, ErtsVsns, AppPath}|AccT]) ->
    group_erts_vsns(T, [{AppVsn, lists:flatten([ErtsVsns, ", ", ErtsVsn]), AppPath}|AccT]);
group_erts_vsns([H|T], Acc) ->
    group_erts_vsns(T, [H|Acc]);
group_erts_vsns([], Acc) ->
    Acc.
    
    
sort_by_version(Attributes) ->
    lists:sort(fun(A1, A2) -> ewr_util:is_version_greater(element(1, A1), element(1, A2)) end, Attributes).
    
renderable_path(Path, DocRoot) ->
    string:substr(Path, length(DocRoot) + 1, length(Path)).

%%--------------------------------------------------------------------
%% @private
%% @doc Fetch data about currently doc'd apps on the file system. Return a list of [{AppName, [{AppVsn, EtsVsn, AppDocPath}]}]
%% @end
%%--------------------------------------------------------------------
gather_app_specs(DocRootDirPath) ->
    dict:to_list(lists:foldl(fun(ErtsDirPath, Dict) ->
				     case filelib:is_dir(ErtsDirPath) of
					 true ->
					     AppPaths = filelib:wildcard(ewl_file:join_paths(ErtsDirPath, "lib/*")),
					     ErtsVsn  = filename:basename(ErtsDirPath),
					     populate_dict(AppPaths, ErtsVsn, Dict);
					 false ->
					     Dict
				     end
			     end,
			     dict:new(),
			     filelib:wildcard(ewl_file:join_paths(DocRootDirPath, "*")))).

populate_dict([AppPath|T], ErtsVsn, Dict) ->    
    try
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

