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
-module(por_app_template).

%% API
-export([
	 create_app_index_page/1
	]).

-include("eunit.hrl").
-include("macros.hrl").
-include("portius.hrl").

-define(VERSION_HISTORY_DEPTH, 5).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @todo separate this function out so that pathing concerns are moved into por_doc_builder and only rendering is here.
%% @doc Create app index file.
%% @spec create_app_index_page(DocSpec) -> ok
%% where
%%  DocSpec = record()
%% @end
%%--------------------------------------------------------------------
create_app_index_page(DocSpec) -> 
    #doc_spec{webserver_doc_root      = DocRoot,
	      generated_docs_base_dir = ErlDocRootDirPath,
	      app_index_file          = IndexFilePath} = DocSpec,
		    
    ewl_file:mkdir_p(ErlDocRootDirPath),
    ErlAppDocRootDirPath = ewl_file:join_paths(ErlDocRootDirPath, "lib"),
    ewl_file:mkdir_p(ErlAppDocRootDirPath),

    {ok, IndexTemplate} = get_index_template(DocSpec),
    {ok, PartTemplate}  = get_part_template(ErlAppDocRootDirPath),
    NewPage             = render_page(IndexTemplate, PartTemplate, gather_app_specs(ErlAppDocRootDirPath), DocRoot),
    case NewPage of
	NewPage when is_list(NewPage) -> 
	    ?INFO_MSG("Page Rendered~n", []),
	    {ok, IOD} = file:open(IndexFilePath, [write]),
	    ok = io:fwrite(IOD, "~s", [NewPage]);
	Error ->
	    ?ERROR_MSG("app src under ~p failed to render with ~p~n", [ErlAppDocRootDirPath, Error])
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private 
%% @doc fetch the app index template. If it does not exist create it and then fetch it. 
%% @end
%%--------------------------------------------------------------------
get_index_template(#doc_spec{app_index_file_src = IndexFilePath, app_index_file = IndexFilePath}) ->
    IndexSrcFilePath = IndexFilePath ++ ".src",
    case {filelib:is_file(IndexFilePath), filelib:is_file(IndexSrcFilePath)} of
	{true, false} -> 
	    file:copy(IndexFilePath, IndexSrcFilePath);
	{_, _} ->
	    ok
    end,
    get_index_template(IndexSrcFilePath);
get_index_template(#doc_spec{app_index_file_src = IndexSrcFilePath}) ->
    get_index_template(IndexSrcFilePath);
get_index_template(SrcFilePath) ->
    case sgte:compile_file(SrcFilePath) of
	{ok, _} = Resp -> 
	    Resp;
	{error, enoent} ->
	    ?ERROR_MSG("Could not find blank app index file at ~s. Creating one~n", [SrcFilePath]),
	    {ok, IOD} = file:open(SrcFilePath, [write]),
	    Page = join(
		     ["<html>", 
		      " <head><title>Erlware Erlang/OTP Application Documentation</title></head>",
		      " <body>",
		      "  <h1>Erlware Erlang/OTP Application Documentation</h1>",
		      "  $app_list$",
		      "  <br/><hr/><small>Powered by Erlware Portius</small>",
		      " </body>",
		      "</html>"],
		     "\n"),
	    ?ERROR_MSG("Could not find blank release template file at ~s. Creating one : ~p~n", [SrcFilePath, Page]),
	    ok = io:fwrite(IOD, "~s", [Page]),
	    get_index_template(SrcFilePath)
    end.

%%--------------------------------------------------------------------
%% @private 
%% @doc fetch the app part template. If it does not exist create it and then fetch it. 
%% @end
%%--------------------------------------------------------------------
get_part_template(ErlAppDocRootDirPath) ->
    PartFilePath = ewl_file:join_paths(ErlAppDocRootDirPath, "listing.part"),
    case sgte:compile_file(PartFilePath) of
	{ok, _} = Resp -> 
	    Resp;
	{error, enoent} ->
	    ?ERROR_MSG("Could not find app listing .part file at ~s. Creating one~n", [PartFilePath]),
	    {ok, IOD} = file:open(PartFilePath, [write]),
	    Page = join(
		     ["<li>", 
		      " $appspec.name$ <a href=\"$appspec.path$\">$appspec.vsn$</a> (Erts: $appspec.erts_vsn$) ",
		      " <br><small>Older Versions: $appspec.back_vsns$</small><br/><br/>",
		      "</li>"],
		     "\n"),
	    ok = io:fwrite(IOD, "~s", [Page]),
	    get_part_template(ErlAppDocRootDirPath)
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
    lists:sort(fun({appspec, L1}, {appspec, L2}) -> fs_lists:get_val(name, L1) < fs_lists:get_val(name, L2)
	       end, lists:flatten(massage_app_specs(Specs, DocRoot))).

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
gather_app_specs(ErlAppDocRootDirPath) ->
    dict:to_list(lists:foldl(fun(ErtsDirPath, Dict) ->
				     case filelib:is_dir(ErtsDirPath) of
					 true ->
					     AppPaths = filelib:wildcard(ewl_file:join_paths(ErtsDirPath, "*")),
					     ErtsVsn  = filename:basename(ErtsDirPath),
					     populate_dict(AppPaths, ErtsVsn, Dict);
					 false ->
					     Dict
				     end
			     end,
			     dict:new(),
			     filelib:wildcard(ewl_file:join_paths(ErlAppDocRootDirPath, "*")))).

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


join([H], _Separator) when is_list(H) ->
    H;
join([H|T], Separator) when is_atom(H) ->
    join([atom_to_list(H)|T], Separator);
join([H|T], Separator) ->
    lists:flatten([H, Separator, join(T, Separator)]);
join([], _Separator) ->
    [].
