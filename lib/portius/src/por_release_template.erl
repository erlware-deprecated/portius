%%%-------------------------------------------------------------------
%%% @author Samuel Rivas <samuelrivas@gmail.com> and Martin Logan
%%% @copyright (C) 2008, Erlware
%%% @doc This module compiles templates with the information about
%%% releaselications comming form other portius modules
%%%
%%% @type release_info() = {RelName, [RelVersionInfo]}
%%%                    RelName = atom()
%%%                    RelVersionInfo = {Version, [ErtsVersion]}
%%%                    Version = string()
%%%                    ErtsVresion = string()
%%% @end
%%%-------------------------------------------------------------------
-module(por_release_template).

%% API
-export([
	 generate_release_doc/2,
	 create_release_index_page/2,
	 create_release_index_page/3
	]).

-include("eunit.hrl").
-include("macros.hrl").

-define(VERSION_HISTORY_DEPTH, 5).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Create release index file.
%% @spec generate_release_doc(RelDocDirPath, ErtsVsn) -> ok
%% where
%%  IndexFilePath = string()
%%  ErlDocRootDirPath = string()
%%  DocRoot = string()
%% @end
%%--------------------------------------------------------------------
generate_release_doc(RelDocDirPath, ErtsVsn) -> 
    {ok, IndexTemplate} = get_index_template(ErlRelDocRootDirPath),
    {ok, PartTemplate}  = get_part_template(ErlRelDocRootDirPath),
    NewPage             = render_page(IndexTemplate, PartTemplate, gather_release_specs(ErlRelDocRootDirPath), DocRoot),
    case NewPage of
	NewPage when is_list(NewPage) -> 
	    ?INFO_MSG("Page Rendered~n", []),
	    {ok, IOD} = file:open(IndexFilePath, [write]),
	    ok = io:fwrite(IOD, "~s", [NewPage]);
	Error ->
	    ?ERROR_MSG("release src under ~p failed to render with ~p~n", [ErlRelDocRootDirPath, Error])
    end.

%%--------------------------------------------------------------------
%% @doc Create release index file.
%% @spec create_release_index_page(IndexFilePath, ErlDocRootDirPath, DocRoot) -> ok
%% where
%%  IndexFilePath = string()
%%  ErlDocRootDirPath = string()
%%  DocRoot = string()
%% @end
%%--------------------------------------------------------------------
create_release_index_page(IndexFilePath, ErlDocRootDirPath, DocRoot) -> 
    ewl_file:mkdir_p(ErlDocRootDirPath),
    ErlRelDocRootDirPath = ewl_file:join_paths(ErlDocRootDirPath, "lib"),
    ewl_file:mkdir_p(ErlRelDocRootDirPath),

    {ok, IndexTemplate} = get_index_template(ErlRelDocRootDirPath),
    {ok, PartTemplate}  = get_part_template(ErlRelDocRootDirPath),
    NewPage             = render_page(IndexTemplate, PartTemplate, gather_release_specs(ErlRelDocRootDirPath), DocRoot),
    case NewPage of
	NewPage when is_list(NewPage) -> 
	    ?INFO_MSG("Page Rendered~n", []),
	    {ok, IOD} = file:open(IndexFilePath, [write]),
	    ok = io:fwrite(IOD, "~s", [NewPage]);
	Error ->
	    ?ERROR_MSG("release src under ~p failed to render with ~p~n", [ErlRelDocRootDirPath, Error])
    end.

%% @spec create_release_index_page(ErlDocRootDirPath, DocRoot) -> ok
%% @equiv create_release_index_page(DefaultIndexFilePath, ErlDocRootDirPath, DocRoot)
create_release_index_page(ErlDocRootDirPath, DocRoot) -> 
    create_release_index_page(ewl_file:join_paths(ErlDocRootDirPath, "lib/index.html"), ErlDocRootDirPath, DocRoot).

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private 
%% @doc fetch the release index template. If it does not exist create it and then fetch it. 
%% @end
%%--------------------------------------------------------------------
get_index_template(ErlRelDocRootDirPath) ->
    SrcFilePath = ewl_file:join_paths(ErlRelDocRootDirPath, "index.src"),
    case sgte:compile_file(SrcFilePath) of
	{ok, _} = Resp -> 
	    Resp;
	{error, enoent} ->
	    ?ERROR_MSG("Could not find blank release index file at ~s. Creating one~n", [SrcFilePath]),
	    {ok, IOD} = file:open(SrcFilePath, [write]),
	    ok = io:fwrite(IOD, "~s", ["<html>\n <head></head>\n <body>\n  <h1>Rel Docs</h1>\n  <ul>\n" ++
				       "$release_list$\n  </ul>\n <small>powered by Erlware Portius</small>\n </body>\n</html>"]),
	    get_index_template(ErlRelDocRootDirPath)
    end.

%%--------------------------------------------------------------------
%% @private 
%% @doc fetch the release part template. If it does not exist create it and then fetch it. 
%% @end
%%--------------------------------------------------------------------
get_part_template(ErlRelDocRootDirPath) ->
    PartFilePath = ewl_file:join_paths(ErlRelDocRootDirPath, "listing.part"),
    case sgte:compile_file(PartFilePath) of
	{ok, _} = Resp -> 
	    Resp;
	{error, enoent} ->
	    ?ERROR_MSG("Could not find release listing .part file at ~s. Creating one~n", [PartFilePath]),
	    {ok, IOD} = file:open(PartFilePath, [write]),
	    ok = io:fwrite(IOD, "~s", ["<li>$releasespec.name$ <a href=\"$releasespec.path$\">$releasespec.vsn$" ++ 
				       "</a> (Erts: $releasespec.erts_vsn$) "++
				       "<br><small>Older Versions: $releasespec.back_vsns$</small></li>"]),
	    get_part_template(ErlRelDocRootDirPath)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Given the correct template and the release data collected from the file system return a rendered release index page.
%% @end
%%--------------------------------------------------------------------
render_page(IndexTemplate, PartTemplate, RelSpecs, DocRoot) ->
    {ok, Map} = sgte:compile("$map li releases$"),
    Str = sgte:render_str(Map, [{li, PartTemplate}, {releases, create_renderable_specs(RelSpecs, DocRoot)}]),
    sgte:render_str(IndexTemplate, [{release_list, Str}]).
    
create_renderable_specs(Specs, DocRoot) ->
    lists:sort(fun({releasespec, L1}, {releasespec, L2}) -> fs_lists:get_val(name, L1) < fs_lists:get_val(name, L2)
	       end, lists:flatten(massage_release_specs(Specs, DocRoot))).

massage_release_specs([{RelName, Attributes}|T], DocRoot) ->
    [{HighRelVsn, HighErtsVsn, HighRelPath}|AggregatedAttributes] = group_erts_vsns(sort_by_version(Attributes)),

    SpecList = [
		{name, RelName}, 
		{vsn, HighRelVsn}, 
		{erts_vsn, HighErtsVsn}, 
		{path, renderable_path(HighRelPath, DocRoot)}
	       ],

    Links = 
	lists:flatten(
	  lists:sublist(
	    lists:map(fun({RelVsn, _ErtsVsn, RelPath}) -> 
			      lists:flatten([" <a href=\"", renderable_path(RelPath, DocRoot),"\">", RelVsn, "</a> |"]) 
		      end, 
		      AggregatedAttributes), ?VERSION_HISTORY_DEPTH)),

    FullSpecList = case Links of
		       "--"    -> SpecList;
		       Links -> [{back_vsns, lists:flatten(["(", string:strip(Links , right, $|), ")"])}|SpecList]
		   end,
    
    [{releasespec, FullSpecList}|massage_release_specs(T, DocRoot)];
massage_release_specs([], _DocRoot) ->
    [].

group_erts_vsns(Attributes) -> lists:reverse(group_erts_vsns(Attributes, [])).

group_erts_vsns([{RelVsn, ErtsVsn, _}|T], [{RelVsn, ErtsVsns, RelPath}|AccT]) ->
    group_erts_vsns(T, [{RelVsn, lists:flatten([ErtsVsns, ", ", ErtsVsn]), RelPath}|AccT]);
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
%% @doc Fetch data about currently doc'd releases on the file system. Return a list of [{RelName, [{RelVsn, EtsVsn, RelDocPath}]}]
%% @end
%%--------------------------------------------------------------------
gather_release_specs(ErlRelDocRootDirPath) ->
    dict:to_list(lists:foldl(fun(ErtsDirPath, Dict) ->
				     case filelib:is_dir(ErtsDirPath) of
					 true ->
					     RelPaths = filelib:wildcard(ewl_file:join_paths(ErtsDirPath, "*")),
					     ErtsVsn  = filename:basename(ErtsDirPath),
					     populate_dict(RelPaths, ErtsVsn, Dict);
					 false ->
					     Dict
				     end
			     end,
			     dict:new(),
			     filelib:wildcard(ewl_file:join_paths(ErlRelDocRootDirPath, "*")))).

populate_dict([RelPath|T], ErtsVsn, Dict) ->    
    try
	{ok, {RelName, RelVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(RelPath),
	populate_dict(T, ErtsVsn,  dict:releaseend(RelName, {RelVsn, ErtsVsn, RelPath}, Dict))
    catch 
	_C:_E ->
	    ?ERROR_MSG("failed on ~p~n", [RelPath]),
	    populate_dict(T, ErtsVsn,  Dict)
    end;
populate_dict([], _ErtsVsn, Dict) ->    
    Dict.

