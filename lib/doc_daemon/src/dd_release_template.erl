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
-module(dd_release_template).

%% API
-export([
	 generate_release_doc/3,
	 create_release_index_page/1
	]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("fslib/include/macros.hrl").
-include("doc_daemon.hrl").

-define(VERSION_HISTORY_DEPTH, 5).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @todo separate this function out so that pathing concerns are moved into dd_doc_builder and only rendering is here.
%% @doc Create release index file.
%% @spec generate_release_doc(RelDocRootDirPath, RelDocDirPath, ErtsVsn) -> ok | {error, Reason}
%% where
%%  RelDocRootDirPath = string()
%%  RelDocDirPath = string()
%%  ErtsVsn = string()
%% @end
%%--------------------------------------------------------------------
generate_release_doc(RelDocRootDirPath, RelDocDirPath, ErtsVsn) -> 
    ControlFilePath = ewl_file:join_paths(RelDocDirPath, "control"),
    try
	{ok, IndexTemplate} = get_single_release_template(RelDocRootDirPath),
	true =  epkg_validation:is_valid_control_file(ControlFilePath),
	Attrs = [
		 {erts_vsn, ErtsVsn}, 
		 {name, filename:basename(RelDocDirPath)},
		 {description, epkg_util:consult_control_file(description, ControlFilePath)},
		 {categories, join(epkg_util:consult_control_file(categories, ControlFilePath), ", ")}
		],
	?INFO_MSG("Attributes to render are ~p~n", [Attrs]),
	NewPage = sgte:render_str(IndexTemplate, Attrs),
	true = is_list(NewPage),
	DocIndexFilePath = ewl_file:join_paths(RelDocDirPath, "index.html"),
	{ok, IOD} = file:open(DocIndexFilePath, [write]),
	ok = io:fwrite(IOD, "~s", [NewPage]),
	?INFO_MSG("Page Rendered and written out to ~s~n", [DocIndexFilePath])
    catch
	_C:Error ->
	    ?ERROR_MSG("release src under ~p failed to render with ~p~n", [RelDocDirPath, Error]),
	    {error, {release_doc, Error}}
    end.

%%--------------------------------------------------------------------
%% @todo separate this function out so that pathing concerns are moved into dd_doc_builder and only rendering is here.
%% @doc Create release index file.
%% @spec create_release_index_page(DocSpec) -> ok
%% where
%%  DocSpec = record()
%% @end
%%--------------------------------------------------------------------
create_release_index_page(DocSpec) -> 
    #doc_spec{webserver_doc_root      = DocRoot,
	      generated_docs_base_dir = ErlDocRootDirPath,
	      release_index_file      = IndexFilePath} = DocSpec,

    RelDocRootDirPath = ewl_file:join_paths(ErlDocRootDirPath, "releases"),
    ewl_file:mkdir_p(RelDocRootDirPath),

    {ok, IndexTemplate} = get_index_template(DocSpec),
    {ok, PartTemplate}  = get_part_template(RelDocRootDirPath),
    NewPage             = render_page(IndexTemplate, PartTemplate, gather_release_specs(RelDocRootDirPath), DocRoot),
    case NewPage of
	NewPage when is_list(NewPage) -> 
	    ?INFO_MSG("Page Rendered~n", []),
	    {ok, IOD} = file:open(IndexFilePath, [write]),
	    ok = io:fwrite(IOD, "~s", [NewPage]);
	Error ->
	    ?ERROR_MSG("release src under ~p failed to render with ~p~n", [RelDocRootDirPath, Error])
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private 
%% @doc fetch the release index template. If it does not exist create it and then fetch it. 
%% @end
%%--------------------------------------------------------------------
get_single_release_template(RelDocRootDirPath) ->
    RelTemplateFilePath = ewl_file:join_paths(RelDocRootDirPath, "single_release_doc_template.src"),
    case sgte:compile_file(RelTemplateFilePath) of
	{ok, _} = Resp -> 
	    Resp;
	{error, enoent} ->
	    Page = join(
		     ["<html>", 
		      " <head><title>$name$ Erlang/OTP Release Documentation</title></head>",
		      " <body>",
		      "  <h1>$name$ Erlang/OTP Release Documentation</h1>",
		      "  <strong>Description:</strong><p>$description$</p>",
		      "  <strong>Categories:</strong><p>$categories$</p>",
		      "  <strong>Compiled for Erts version:</strong> $erts_vsn$",
		      "  <br/><hr/><small>Powered by Erlware Portius</small>",
		      " </body>",
		      "</html>"],
		     "\n"),
	    ?ERROR_MSG("Could not find blank release template file at ~s. Creating one : ~p~n", [RelTemplateFilePath, Page]),
	    
	    {ok, IOD} = file:open(RelTemplateFilePath, [write]),
	    ok = io:fwrite(IOD, "~s", [Page]),
	    get_index_template(RelDocRootDirPath)
    end.


%%--------------------------------------------------------------------
%% @private 
%% @doc fetch the release index template. If it does not exist create it and then fetch it. 
%% @end
%%--------------------------------------------------------------------
get_index_template(#doc_spec{release_index_file_src = IndexFilePath, release_index_file = IndexFilePath}) ->
    IndexSrcFilePath = IndexFilePath ++ ".src",
    case {filelib:is_file(IndexFilePath), filelib:is_file(IndexSrcFilePath)} of
	{true, false} -> 
	    file:copy(IndexFilePath, IndexSrcFilePath);
	{_, _} ->
	    ok
    end,
    get_index_template(IndexSrcFilePath);
get_index_template(#doc_spec{release_index_file_src = IndexSrcFilePath}) ->
    get_index_template(IndexSrcFilePath);
get_index_template(SrcFilePath) ->
    case sgte:compile_file(SrcFilePath) of
	{ok, _} = Resp -> 
	    Resp;
	{error, enoent} ->
	    ?ERROR_MSG("Could not find blank release index file at ~s. Creating one~n", [SrcFilePath]),
	    {ok, IOD} = file:open(SrcFilePath, [write]),
	    Page = join(
		     ["<html>", 
		      " <head><title>Erlware Erlang/OTP Release Listing</title></head>",
		      " <body>",
		      "  <h1>Erlware Erlang/OTP Release Listing</h1>",
		      "  $release_list$",
		      "  <br/><hr/><small>Powered by Erlware Portius</small>",
		      " </body>",
		      "</html>"],
		     "\n"),
	    ok = io:fwrite(IOD, "~s", [Page]),
	    get_index_template(SrcFilePath)
    end.

%%--------------------------------------------------------------------
%% @private 
%% @doc fetch the release part template. If it does not exist create it and then fetch it. 
%% @end
%%--------------------------------------------------------------------
get_part_template(RelDocRootDirPath) ->
    PartFilePath = ewl_file:join_paths(RelDocRootDirPath, "listing.part"),
    case sgte:compile_file(PartFilePath) of
	{ok, _} = Resp -> 
	    Resp;
	{error, enoent} ->
	    ?ERROR_MSG("Could not find release listing .part file at ~s. Creating one~n", [PartFilePath]),
	    {ok, IOD} = file:open(PartFilePath, [write]),
	    Page = join(
		     ["<li>", 
		      " $releasespec.name$ <a href=\"$releasespec.path$\/\">$releasespec.vsn$</a><br/>",
		      " <small>Older Versions: $releasespec.back_vsns$</small><br/><br/>",
		      "</li>"],
		     "\n"),
	    ok = io:fwrite(IOD, "~s", [Page]),
	    get_part_template(RelDocRootDirPath)
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
    [{HighRelVsn, HighRelPath}|AggregatedAttributes] = sort_by_version(Attributes),

    SpecList = [
		{name, RelName}, 
		{vsn, HighRelVsn}, 
		{path, lists:concat([www_renderable_path(HighRelPath, DocRoot), "/index.html"])}
	       ],

    Links = 
	lists:flatten(
	  lists:sublist(
	    lists:map(fun({RelVsn, RelPath}) -> 
			      lists:flatten([" <a href=\"", www_renderable_path(RelPath, DocRoot),"/index.html\">", RelVsn, "</a> |"]) 
		      end, 
		      AggregatedAttributes), ?VERSION_HISTORY_DEPTH)),

    FullSpecList = case Links of
		       "--"    -> SpecList;
		       Links -> [{back_vsns, lists:flatten(["(", string:strip(Links , right, $|), ")"])}|SpecList]
		   end,
    
    [{releasespec, FullSpecList}|massage_release_specs(T, DocRoot)];
massage_release_specs([], _DocRoot) ->
    [].

sort_by_version(Attributes) ->
    lists:sort(fun(A1, A2) -> ewr_util:is_version_greater(element(1, A1), element(1, A2)) end, Attributes).
    
www_renderable_path(Path, DocRoot) ->
    string:substr(Path, length(DocRoot) + 1, length(Path)).

%%--------------------------------------------------------------------
%% @private
%% @doc Fetch data about currently doc'd releases on the file system. Return a list of [{RelName, [{RelVsn, EtsVsn, RelDocPath}]}]
%% @end
%%--------------------------------------------------------------------
gather_release_specs(RelDocRootDirPath) ->
    RelPaths = filelib:wildcard(ewl_file:join_paths(RelDocRootDirPath, "*")),
    dict:to_list(populate_dict(RelPaths, dict:new())).
			    
populate_dict([RelPath|T], Dict) ->    
    case filelib:is_dir(RelPath) of
	true -> 
	    try
		{ok, {RelName, RelVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(RelPath),
		populate_dict(T, dict:append(RelName, {RelVsn, RelPath}, Dict))
	    catch 
		_C:_E ->
		    ?ERROR_MSG("failed on ~p~n", [RelPath]),
		    populate_dict(T, Dict)
	    end;
	false ->
	    populate_dict(T, Dict)
    end;
populate_dict([], Dict) ->    
    Dict.

join([H], _Separator) when is_list(H) ->
    H;
join([H|T], Separator) when is_atom(H) ->
    join([atom_to_list(H)|T], Separator);
join([H|T], Separator) ->
    lists:flatten([H, Separator, join(T, Separator)]);
join([], _Separator) ->
    [].
