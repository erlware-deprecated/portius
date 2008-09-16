%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2008, Martin Logan
%%% @doc
%%%  functions for building documentation. 
%%% @end
%%% Created :  8 May 2008 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(por_doc_builder).

%% API
-export([
	 build_index_docs/1,
	 build_release_docs/3,
	 build_app_docs/3
	]).

-include_lib("fslib/include/macros.hrl").
-include("portius.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc use a doc spec to build the index files for app and release documentation.
%% @spec build_index_docs(DocSpec) -> ok
%% @end
%%--------------------------------------------------------------------
build_index_docs(undefined) ->
    ?INFO_MSG("Doc transition spec undefined. Skipping doc building.~n", []),
    ok;
build_index_docs(DocSpec) ->
    AIR = (catch por_app_template:create_app_index_page(DocSpec)),
    RIR = (catch por_release_template:create_release_index_page(DocSpec)),
    ?INFO_MSG("Result of create app index page call ~p~n", [AIR]),
    ?INFO_MSG("Result of create release index page call ~p~n", [RIR]).

%%--------------------------------------------------------------------
%% @doc Build the documentation for an application.
%% @spec build_app_docs(PackageDirPath, ErtsVsn, DocSpec::record()) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
build_app_docs(PackageDirPath, _ErtsVsn, undefined) ->
    ?INFO_MSG("Doc transition spec undefined for ~p. Skipping doc building.~n", [PackageDirPath]),
    ok;
build_app_docs(PackageDirPath, ErtsVsn, #doc_spec{generated_docs_base_dir = DocDirPath}) ->
    {ok, {AppName, AppVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(PackageDirPath),
    case catch edoc:application(list_to_atom(AppName), PackageDirPath, []) of
	ok -> 
	    GeneratedDocDirPath = filename:join([PackageDirPath, "doc"]),
	    LibDocDirPath       = filename:join([DocDirPath, "lib", ErtsVsn, AppName ++ "-" ++ AppVsn]),
	    ewl_file:mkdir_p(LibDocDirPath),
	    ?INFO_MSG("copy doc dir from ~s to ~s~n", [GeneratedDocDirPath, LibDocDirPath]),
	    ewl_file:copy_dir(GeneratedDocDirPath, LibDocDirPath);
	Error -> 
	    ?ERROR_MSG("doc failed for ~s-~s with ~p~n", [AppName, AppVsn, Error]),
	    {error, {doc_failed, AppName, AppVsn, Error}}
    end.
	    
%%--------------------------------------------------------------------
%% @doc Build the documentation for an release. 
%% @spec build_release_docs(PackageDirPath, ErtsVsn, DocSpec::record()) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
build_release_docs(PackageDirPath, _ErtsVsn, undefined) ->
    ?INFO_MSG("Doc transition spec undefined for ~p. Skipping doc building.~n", [PackageDirPath]),
    ok;
build_release_docs(PackageDirPath, ErtsVsn, #doc_spec{generated_docs_base_dir = DocDirPath}) ->
    {ok, {RelName, RelVsn}} = epkg_installed_paths:package_dir_to_name_and_vsn(PackageDirPath),
    ControlFilePath = ewl_file:join_paths(PackageDirPath, "control"),
    case filelib:is_file(ControlFilePath) of
	true -> 
	    RelDocBaseDirPath = ewl_file:join_paths(DocDirPath, "releases"),
	    RelDocDirPath = ewl_file:join_paths(RelDocBaseDirPath, RelName ++ "-" ++ RelVsn),
	    ewl_file:mkdir_p(RelDocDirPath),
	    ?INFO_MSG("copy doc dir from ~s to ~s~n", [ControlFilePath, RelDocDirPath]),
	    file:copy(ControlFilePath, ewl_file:join_paths(RelDocDirPath, "control")),
	    Reply = por_release_template:generate_release_doc(RelDocBaseDirPath, RelDocDirPath, ErtsVsn),
	    ?INFO_MSG("release doc generation returned ~p~n", [Reply]);
	false  -> 
	    ?ERROR_MSG("doc failed for ~s-~s because the release has no control file~n", [RelName, RelVsn]),
	    {error, {doc_failed, RelName, RelVsn}}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
