%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@sixfoe>
%%% @copyright (C) 2008, Martin Logan
%%% @doc
%%%   Creates and processes trees representing a file system.
%%%
%%% @todo think about handling symlinks - do we really need them in our repo?
%%%
%%% @type tree() = File | Dir
%%% where
%%%  Tree = File | Dir
%%%   Dir = {dir, DirName, [Tree]}
%%%   File = {file, FileName}
%%% @end
%%% Created :  2 Jan 2008 by Martin Logan <martinjlogan@sixfoe>
%%%-------------------------------------------------------------------
-module(por_file_tree).

%% API
-export([
	 create_tree/1,
	 find_additions/2,
	 file_paths/1
	]).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Create a term() tree structure represeting a file system tree.
%% @spec create_tree(FromDirPath) -> Tree
%% @end
%%--------------------------------------------------------------------
create_tree(FromDirPath) ->
    FromDirName = filename:basename(FromDirPath),
    case filelib:is_dir(FromDirPath) of
	false ->
	    {file, FromDirName};
	true ->
	    {dir, FromDirName, lists:foldl(fun(CheckFromDirPath, Acc) when CheckFromDirPath == FromDirPath -> 
					       Acc;
					  (ChildFromDirPath, Acc) -> 
					       case create_tree(ChildFromDirPath) of
						   []  -> Acc;
						   Res -> [Res|Acc]
					       end
				       end, [], filelib:wildcard(FromDirPath ++ "/*"))}
    end.

%%--------------------------------------------------------------------
%% @doc Return a tree containing what is present in tree2 that is not present in tree1.
%% @spec find_additions(Tree::tree(), Tree2::tree()) -> Tree | []
%% @end
%%--------------------------------------------------------------------
find_additions({dir, _DirName, List}, {dir, DirName2, List2}) ->
    case diff_subdirs(List, List2) of
	[]         -> [];
	SubDirDiff -> {dir, DirName2, SubDirDiff}
    end;
find_additions(Tree, Tree2) ->
    find_additions2(Tree, Tree2).

find_additions2(Tree, Tree) ->
    [];
find_additions2({dir, DirName, List}, {dir, DirName, List2}) ->
    case diff_subdirs(List, List2) of
	[]         -> [];
	SubDirDiff -> {dir, DirName, SubDirDiff}
    end;
find_additions2(_Tree, Tree2) ->
    Tree2.
 
diff_subdirs(List, [Tree|T]) ->
    lists:flatten([diff_subdirs2(List, Tree), diff_subdirs(List, T)]);
diff_subdirs(_List, []) ->
    [].

diff_subdirs2([Tree|_], Tree) ->
    [];
diff_subdirs2([{dir, Name, _} = Tree2|_], {dir, Name, _} = Tree) ->
    find_additions2(Tree2, Tree);
diff_subdirs2([_|T], Tree) ->
    diff_subdirs2(T, Tree);
diff_subdirs2([], Tree) ->
    Tree.
	
%%--------------------------------------------------------------------
%% @doc Return a list of the files that are present in the tree.
%% @spec file_paths(Tree::tree()) -> [string()]
%% @end
%%--------------------------------------------------------------------
file_paths({file, FileName}) ->
    [FileName];
file_paths({dir, DirName, SubDirs}) ->
    lists:foldl(fun(SubDir, Acc) -> 
			[lists:flatten([DirName, "/", Path]) || Path <-  file_paths(SubDir)] ++ Acc
		end, [], SubDirs).
    
%%====================================================================
%% Internal functions
%%====================================================================

find_additions_test() ->
    TreeA = 
	{dir,"test_dir",
	 [{dir,"path_b",
           [{file,"file_b3"},
            {file,"file_b2"},
            {file,"file_b"},
            {dir,"dir_b1",[{file,"file_b1-1"}]}]},
	  {dir,"path_a",[{dir,"path_a1",[{file,"file_a1"}]}]}]},

    TreeB = 
	{dir,"test_dir2",
	 [
	  {dir,"path_c", [{file,"file_c"}]},
	  {dir,"path_b",
           [{file,"file_b4"},
            {file,"file_b3"},
            {file,"file_b2"},
            {file,"file_b"},
            {dir,"dir_b1",[{file,"file_b1-1"}]}]},
	  {dir,"path_a",[{dir,"path_a1",[{file,"file_a1"}]}]}]},

    Result = {dir,"test_dir2",
	      [
	       {dir,"path_c", [{file,"file_c"}]},
	       {dir,"path_b", [{file,"file_b4"}]}
	      ]
	     },
    
    ?assertMatch(Result, find_additions(TreeA, TreeB)).

file_paths_test() ->
    DiffTree = {dir,"test_dir",
		[
		 {dir,"path_c", [{file,"file_c"}]},
		 {dir,"path_b", [{file,"file_b4"}]}
		]
	       },

    Result = ["test_dir/path_b/file_b4","test_dir/path_c/file_c"],

    ?assertMatch(Result, file_paths(DiffTree)).
