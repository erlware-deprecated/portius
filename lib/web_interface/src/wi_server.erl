%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2010, Martin Logan
%%% @doc 
%%%  
%%% @end
%%% Created : 11 Feb 2010 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(wi_server).

-behaviour(gen_web_server).

%% API
-export([start_link/0]).

%% callbacks
-export([
	 init/1,
	 head/3,
	 get/3,
	 delete/3,
	 options/4,
	 post/4,
	 put/4,
	 trace/4,
	 connect/4,
	 other_methods/4
	]).

-record(state, {document_root}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc starts the server
%% @spec () -> {ok, pid()}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    {ok, IP} = gas:get_env(web_interface, ip, default_ip),
    {ok, Port} = gas:get_env(web_interface, port, 8080),
    {ok, DocumentRoot} = gas:get_env(web_interface, document_root, "/tmp/repo"),
    case Port of
	Port when is_list(Port) ->
	    gen_web_server:start_link(?MODULE, IP, list_to_integer(Port), DocumentRoot);
	Port ->
	    gen_web_server:start_link(?MODULE, IP, Port, DocumentRoot)
    end.
	    

%%%===================================================================
%%% gen_web_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc 
%% @spec (UserArgs) -> void()
%% @end
%%--------------------------------------------------------------------
init(DocumentRoot) ->
    {ok, #state{document_root = DocumentRoot}}.

%%--------------------------------------------------------------------
%% @doc
%% @spec (RequestLine, Headers, State) -> Response
%% @end
%%--------------------------------------------------------------------
get({http_request, M, {abs_path, <<"/">>}, O}, Headers, State) ->
    get({http_request, M, {abs_path, <<"/index.html">>}, O}, Headers, State);
get({http_request, _, {abs_path, AbsPathBin}, _}, Headers, State) ->
    AbsPath = binary_to_list(AbsPathBin),
    RawFilePath = filename:join(State#state.document_root, string:strip(AbsPath, left, $\/)),
    case filelib:is_dir(RawFilePath) of
	true ->
	    IndexFile = filename:join(RawFilePath, "index.html"),
	    case filelib:is_file(IndexFile) of
		true  ->
		    get_a_file(IndexFile, Headers);
		false -> 
		    get_a_directory(RawFilePath, AbsPath, Headers)
	    end;
	false ->
	    get_a_file(RawFilePath, Headers)
    end.
    
	    
head(_RequestLine, _Headers, _State) -> gen_web_server:http_reply(200).
delete(_RequestLine, _Headers, _State) -> gen_web_server:http_reply(200).

%%--------------------------------------------------------------------
%% @doc
%% @spec (RequestLine, Headers, Body, State) -> Response
%% @end
%%--------------------------------------------------------------------
put({http_request, _, {abs_path, AbsPathBin}, _}, _Headers, Body, State) ->
    AbsPath = binary_to_list(AbsPathBin),
    To = filename:join(State#state.document_root, string:strip(AbsPath, left, $\/)),
    case catch write_data(Body, To) of
	ok ->
	    gen_web_server:http_reply(201);
	Error ->
	    error_logger:info_msg("failed to write data to ~p with error ~p~n", [To, Error]),
	    gen_web_server:http_reply(405)
    end.
trace(_RequestLine, _Headers, _Body, _State) -> gen_web_server:http_reply(200).
post(_RequestLine, _Headers, _Body, _State) -> gen_web_server:http_reply(200).
options(_RequestLine, _Headers, _Body, _State) -> gen_web_server:http_reply(200).
connect(_RequestLine, _Headers, _Body, _State) -> gen_web_server:http_reply(200).

%%--------------------------------------------------------------------
%% @doc
%% @spec (RequestLine, Headers, Body, State) -> Response
%% @end
%%--------------------------------------------------------------------
other_methods({http_request, <<"PROPFIND">>, {abs_path, AbsPathBin}, _}, Headers, _Body, State) ->
    AbsPath = binary_to_list(AbsPathBin),
    {value, {'Host', Host}} = lists:keysearch('Host', 1, Headers),
    case gws_web_dav_util:propfind(State#state.document_root, AbsPath, Host, 1) of
	error -> 
	    gen_web_server:http_reply(404);
	Resp -> 
	    gen_web_server:http_reply(207, Headers, Resp)
    end;
other_methods({http_request, <<"MKCOL">>, {abs_path, AbsPathBin}, _}, _Headers, _Body, State) ->
    AbsPath = binary_to_list(AbsPathBin),
    gws_web_dav_util:mkcol(State#state.document_root, AbsPath),
    gen_web_server:http_reply(201);
other_methods(RequestLine, Headers, Body, _State) ->
    error_logger:info_msg("request is ~p ~p ~p~n", [RequestLine, Headers, Body]),
    gen_web_server:http_reply(200).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%-------------------------------------------------------------------
%% @spec write_data(Data, Location) -> ok | {error, Reason}
%% @doc
%%  Write the data to the specified location.
%% @end
%% @private
%%-------------------------------------------------------------------
write_data(Data, To) ->
    case file:open(To, [write, raw]) of
        {ok, Fd} ->
	    error_logger:info_msg("write_data writing to ~p~n", [To]),
            ok = file:write(Fd, Data),
            file:close(Fd),
            ok;
        {error, Reason} ->
            throw({file_open_error, Reason})
    end.


get_a_directory(DirPath, AbsPath, Headers) ->
    {value, {'Host', Host}} = lists:keysearch('Host', 1, Headers),
    DirList = create_directory_listing_html(binary_to_list(Host), DirPath, AbsPath),
    gen_web_server:http_reply(200, Headers, DirList).

create_directory_listing_html(Host, DirPath, AbsPath) ->
    {ok, Dirs} = file:list_dir(DirPath),
    Links = 
	lists:map(fun(Dir) ->
			  case filelib:is_dir(filename:join(DirPath, Dir)) of
			      true ->
				  lists:concat(["<a href=\"http://", Host, AbsPath, "/", Dir, "\/\">", Dir, "</a>"]);
			      false ->
				  lists:concat(["<a href=\"http://", Host, AbsPath, "/", Dir, "\">", Dir, "</a>"])
			  end
		  end, Dirs),
    string:join(Links, "<br/>").
    
get_a_file(FilePath, Headers) ->
    error_logger:info_msg("fetching package at ~p ~p~n", [FilePath, Headers]),
    case catch file:read_file(FilePath) of
	{ok, File} ->
	    gen_web_server:http_reply(200, Headers, File);
	_Error ->
	    gen_web_server:http_reply(404)
    end.
