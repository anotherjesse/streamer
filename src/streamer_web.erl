%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for streamer.

-module(streamer_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    Pid = spawn(fun() -> manager() end),
    register(manager, Pid),
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            Response = Req:ok({"text/html; charset=utf-8",
                              [{"Server","Mochiweb-Test"}],
                              chunked}),
            Response:write_chunk("You are subscribed to: " ++ Path ++ "\n"),
            manager ! {connect, Path, Response};
        'POST' ->
            % FIXME: grab form post data and convert to json
            manager ! {send, Path, "hi"},
            Req:ok({"text/plain", "kthxbai"});
        _ ->
            Req:respond({501, [], []})
    end.

%% using a process dictionary instead of an ETS table
%% see: http://stackoverflow.com/questions/1483550/
get_or_spawn(Path) ->
    Pid = erlang:get(Path),
    case Pid of
        undefined ->
            io:format("Spawning new feeder for ~p~n", [Path]),
            NewPid = spawn(fun() -> feeder([]) end),
            io:format("Pid is ~p~n", [NewPid]),
            erlang:put(Path, NewPid),
            NewPid;
        _ ->
            Pid
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.


manager() ->
    receive
        {connect, Path, Response} ->
            Pid = get_or_spawn(Path),
            Pid ! {connect, Response};
        {send, Path, Data} ->
            Pid = get_or_spawn(Path),
            Pid ! {data, Data}
    end,
    manager().


feeder(Connections) ->
    receive
        {connect, Connection} ->
            io:format("User connected: ~w~n", [Connection]),
            NewConns = [Connection | Connections];
        {disconnect, Connection} ->
            io:format("User disconnected: ~w~n", [Connection]),
            NewConns = lists:delete(Connection, Connections);
        {data, Data} ->
            send_data(Connections, Data),
            NewConns = Connections
    end,
    feeder(NewConns).

send_data(Connections, Data) ->
    SendData = fun(Response) ->
        Response:write_chunk(Data ++ "\n")
    end,
    lists:foreach(SendData, Connections).


feed(Response, Path, N) ->
    receive
        %{router_msg, Msg} ->
        %    Html = io_lib:format("Recvd msg #~w: ‘~s’<br/>", [N, Msg]),
        %    Response:write_chunk(Html);
    after 10000 ->
        Msg = io_lib:format("Chunk ~w for path ~s\n", [N, Path]),
        Response:write_chunk(Msg)
    end,
    feed(Response, Path, N+1).

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
