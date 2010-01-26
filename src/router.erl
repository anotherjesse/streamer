-module(router).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).
 
-export([send/2, listen/3]).

-define(SERVER, global:whereis_name(?MODULE)).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init([]) ->
    % set this so we can catch death of logged in pids:
    process_flag(trap_exit, true),
    % use ets for routing tables
    {ok, running}.

handle_call({listen, Pid, Path, Response}, _From, State) when is_pid(Pid) ->
    io:format("~w listening on ~s\n", [Pid, Path]),
    % link(Pid),
    add_to_channel(Path, Pid, Response),
    {reply, ok, State};


handle_call({send, Path, Json}, _From, State) ->
    io:format("send ~s to ~s\n", [Json, Path]),
    send_to_channel(Path, Json),
    {reply, ok, State};

handle_call({logout, Pid}, _From, State) when is_pid(Pid)->
    io:format("stop listening on ~w\n", [Pid]),
    % remove_from_channel(Pid),
    {reply, ok, State}.
    
listen(Pid, Path, Response) when is_pid(Pid) ->
    gen_server:call(?SERVER, {listen, Pid, Path, Response}).

send(Path, Json) ->
    gen_server:call(?SERVER, {send, Path, Json}).

% using a process dictionary instead of an ETS table
% see: http://stackoverflow.com/questions/1483550/
get_or_spawn(Channel) ->
    case erlang:get(Channel) of
        undefined ->
            ChannelPid = spawn(fun() -> channel(Channel) end),
            erlang:put(Channel, ChannelPid),
            ChannelPid;
        Pid ->
            Pid
    end.


add_to_channel(Path, Pid, Response) ->
    ChannelPid = get_or_spawn(Path),
    ChannelPid ! {connect, Pid, Response}.

send_to_channel(Path, Json) ->
    ChannelPid = get_or_spawn(Path),
    ChannelPid ! {send, Json}.

remove_from_channel(Path, Pid) ->
    ChannelPid = get_or_spawn(Path),
    ChannelPid ! {disconnect, Pid}.

channel(Path) ->
    process_flag(trap_exit, true),
    io:format("New Channel for ~w~n", [Path]),
    feeder([]).

feeder(Connections) ->
    receive
        {connect, Pid, Response} ->
            link(Pid),
            io:format("User connected: ~w~n", [Pid]),
            NewConns = [{Pid, Response} | Connections];
        {send, Data} ->
            send_data(Connections, Data),
            NewConns = Connections;
        {'EXIT', Pid, _Why} ->
            io:format("User disconnected: ~w~n", [Pid]),
            NewConns = proplists:delete(Pid, Connections)
    end,
    feeder(NewConns).

send_data(Connections, Data) ->
    SendData = fun({_Pid, Response}) ->
        Response:write_chunk(Data ++ "\n")
    end,
    lists:foreach(SendData, Connections).


handle_info(Info, State) ->
    case Info of
        Wtf ->
            io:format("Caught unhandled message: ~w\n", [Wtf])
    end,
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
    
terminate(_Reason, _State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
