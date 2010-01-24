%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(streamer).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start() -> ok
%% @doc Start the streamer server.
start() ->
    streamer_deps:ensure(),
    ensure_started(crypto),
    application:start(streamer).

%% @spec stop() -> ok
%% @doc Stop the streamer server.
stop() ->
    Res = application:stop(streamer),
    application:stop(crypto),
    Res.
