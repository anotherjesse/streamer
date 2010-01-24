%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the streamer application.

-module(streamer_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2, stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for streamer.
start(_Type, _StartArgs) ->
    streamer_deps:ensure(),
    streamer_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for streamer.
stop(_State) ->
    ok.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
