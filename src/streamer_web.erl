%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for streamer.

-module(streamer_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    router:start_link(),
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
            Response:write_chunk("{\"ok\": \"subscribed\"}\n"),
            router:listen(self(), Path, Response);
        'POST' ->
            Json = mochijson:encode({struct, Req:parse_post()}),
            router:send(Path, Json),
            Req:ok({"text/plain", "kthxbai"});
        _ ->
            Req:respond({501, [], []})
    end.

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
