-module(hot_code_reload_app).
-behaviour(application).

-export([start/0, start/2, stop/1]).


start() ->
    application:start(hot_code_reload).

start(_StartType, _StartArgs) ->
    hot_code_reload_sup:start_link().

stop(_State) ->
    ok.
