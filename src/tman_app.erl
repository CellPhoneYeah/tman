-module(tman_app).

-export([
         start/2,
         stop/1
        ]).

start(_, _) ->
    tman_sup:start_link().

stop(_) ->
    ok.
