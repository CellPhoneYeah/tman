-module(server).

-export([
         start/0
        ]).

start() ->
    case catch application:start(tman) of
        ok ->
            ok;
        _Reason ->
            error
    end.
