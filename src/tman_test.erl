-module(tman_test).

-export([
         test/0
        ]).

test() ->
    tman:start_listener(test, 15, [{port, 8089}], echo_protocol, []).
