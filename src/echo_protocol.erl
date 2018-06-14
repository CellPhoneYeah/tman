-module(echo_protocol).

-export([
        start_link/2,
        init/2
       ]).

start_link(Socket, Opts) ->
    Pid = spawn_link(?MODULE, init, [Socket, Opts]),
    {ok, Pid}.

init(Socket, _Opts = []) ->
    loop(Socket).

loop(Socket) ->
    io:format("start loop"),
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, Data} ->
            io:format("Data ~p~n", [Data]),
            gen_tcp:send(Socket, Data),
            loop(Socket);
        _ ->
            io:format("close"),
            ok = gen_tcp:close(Socket)
    end.
