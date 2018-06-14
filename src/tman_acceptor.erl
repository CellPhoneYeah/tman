-module(tman_acceptor).

-export([
         start_link/1
        ]).

-export([
         loop/1
        ]).

start_link(LSocket) ->
    Pid = spawn_link(?MODULE, loop, [LSocket]),
    {ok, Pid}.

loop(LSocket) ->
    case gen_tcp:accept(LSocket, infinity) of
        {ok, Socket} ->
            tman_conns:start_protocol(Socket),
            %% 等待连接管理进程回复，如果没有回复，则说明达到了最大连接量，acceptor进入睡眠
            receive
                ok ->
                    ok
            end;
        {error, Reason} ->
            io:format("error ~p", [Reason]),
            ok
    end,
    ?MODULE:loop(LSocket).
