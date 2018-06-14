-module(tman_protocol).

%% 必须启动一个进程来处理Socket的请求，并且Socket是{active, false}模式，所以读消息需要用到Transport:recv/2
-callback start_link(
            Socket :: any(),
            Transport :: module(),
            ProtocolOptions :: any())
-> {ok, ConnectionPid :: pid()}.
