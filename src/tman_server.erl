-module(tman_server).

-include("tman.hrl").

-export([
         start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).
-export([
         add_listener/3,
         clean_listen/1,
         set_conns_server/2,
         get_conns_server/1,
         set_port/2,
         get_port/1,
         set_max_conns/2,
         get_max_conns/1,
         set_protocol_opts/2,
         get_protocol_opts/1
        ]).

-behaviour(gen_server).

%% 每个监听进程所必须的信息有max_conns, opts, conns_sup, port, protocol_opts

%%% =====================================
%%% API
%%% =====================================
% 添加监听进程
add_listener(Ref, MaxConns, Opts) ->
    io:format("add_listener~n"),
    gen_server:cast(?MODULE, {add_listener, Ref, MaxConns, Opts}).
clean_listen(Ref) ->
    ets:delete(?ETS_TMAN, {port, Ref}),
    ets:delete(?ETS_TMAN, {max_conns, Ref}),
    ets:delete(?ETS_TMAN, {opts, Ref}),
    ok.
% 设置连接管理进程
set_conns_server(Ref, Pid) ->
    io:format("set_conns_server~n"),
    gen_server:cast(?MODULE, {set_conns_server, Ref, Pid}),
    ok.
% 获取监听管理进程pid
get_conns_server(Ref) ->
    io:format("get_conns_server~n"),
    ets:lookup_element(?ETS_TMAN, {conns_server, Ref}, 2).
% 设置监听端口
set_port(Ref, Port) ->
    io:format("set_port~n"),
    gen_server:cast(?MODULE, {set_port, Ref, Port}),
    ok.
% 获取监听的端口号
get_port(Ref) ->
    io:format("get_port~n"),
    ets:lookup_element(?ETS_TMAN, {port, Ref}, 2).
% 设置最大连接数量
set_max_conns(Ref, MaxConns) ->
    io:format("set_max_conns~n"),
    gen_server:cast(?MODULE, {set_max_conns, Ref, MaxConns}),
    ok.
% 获取最大连接数量
get_max_conns(Ref) ->
    io:format("get_max_conns~n"),
    ets:lookup_element(?ETS_TMAN, {max_conns, Ref}, 2).
% 设置回调协议的参数
set_protocol_opts(Ref, ProtoOpts) ->
    io:format("set_protocol_opts~n"),
    gen_server:cast(?MODULE, {set_protocol_opts, Ref, ProtoOpts}),
    ok.
% 获取回调协议的参数
get_protocol_opts(Ref) ->
    io:format("get_protocol_opts~n"),
    ets:lookup_element(?ETS_TMAN, {protocol_opts, Ref}, 2).
%%% =====================================
%%% 内部
%%% =====================================
do_handle_call(_, State) ->
    {ok, none, State}.

do_handle_cast({add_listener, Ref, MaxConns, Opts}, State) ->
    io:format("do_add_listener"),
    ets:insert(?ETS_TMAN, {{max_conns, Ref}, MaxConns}),
    ets:insert(?ETS_TMAN, {{protocol_opts, Ref}, Opts}),
    {ok, State};
do_handle_cast({set_conns_server, Ref, Pid}, State) ->
    ets:insert(?ETS_TMAN, {{conns_server, Ref}, Pid}),
    {ok, State};
do_handle_cast({set_port, Ref, Port}, State) ->
    ets:insert(?ETS_TMAN, {{port, Ref}, Port}),
    {ok, State};
do_handle_cast({set_max_conns, Ref, MaxConns}, State) ->
    ets:insert(?ETS_TMAN, {{max_conns, Ref}, MaxConns}),
    {ok, State};
do_handle_cast(Request, State) ->
    io:format("unknow mod ~p line ~p request ~p~n", [?MODULE, ?LINE, Request]),
    {ok, State}.

do_handle_info(Request, State) ->
    io:format("unknow request ~p", [Request]),
    {ok, State}.
%%% =====================================
%%% 回调
%%% =====================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("tman_server start ~n"),
    {ok, []}.

handle_call(Request, _From, State) ->
    case catch do_handle_call(Request, State) of
        {ok, Reply, State} ->
            {reply, Reply, State};
        Reason ->
            io:format("error ~p", [Reason]),
            {reply, error, State}
    end.

handle_cast(Request, State) ->
    case catch do_handle_cast(Request, State) of
        {ok, NewState} ->
            {noreply, NewState};
        Reason ->
            io:format("unexpected ~p", [Reason]),
            {noreply, State}
    end.

handle_info(Request, State) ->
    case catch do_handle_info(Request, State) of
        {ok, NewState} ->
            {noreply, NewState};
        Reason ->
            io:format("unexpected ~p", [Reason]),
            {noreply, State}
    end.

terminate(_State, _Reason) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


