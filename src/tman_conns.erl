-module(tman_conns).

-behaviour(gen_server).

-export([
         start_link/2,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-export([
         start_protocol/1
        ]).

-record(conns_state, {
          parent,           % 监视进程
          ref,              % 应用名
          protocol,         % 回调协议模块
          opts,             % 协议参数
          current_conns,    % 当前连接数量
          children_num,     % 工作进程数量(包含睡眠中的工作进程)
          sleepers,         % 处于休眠状态的acceptor进程
          max_conns}).      % 最大连接数量

%%% ================================
%%% API
%%% ================================
start_protocol(Socket) ->
    gen_server:cast(?MODULE, {start_protocol, self(), Socket}).

%%% ================================
%%% 内部函数
%%% ================================
do_handle_call(Request, State) ->
    io:format("bad request ~p~n", [Request]),
    {ok, ok, State}.

do_handle_cast({start_protocol, SocketHandler, Socket}, State) ->
    #conns_state{
        current_conns = CurrentConns,
        max_conns = MaxConns,
        children_num = ChildrenNum,
        sleepers = Sleepers,
        protocol = Protocol,
        opts = Opts} = State,
    {ok, Pid} = Protocol:start_link(Socket, Opts),
    gen_tcp:controlling_process(Socket, Pid),
    NewChildrenNum = ChildrenNum + 1,
    case NewChildrenNum > MaxConns of
        true ->
            NewSleepers = [Pid | Sleepers],
            NewConns = CurrentConns;
        false ->
            NewSleepers = Sleepers,
            NewConns = CurrentConns + 1,
            SocketHandler ! ok
    end,
    io:format("NewConns ~p,ChildrenNum ~p, Sleepers ~p~n", [NewConns, NewChildrenNum, NewSleepers]),
    NewState = State#conns_state{
           current_conns = NewConns,
           children_num = NewChildrenNum,
           sleepers = NewSleepers},
    {ok, NewState};
    
do_handle_cast(Request, State) ->
    io:format("bad request ~p~n", [Request]),
    {ok, State}.

do_handle_info(Request, State) ->
    io:format("bad request ~p~n", [Request]),
    {ok, State}.

%%% ================================
%%% 回调
%%% ================================
start_link(Ref, Protocol) ->
    io:format("start tman_conns~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, {self(), Ref, Protocol}, []).

init({Parent, Ref, Protocol}) ->
    tman_server:set_conns_server(Ref, self()),
    MaxConns = tman_server:get_max_conns(Ref),
    Opts = tman_server:get_protocol_opts(Ref),
    State = #conns_state{
               parent = Parent,
               ref = Ref,
               protocol = Protocol,
               opts = Opts,
               current_conns = 0,
               children_num = 0,
               sleepers = [],
               max_conns = MaxConns},
    {ok, State}.

handle_call(Request, _From, State) ->
    case catch do_handle_call(Request, State) of
        {ok, Reply, NewState} ->
            {reply, Reply, NewState};
        Reason ->
            io:format("unexpected ~p~n", [Reason]),
            {reply, ok, State}
    end.

handle_cast(Request, State) ->
    case catch do_handle_cast(Request, State) of
        {ok, NewState} ->
            {noreply, NewState};
        Reason ->
            io:format("unexpected ~p~n", [Reason]),
            {noreply, ok, State}
    end.

handle_info(Request, State) ->
    case catch do_handle_info(Request, State) of
        {ok, NewState} ->
            {noreply, NewState};
        Reason ->
            io:format("unexpected ~p~n", [Reason]),
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
