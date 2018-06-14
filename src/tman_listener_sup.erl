-module(tman_listener_sup).

-behaviour(supervisor).

-export([
         start_link/5,
         init/1
        ]).

start_link(Ref, AcceptorNum, TcpOpts, Protocol, ProtoOpts) ->
    MaxConns = proplists:get_value(max_conns, TcpOpts, 10),
    tman_server:add_listener(Ref, MaxConns, ProtoOpts),
    supervisor:start_link(?MODULE, {Ref, AcceptorNum, TcpOpts, Protocol}).

init({Ref, AcceptorNum, TcpOpts, Protocol}) ->
    SupFlag = #{
      strategy => rest_for_one,
      intensity => 10,
      period => 10
     },
    TmanConns = #{
      id => tman_conns,
      start => {tman_conns, start_link, [Ref, Protocol]},
      restart => permanent,
      shutdown => infinity,
      type => worker,
      modules => [tman_conns]
     },
    TmanAcceptorSup = #{
      id => tman_acceptor_sup,
      start => {tman_acceptor_sup, start_link, [Ref, AcceptorNum, TcpOpts]},
      restart => permanent,
      shutdown => infinity,
      type => supervisor,
      modules => [tman_acceptor_sup]
     },
    ChildSpecs = [TmanConns, TmanAcceptorSup],
    {ok, {SupFlag, ChildSpecs}}.
