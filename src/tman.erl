-module(tman).

-export([
         start_listener/5
        ]).

start_listener(Ref, AcceptorNum, TcpOpts, Protocol, ProtoOpts)
  when is_integer(AcceptorNum) andalso is_atom(Protocol) ->
    supervisor:start_child(tman_sup, child_spec(Ref, AcceptorNum, TcpOpts, Protocol, ProtoOpts)).

child_spec(Ref, AcceptorNum, TcpOpts, Protocol, ProtoOpts) ->
    #{id => {tman_listener_sup, Ref}, 
      start => {tman_listener_sup, start_link, [Ref, AcceptorNum, TcpOpts, Protocol, ProtoOpts]},
      restart => permanent,
      shutdown => 5000,
      type => supervisor,
      modules => [tman_listener_sup]
     }.
