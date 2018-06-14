-module(tman_acceptor_sup).

-behaviour(supervisor).

-export([
         init/1,
         start_link/3
        ]).

start_link(Ref, AcceptorNum, TcpOpts) ->
    supervisor:start_link(?MODULE,[Ref, AcceptorNum, TcpOpts]).

init([Ref, AcceptorNum, TcpOpts]) ->
    {ok, LSocket} = gen_tcp:listen(0, check_opts(TcpOpts)),
    {ok, {_, Port}} = inet:sockname(LSocket),
    tman_server:set_port(Ref, Port),
    ChildSpecs = [
                  begin
                      #{
                    id => {acceptor, self(), N},
                    start => {tman_acceptor, start_link, [LSocket]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [tman_acceptor]
                   }
                  end || N <- lists:seq(1, AcceptorNum)],
    SupFlag = #{
      strategy => one_for_one,
      intensity => 10,
      period => 10},
    {ok, {SupFlag, ChildSpecs}}.

check_opts(Opts) ->
    Opts1 = case lists:member(backlog, Opts) of
        true ->
            Opts;
        false ->
            [{backlog, 1024} | Opts]
    end,
    filter_opts(Opts1).

filter_opts(Opts) ->
    filter_opts(Opts, 
                [backlog, ip, nodelay, port, raw],
                [binary, {active, false}, {packet, raw}, {reuseaddr, true}, {nodelay, true}]).

filter_opts([], _Member, Acc) ->
    Acc;
filter_opts([{Key, V} | Tail], Member, Acc) ->
    case lists:member(Key, Member) of
        true ->
            filter_opts(Tail, Member, [{Key, V} | Acc]);
        false ->
            filter_opts(Tail, Member, Acc)
    end;
filter_opts([{raw, _, _, _} = Opt | Tail], Member, Acc) ->
    case lists:member(raw, Member) of
        true ->
            filter_opts(Tail, Member, [Opt | Acc]);
        false ->
            filter_opts(Tail, Member, Acc)
    end.
