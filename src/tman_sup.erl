-module(tman_sup).

-include("tman.hrl").

-export([
         init/1,
         start_link/0
        ]).

-behaviour(supervisor).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlag = #{
      strategy => one_for_one,
      intensity => 10,
      period => 1
     },

    TmanServer = #{
      id => tman_server,
      start => {tman_server, start_link, []},
      restart => permanent,
      shutdown => 30000,
      type => worker,
      modules => [tman_server]
     },
    ChildSpec = [TmanServer],
    init_ets(),
    {ok, {SupFlag, ChildSpec}}.

init_ets() ->
    ets:new(?ETS_TMAN, [set, named_table, public, {keypos, 1}]).
