-module(gp_cluster_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        #{id => gp_leader,
          start => {gp_leader, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [gp_leader]}
    ],
    {ok, {#{strategy => one_for_one, intensity => 3, period => 5}, Children}}.
