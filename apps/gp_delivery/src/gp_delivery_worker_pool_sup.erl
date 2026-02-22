-module(gp_delivery_worker_pool_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Workers) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Workers]).

init([Workers]) ->
    SupFlags = #{strategy => simple_one_for_one, intensity => 100, period => 60},
    WorkerSpec = #{id      => gp_delivery_worker,
                   start   => {gp_delivery_worker, start_link, []},
                   restart => temporary,
                   shutdown => 5000,
                   type    => worker},
    %% Pre-start Workers idle workers
    Self = self(),
    spawn(fun() ->
        timer:sleep(500),
        [supervisor:start_child(gp_delivery_worker_pool_sup, []) || _ <- lists:seq(1, Workers)],
        _ = Self
    end),
    {ok, {SupFlags, [WorkerSpec]}}.
