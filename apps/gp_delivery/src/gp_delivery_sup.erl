-module(gp_delivery_sup).
-behaviour(supervisor).

-export([start_link/0, init/1, start_workers/0, stop_workers/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 10, period => 60},

    Workers = gp_config:get_int(<<"GP_DELIVERY_WORKERS">>, 16),

    Poller = #{id      => gp_delivery_poller,
               start   => {gp_delivery_poller, start_link, []},
               restart => permanent,
               shutdown => 5000,
               type    => worker},

    WorkerPool = #{id      => gp_delivery_worker_pool_sup,
                   start   => {gp_delivery_worker_pool_sup, start_link, [Workers]},
                   restart => permanent,
                   shutdown => infinity,
                   type    => supervisor},

    Compaction = #{id      => gp_compaction,
                   start   => {gp_compaction, start_link, []},
                   restart => permanent,
                   shutdown => 5000,
                   type    => worker},

    {ok, {SupFlags, [Poller, WorkerPool, Compaction]}}.

%% Start delivery poller and worker pool (called by leader node).
start_workers() ->
    case supervisor:start_child(?MODULE, #{
        id      => gp_delivery_poller,
        start   => {gp_delivery_poller, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type    => worker}) of
        {ok, _} -> ok;
        {error, already_present} ->
            supervisor:restart_child(?MODULE, gp_delivery_poller);
        {error, {already_started, _}} -> ok;
        Err -> Err
    end.

%% Stop delivery poller and worker pool (called when losing leadership).
stop_workers() ->
    _ = supervisor:terminate_child(?MODULE, gp_delivery_poller),
    _ = supervisor:delete_child(?MODULE, gp_delivery_poller),
    ok.
