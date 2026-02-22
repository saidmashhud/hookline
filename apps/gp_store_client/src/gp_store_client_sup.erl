-module(gp_store_client_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 10, period => 10},
    PoolSize = gp_config:get_int(<<"GP_STORE_POOL_SIZE">>, 8),
    PoolSpec = #{id => gp_store_pool,
                 start => {gp_store_pool, start_link, [PoolSize]},
                 restart => permanent,
                 shutdown => 5000,
                 type => worker},
    {ok, {SupFlags, [PoolSpec]}}.
