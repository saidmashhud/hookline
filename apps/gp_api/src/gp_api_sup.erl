-module(gp_api_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    KeyStore = #{id      => gp_api_key_store,
                 start   => {gp_api_key_store, start_link, []},
                 restart => permanent,
                 type    => worker},
    {ok, {SupFlags, [KeyStore]}}.
