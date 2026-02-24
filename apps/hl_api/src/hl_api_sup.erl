-module(hl_api_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    KeyStore    = #{id      => hl_api_key_store,
                    start   => {hl_api_key_store, start_link, []},
                    restart => permanent,
                    type    => worker},
    TenantStore = #{id      => hl_tenant_store,
                    start   => {hl_tenant_store, start_link, []},
                    restart => permanent,
                    type    => worker},
    KeyRotation = #{id      => hl_key_rotation,
                    start   => {hl_key_rotation, start_link, []},
                    restart => permanent,
                    type    => worker},
    {ok, {SupFlags, [KeyStore, TenantStore, KeyRotation]}}.
