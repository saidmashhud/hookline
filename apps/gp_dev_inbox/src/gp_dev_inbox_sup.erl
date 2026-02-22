-module(gp_dev_inbox_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    InboxSpec = #{id => gp_dev_inbox,
                  start => {gp_dev_inbox, start_link, []},
                  restart => permanent,
                  shutdown => 5000,
                  type => worker},
    {ok, {SupFlags, [InboxSpec]}}.
