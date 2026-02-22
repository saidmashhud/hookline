-module(gp_api_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    gp_api_h_apikeys:init_table(),
    gp_api_h_replay:init_table(),
    Port = gp_config:get_int(<<"GP_PORT">>, 8080),
    ListenAddr = gp_config:get_str(<<"GP_LISTEN_ADDR">>, "0.0.0.0"),
    Dispatch = gp_api_router:dispatch(),
    {ok, _} = cowboy:start_clear(
        http_listener,
        [{port, Port}, {ip, parse_addr(ListenAddr)}],
        #{env => #{dispatch => Dispatch},
          middlewares => [cowboy_router, gp_api_auth, cowboy_handler]}
    ),
    logger:info("GatePulse API listening on ~s:~p", [ListenAddr, Port]),
    gp_api_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(http_listener),
    ok.

parse_addr(Addr) ->
    {ok, IP} = inet:parse_address(Addr),
    IP.
