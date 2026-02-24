-module(hl_api_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case validate_auth_config() of
        ok ->
            hl_api_h_apikeys:init_table(),
            hl_api_h_replay:init_table(),
            Port = hl_config:get_int(<<"HL_PORT">>, 8080),
            ListenAddr = hl_config:get_str(<<"HL_LISTEN_ADDR">>, "0.0.0.0"),
            Dispatch = hl_api_router:dispatch(),
            {ok, _} = cowboy:start_clear(
                http_listener,
                [{port, Port}, {ip, parse_addr(ListenAddr)}],
                #{env => #{dispatch => Dispatch},
                  middlewares => [cowboy_router, hl_api_auth, cowboy_handler]}
            ),
            logger:info("HookLine API listening on ~s:~p", [ListenAddr, Port]),
            hl_api_sup:start_link();
        {error, Reason} ->
            logger:error("Invalid auth config: ~p", [Reason]),
            {error, {invalid_auth_config, Reason}}
    end.

stop(_State) ->
    cowboy:stop_listener(http_listener),
    ok.

parse_addr(Addr) ->
    {ok, IP} = inet:parse_address(Addr),
    IP.

validate_auth_config() ->
    case hl_config:get_str("HL_AUTH_MODE", "api_key") of
        "service_token" ->
            require_nonempty("HL_SERVICE_TOKEN");
        "api_key" ->
            ApiKey = hl_config:get_str("HL_API_KEY", ""),
            AdminKey = hl_config:get_str("HL_ADMIN_KEY", ""),
            case ApiKey =/= "" orelse AdminKey =/= "" of
                true  -> ok;
                false -> {error, missing_api_or_admin_key}
            end;
        Mode ->
            {error, {unsupported_auth_mode, Mode}}
    end.

require_nonempty(Name) ->
    case hl_config:get_str(Name, "") of
        "" -> {error, {missing_env, Name}};
        _  -> ok
    end.
