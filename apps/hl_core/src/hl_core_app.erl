-module(hl_core_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Sup = hl_core_sup:start_link(),
    %% Deferred: wait 2 s for all apps (including hl_dev_inbox, hl_delivery)
    %% to fully start before checking/seeding.
    erlang:spawn(fun() ->
        timer:sleep(2000),
        first_run()
    end),
    Sup.

stop(_State) ->
    ok.

%% ── First-run detection ──────────────────────────────────────────────────────

first_run() ->
    TenantId = list_to_binary(hl_config:get_str("HL_TENANT_ID", "default")),
    case hl_store_client:list_endpoints(TenantId) of
        {ok, #{<<"items">> := []}} ->
            %% Store is empty — seed demo data and print welcome banner
            Token = seed_demo_data(TenantId),
            print_welcome(Token);
        _ ->
            ok
    end.

seed_demo_data(TenantId) ->
    Token = hl_core_uuid:generate_str(),
    Port  = hl_config:get_int("HL_PORT", 8080),
    InboxURL = iolist_to_binary([
        "http://localhost:", integer_to_binary(Port),
        "/v1/dev/inbox/receive/", Token
    ]),

    %% Register the inbox token (hl_dev_inbox may be in a different app;
    %% use catch so we don't crash if it's not loaded yet).
    catch hl_dev_inbox:create(Token),

    %% Create demo endpoint in the store
    EpId = hl_core_uuid:generate_str(),
    Now  = erlang:system_time(millisecond),
    Ep = #{
        <<"id">>          => EpId,
        <<"endpoint_id">> => EpId,
        <<"tenant_id">>   => TenantId,
        <<"url">>         => InboxURL,
        <<"name">>        => <<"Demo Inbox">>,
        <<"enabled">>     => true,
        <<"created_at">>  => Now
    },
    hl_store_client:put_endpoint(#{
        <<"endpoint_id">> => EpId,
        <<"payload">>     => jsx:encode(Ep),
        <<"tenant_id">>   => TenantId
    }),

    %% Create subscription for all topics
    SubId = hl_core_uuid:generate_str(),
    Sub = #{
        <<"id">>              => SubId,
        <<"subscription_id">> => SubId,
        <<"tenant_id">>       => TenantId,
        <<"endpoint_id">>     => EpId,
        <<"topic_pattern">>   => <<"#">>,
        <<"created_at">>      => Now
    },
    hl_store_client:put_subscription(Sub),

    %% Keep ETS caches consistent (ignore errors if not yet started)
    catch hl_subscription_cache:add(Sub),
    catch hl_tenant_manager:start_actor(Ep),

    Token.

print_welcome(Token) ->
    ApiKey = case hl_config:get_str("HL_API_KEY", "") of
        "" ->
            case hl_config:get_str("HL_ADMIN_KEY", "") of
                ""       -> "<set HL_API_KEY or HL_ADMIN_KEY>";
                AdminKey -> AdminKey
            end;
        Key ->
            Key
    end,
    Port   = hl_config:get_int("HL_PORT", 8080),
    io:format(
        "~n=======================================================~n"
        "  HookLine is running!~n~n"
        "  Console:    http://localhost:~B/console~n"
        "  API:        http://localhost:~B/v1~n"
        "  API Key:    ~s~n~n"
        "  Demo inbox ready. Publish your first event:~n~n"
        "  curl -X POST http://localhost:~B/v1/events \\~n"
        "    -H \"Authorization: Bearer ~s\" \\~n"
        "    -H \"Content-Type: application/json\" \\~n"
        "    -d '{\"topic\":\"hello.world\",\"payload\":{\"msg\":\"it works\"}}'~n~n"
        "  View deliveries at: http://localhost:~B/v1/dev/inbox/messages?token=~s~n"
        "  Or open:           http://localhost:~B/console~n"
        "=======================================================~n~n",
        [Port, Port, ApiKey, Port, ApiKey, Port, Token, Port]
    ).
