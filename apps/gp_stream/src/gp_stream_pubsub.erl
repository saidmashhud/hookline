-module(gp_stream_pubsub).
-behaviour(gen_server).

-export([start_link/0, publish/2, subscribe/3, unsubscribe/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(PG_SCOPE, gp_stream).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    pg:start_link(?PG_SCOPE),
    {ok, #{}}.

%% Publish an event to all SSE subscribers for a tenant
publish(TenantId, Event) ->
    Members = pg:get_members(?PG_SCOPE, TenantId),
    lists:foreach(fun(Pid) ->
        Pid ! {gp_stream, TenantId, Event}
    end, Members).

%% Subscribe this process to events for TenantId matching Pattern
subscribe(TenantId, _Pattern, Pid) ->
    pg:join(?PG_SCOPE, TenantId, Pid).

%% Unsubscribe
unsubscribe(TenantId, _Pattern, Pid) ->
    pg:leave(?PG_SCOPE, TenantId, Pid).

handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
