-module(gp_delivery_poller).
-behaviour(gen_server).

-export([start_link/0, pause/0, resume/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(POLL_MS, 1000).
-define(CLAIM_COUNT, 20).
-define(LEASE_SECS, 30).

-record(state, {timer :: reference() | undefined, paused = false :: boolean()}).

pause() ->
    gen_server:call(?MODULE, pause).

resume() ->
    gen_server:call(?MODULE, resume).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Timer = schedule_poll(),
    {ok, #state{timer = Timer}}.

handle_call(pause, _From, State) ->
    {reply, ok, State#state{paused = true}};
handle_call(resume, _From, State) ->
    {reply, ok, State#state{paused = false}};
handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(poll, #state{paused = true} = State) ->
    Timer = schedule_poll(),
    {noreply, State#state{timer = Timer}};
handle_info(poll, State) ->
    do_poll(),
    Timer = schedule_poll(),
    {noreply, State#state{timer = Timer}};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

schedule_poll() ->
    erlang:send_after(?POLL_MS, self(), poll).

do_poll() ->
    case gp_store_client:claim_jobs(?CLAIM_COUNT, ?LEASE_SECS) of
        {ok, #{<<"jobs">> := Jobs}} when Jobs =/= [] ->
            lists:foreach(fun(Job) ->
                supervisor:start_child(gp_delivery_worker_pool_sup, [Job])
            end, Jobs);
        _ ->
            ok
    end.
