%% Periodic compaction of old store segments.
%% Runs every GP_COMPACT_INTERVAL_SECS (default: 3600s = 1h).
%% Deletes segments older than GP_RETENTION_DAYS (default: 7 days).
-module(gp_compaction).
-behaviour(gen_server).

-export([start_link/0, run/0, stats/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(INTERVAL_MS,     3_600_000).   %% 1 hour
-define(RETENTION_SECS,  7 * 86_400).  %% 7 days

run() ->
    gen_server:cast(?MODULE, run_now).

stats() ->
    gen_server:call(?MODULE, stats).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    schedule(),
    {ok, #{last_run_at => 0, segments_deleted => 0, duration_ms => 0}}.

handle_call(stats, _From, State) ->
    {reply, State, State};
handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(run_now, State) ->
    NewState = do_compact(State),
    {noreply, NewState};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(compact, State) ->
    NewState = do_compact(State),
    schedule(),
    {noreply, NewState};

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

schedule() ->
    IntervalMs = gp_config:get_int("GP_COMPACT_INTERVAL_MS", ?INTERVAL_MS),
    erlang:send_after(IntervalMs, self(), compact).

do_compact(State) ->
    RetentionSecs = gp_config:get_int("GP_RETENTION_SECS", ?RETENTION_SECS),
    T0 = erlang:monotonic_time(millisecond),
    Deleted = case gp_store_client:compact(RetentionSecs) of
        {ok, #{<<"segments_deleted">> := N}} ->
            logger:info(#{event => compaction_done, segments_deleted => N,
                          retention_secs => RetentionSecs}),
            N;
        {error, Reason} ->
            logger:warning(#{event => compaction_failed, reason => Reason}),
            0
    end,
    DurationMs = erlang:monotonic_time(millisecond) - T0,
    State#{
        last_run_at      => erlang:system_time(millisecond),
        segments_deleted => Deleted,
        duration_ms      => DurationMs
    }.
