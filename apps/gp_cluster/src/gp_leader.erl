%% Leader election using Erlang global module.
%% On becoming leader: starts delivery workers.
%% On losing leadership: stops delivery workers.
-module(gp_leader).
-behaviour(gen_server).

-export([start_link/0, is_leader/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(LEADER_NAME, gatepulse_leader).
-define(RETRY_INTERVAL_MS, 5000).

-record(state, {is_leader = false :: boolean()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

is_leader() ->
    gen_server:call(?MODULE, is_leader, 5000).

init([]) ->
    erlang:send_after(0, self(), try_elect),
    {ok, #state{}}.

handle_call(is_leader, _From, State) ->
    {reply, State#state.is_leader, State};
handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(try_elect, State) ->
    case global:register_name(?LEADER_NAME, self()) of
        yes ->
            logger:info(#{event => leader_elected, node => node()}),
            on_become_leader(),
            {noreply, State#state{is_leader = true}};
        no ->
            case global:whereis_name(?LEADER_NAME) of
                undefined ->
                    erlang:send_after(?RETRY_INTERVAL_MS, self(), try_elect);
                Pid ->
                    erlang:monitor(process, Pid)
            end,
            {noreply, State#state{is_leader = false}}
    end;

handle_info({'DOWN', _Ref, process, _Pid, _Reason}, State) ->
    logger:info(#{event => leader_died, node => node()}),
    on_lose_leadership(),
    erlang:send_after(0, self(), try_elect),
    {noreply, State#state{is_leader = false}};

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, State) ->
    case State#state.is_leader of
        true ->
            global:unregister_name(?LEADER_NAME),
            on_lose_leadership();
        false -> ok
    end.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

on_become_leader() ->
    case whereis(gp_delivery_sup) of
        undefined -> ok;
        _ -> catch gp_delivery_sup:start_workers()
    end.

on_lose_leadership() ->
    case whereis(gp_delivery_sup) of
        undefined -> ok;
        _ -> catch gp_delivery_sup:stop_workers()
    end.
