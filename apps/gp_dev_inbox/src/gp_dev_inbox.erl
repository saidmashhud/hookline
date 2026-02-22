-module(gp_dev_inbox).
-behaviour(gen_server).

-export([start_link/0, create/1, store/2, list/1, delete/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TABLE, gp_dev_inbox_ets).
-define(MAX_MESSAGES_PER_INBOX, 100).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(?TABLE, [named_table, public, {write_concurrency, true}, bag]),
    {ok, #{}}.

create(Token) ->
    %% Just register the token (messages arrive via store/2)
    ets:insert(?TABLE, {Token, created, erlang:system_time(millisecond)}).

store(Token, Message) ->
    %% Enforce max messages
    Count = length(ets:lookup(?TABLE, {msg, Token})),
    if
        Count >= ?MAX_MESSAGES_PER_INBOX ->
            %% Drop oldest (FIFO eviction)
            [{_, _, OldestKey} | _] = lists:sort(ets:lookup(?TABLE, {msg, Token})),
            ets:delete(?TABLE, {msg, Token, OldestKey});
        true -> ok
    end,
    Key = erlang:system_time(nanosecond),
    KeyBin = integer_to_binary(Key),
    ets:insert(?TABLE, {{msg, Token, Key}, Message#{<<"id">> => KeyBin}}).

list(Token) ->
    Entries = ets:match_object(?TABLE, {{msg, Token, '_'}, '_'}),
    Sorted = lists:sort(fun({{msg, _, K1}, _}, {{msg, _, K2}, _}) -> K1 =< K2 end, Entries),
    [Msg || {_, Msg} <- Sorted].

delete(Token) ->
    ets:match_delete(?TABLE, {{msg, Token, '_'}, '_'}),
    ets:match_delete(?TABLE, {Token, created, '_'}).

handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
