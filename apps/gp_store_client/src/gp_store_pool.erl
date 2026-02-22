-module(gp_store_pool).
-behaviour(gen_server).

-export([start_link/1, call/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(RECV_TIMEOUT, 5000).

-record(state, {
    socket_path :: string(),
    pool        :: queue:queue(),
    size        :: pos_integer(),
    pending     :: [{reference(), pid()}]
}).

start_link(PoolSize) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [PoolSize], []).

%% Send a pre-encoded frame; returns decoded response map
call(Frame) ->
    gen_server:call(?MODULE, {call, Frame}, 10000).

init([PoolSize]) ->
    SocketPath = gp_config:get_str(<<"GP_STORE_SOCKET">>, "/tmp/gp_store.sock"),
    Conns = connect_all(SocketPath, PoolSize, []),
    {ok, #state{
        socket_path = SocketPath,
        pool        = queue:from_list(Conns),
        size        = length(Conns),
        pending     = []
    }}.

connect_all(_Path, 0, Acc) -> Acc;
connect_all(Path, N, Acc) ->
    case gen_tcp:connect({local, Path}, 0,
                         [binary, {packet, 0}, {active, false},
                          {send_timeout, 5000}]) of
        {ok, Sock} -> connect_all(Path, N-1, [Sock | Acc]);
        {error, _}  ->
            timer:sleep(500),
            connect_all(Path, N-1, Acc)
    end.

handle_call({call, Frame}, _From, #state{pool = Pool} = State) ->
    case queue:out(Pool) of
        {{value, Sock}, Pool2} ->
            Result = do_call(Sock, Frame),
            {reply, Result, State#state{pool = queue:in(Sock, Pool2)}};
        {empty, _} ->
            {reply, {error, pool_empty}, State}
    end;
handle_call(_Req, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, #state{pool = Pool}) ->
    lists:foreach(fun gen_tcp:close/1, queue:to_list(Pool)),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

do_call(Sock, Frame) ->
    case gen_tcp:send(Sock, Frame) of
        ok ->
            recv_response(Sock);
        {error, Reason} ->
            {error, Reason}
    end.

recv_response(Sock) ->
    case gen_tcp:recv(Sock, 4, ?RECV_TIMEOUT) of
        {ok, <<Len:32/big>>} ->
            case gen_tcp:recv(Sock, Len, ?RECV_TIMEOUT) of
                {ok, Body} ->
                    gp_store_protocol:decode(Body);
                {error, R} -> {error, R}
            end;
        {error, R} -> {error, R}
    end.
