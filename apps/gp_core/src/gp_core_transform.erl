%% Event transformation DSL.
%%
%% Supported operations (list of maps):
%%   #{<<"op">> => <<"rename_field">>,  <<"from">> => F, <<"to">> => T}
%%   #{<<"op">> => <<"remove_field">>,  <<"field">> => F}
%%   #{<<"op">> => <<"add_field">>,     <<"field">> => F, <<"value">> => V}
%%   #{<<"op">> => <<"set_field">>,     <<"field">> => F, <<"value">> => V}
-module(gp_core_transform).
-export([apply/2]).

apply(undefined, Event) -> Event;
apply(null, Event)      -> Event;
apply([], Event)        -> Event;
apply(Ops, Event) when is_list(Ops) ->
    lists:foldl(fun apply_op/2, Event, Ops);
apply(Op, Event) when is_map(Op) ->
    apply_op(Op, Event).

apply_op(#{<<"op">> := <<"rename_field">>,
           <<"from">> := From,
           <<"to">>   := To}, Event) ->
    case maps:take(From, Event) of
        {Val, Event2} -> Event2#{To => Val};
        error         -> Event
    end;
apply_op(#{<<"op">> := <<"remove_field">>, <<"field">> := Field}, Event) ->
    maps:remove(Field, Event);
apply_op(#{<<"op">> := <<"add_field">>,
           <<"field">> := Field,
           <<"value">> := Val}, Event) ->
    case maps:is_key(Field, Event) of
        true  -> Event;
        false -> Event#{Field => Val}
    end;
apply_op(#{<<"op">> := <<"set_field">>,
           <<"field">> := Field,
           <<"value">> := Val}, Event) ->
    Event#{Field => Val};
apply_op(_, Event) ->
    Event.
