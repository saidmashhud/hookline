-module(gp_api_error).
-export([reply/3, reply/4]).

reply(Req, Status, Code) ->
    reply(Req, Status, Code, <<>>).

reply(Req, Status, Code, Detail) ->
    Body = jsx:encode(#{
        <<"error">>  => atom_to_binary(Code, utf8),
        <<"detail">> => Detail
    }),
    cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        Body, Req).
