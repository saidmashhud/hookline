-module(gp_api_h_openapi).
-export([init/2]).

init(Req0, Opts) ->
    PrivDir = code:priv_dir(gp_api),
    File = filename:join([PrivDir, "openapi.yaml"]),
    case file:read_file(File) of
        {ok, Body} ->
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/yaml">>},
                Body, Req0),
            {ok, Req, Opts};
        {error, _} ->
            Req = cowboy_req:reply(404, #{}, <<"not found">>, Req0),
            {ok, Req, Opts}
    end.
