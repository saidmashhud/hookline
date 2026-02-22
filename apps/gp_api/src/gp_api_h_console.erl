%% Serves the management console UI.
%% All /console/** paths return the same SPA index.html.
-module(gp_api_h_console).
-export([init/2]).

init(Req0, Opts) ->
    PrivDir  = code:priv_dir(gp_api),
    FilePath = filename:join([PrivDir, "console", "index.html"]),
    HTML = case file:read_file(FilePath) of
        {ok, Bin} -> Bin;
        {error, _} -> fallback_html()
    end,
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/html; charset=utf-8">>},
        HTML, Req0),
    {ok, Req, Opts}.

fallback_html() ->
    <<"<!DOCTYPE html><html><head><title>GatePulse Console</title></head>",
      "<body><h1>GatePulse Console</h1>",
      "<p>Console assets not found. Run <code>make assets</code> to build.</p>",
      "</body></html>">>.
