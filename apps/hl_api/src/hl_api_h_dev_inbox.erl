-module(hl_api_h_dev_inbox).
-export([init/2]).

init(Req0, #{action := ui} = Opts) ->
    PrivDir = code:priv_dir(hl_dev_inbox),
    FilePath = filename:join([PrivDir, "inbox_ui.html"]),
    HTML = case file:read_file(FilePath) of
        {ok, Bin} -> Bin;
        {error, _} -> inline_ui_html()
    end,
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/html; charset=utf-8">>},
        HTML, Req0),
    {ok, Req, Opts};

init(Req0, #{action := messages} = Opts) ->
    Method = cowboy_req:method(Req0),
    handle_inbox(Method, Req0, Opts);

init(Req0, #{action := receive_hook} = Opts) ->
    Token = cowboy_req:binding(token, Req0),
    Method = cowboy_req:method(Req0),
    case Method of
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            Headers = cowboy_req:headers(Req1),
            Msg = #{
                <<"token">>      => Token,
                <<"body">>       => Body,
                <<"headers">>    => maps:to_list(Headers),
                <<"received_at">> => erlang:system_time(millisecond)
            },
            hl_dev_inbox:store(Token, Msg),
            Req2 = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"ok">> => true}), Req1),
            {ok, Req2, Opts};
        _ ->
            Req = cowboy_req:reply(405, #{}, <<>>, Req0),
            {ok, Req, Opts}
    end;

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    handle_inbox(Method, Req0, Opts).

handle_inbox(<<"POST">>, Req0, Opts) ->
    %% Create a new inbox
    Token = hl_core_uuid:generate_str(),
    hl_dev_inbox:create(Token),
    %% Build absolute receive_url from the Host header so callers can use it
    %% directly as an endpoint URL (which requires http(s)://).
    Host = cowboy_req:header(<<"host">>, Req0, <<"localhost:8080">>),
    Scheme = case cowboy_req:header(<<"x-forwarded-proto">>, Req0, <<"http">>) of
        <<"https">> -> <<"https">>;
        _           -> <<"http">>
    end,
    ReceiveURL = <<Scheme/binary, "://", Host/binary,
                   "/v1/dev/inbox/receive/", Token/binary>>,
    Req = cowboy_req:reply(201,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(#{
            <<"token">>       => Token,
            <<"receive_url">> => ReceiveURL
        }), Req0),
    {ok, Req, Opts};

handle_inbox(<<"GET">>, Req0, Opts) ->
    QS = cowboy_req:parse_qs(Req0),
    %% Accept both `token` and `endpoint_id` (TZ compat)
    Token = case proplists:get_value(<<"token">>, QS) of
        undefined -> proplists:get_value(<<"endpoint_id">>, QS);
        T -> T
    end,
    case cowboy_req:binding(id, Req0) of
        undefined ->
            Messages = case Token of
                undefined -> [];
                T2 -> hl_dev_inbox:list(T2)
            end,
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"items">> => Messages, <<"count">> => length(Messages)}), Req0),
            {ok, Req, Opts};
        MsgId ->
            %% GET /messages/:id — look up by message id (nanosecond key)
            All = case Token of
                undefined -> [];
                T3 -> hl_dev_inbox:list(T3)
            end,
            case lists:filter(fun(M) -> maps:get(<<"id">>, M, undefined) =:= MsgId end, All) of
                [Msg | _] ->
                    Req = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        jsx:encode(Msg), Req0),
                    {ok, Req, Opts};
                [] ->
                    hl_api_error:reply(Req0, 404, not_found, <<"Message not found">>),
                    {ok, Req0, Opts}
            end
    end;

handle_inbox(_, Req0, Opts) ->
    Req = cowboy_req:reply(405, #{}, <<>>, Req0),
    {ok, Req, Opts}.

inline_ui_html() ->
    <<"<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"UTF-8\">",
      "<title>HookLine Dev Inbox</title>",
      "<style>body{font-family:system-ui,sans-serif;background:#0f1117;color:#e0e0e0;padding:24px}",
      "h1{color:#6c63ff;margin-bottom:20px}",
      ".toolbar{display:flex;gap:12px;margin-bottom:20px;align-items:center}",
      "input{background:#1a1d27;border:1px solid #2d3142;border-radius:6px;color:#e0e0e0;padding:8px 12px;font-size:13px;width:320px}",
      "button{background:#6c63ff;border:none;color:white;padding:8px 18px;border-radius:6px;cursor:pointer;font-size:13px}",
      ".msg{background:#1a1d27;border:1px solid #2d3142;border-radius:8px;padding:16px;margin-bottom:12px}",
      ".msg-time{font-size:11px;color:#6c73a0;margin-bottom:8px}",
      ".msg-topic{font-size:13px;font-weight:600;color:#6c63ff;margin-bottom:6px}",
      "pre{background:#0a0d14;border-radius:6px;padding:12px;font-size:12px;overflow-x:auto;white-space:pre-wrap}",
      ".empty{color:#6c73a0;text-align:center;padding:40px}",
      "</style></head><body>",
      "<h1>&#128513; Dev Inbox</h1>",
      "<div class=\"toolbar\">",
      "<input type=\"password\" id=\"apiKeyInput\" placeholder=\"API key\">",
      "<input type=\"text\" id=\"tokenInput\" placeholder=\"Inbox token\">",
      "<button onclick=\"createInbox()\">New Inbox</button>",
      "<button onclick=\"loadMessages()\" style=\"background:#2d3142;border:1px solid #3d4257\">Refresh</button>",
      "<span id=\"statusMsg\" style=\"font-size:13px;color:#6c73a0\"></span>",
      "</div>",
      "<div id=\"receiveUrl\" style=\"margin-bottom:16px;font-size:12px;color:#6c73a0\"></div>",
      "<div id=\"messages\"><div class=\"empty\">Create an inbox or enter a token to view messages</div></div>",
      "<script>",
      "let token = localStorage.getItem('hl_inbox_token') || '';",
      "let apiKey = localStorage.getItem('hl_api_key') || '';",
      "document.getElementById('tokenInput').value = token;",
      "document.getElementById('apiKeyInput').value = apiKey;",
      "if(token) { showUrl(); loadMessages(); }",
      "function authHeaders(json) {",
      "  apiKey = (document.getElementById('apiKeyInput').value || '').trim();",
      "  if(!apiKey) { throw new Error('API key is required'); }",
      "  localStorage.setItem('hl_api_key', apiKey);",
      "  const h = {'Authorization':'Bearer ' + apiKey};",
      "  if(json) h['Content-Type'] = 'application/json';",
      "  return h;",
      "}",
      "async function createInbox() {",
      "  const r = await fetch('/v1/dev/inbox', {method:'POST',headers:authHeaders(true),body:'{}'});",
      "  const d = await r.json();",
      "  token = d.token; localStorage.setItem('hl_inbox_token', token);",
      "  document.getElementById('tokenInput').value = token;",
      "  showUrl(); loadMessages();",
      "}",
      "function showUrl() {",
      "  document.getElementById('receiveUrl').innerHTML = `Webhook URL: <strong>${location.origin}/v1/dev/inbox/receive/${token}</strong>`;",
      "}",
      "async function loadMessages() {",
      "  token = document.getElementById('tokenInput').value;",
      "  localStorage.setItem('hl_inbox_token', token);",
      "  if(!token) return;",
      "  showUrl();",
      "  const r = await fetch('/v1/dev/inbox/messages?token='+token, {headers:authHeaders(false)});",
      "  const d = await r.json();",
      "  const msgs = d.items || [];",
      "  const el = document.getElementById('messages');",
      "  if(!msgs.length) { el.innerHTML = '<div class=\"empty\">No messages yet</div>'; return; }",
      "  el.innerHTML = msgs.reverse().map(m => {",
      "    let body = m.body || '';",
      "    try { body = JSON.stringify(JSON.parse(body instanceof Uint8Array ? new TextDecoder().decode(body) : body), null, 2); } catch(e) {}",
      "    const ts = new Date(m.received_at).toLocaleString();",
      "    const topic = (m.headers || []).find(([k]) => k === 'x-gp-topic');",
      "    return `<div class=\"msg\">",
      "<div class=\"msg-time\">${ts}</div>",
      "${topic ? `<div class=\"msg-topic\">${topic[1]}</div>` : ''}",
      "<pre>${body}</pre>",
      "</div>`;",
      "  }).join('');",
      "}",
      "setInterval(loadMessages, 2000);",
      "</script></body></html>">>.
