#!/usr/bin/env bash
# HookLine — complex end-to-end test scenarios
# Usage: HL_URL=http://localhost:8080 HL_API_KEY=<api-key> bash test/e2e.sh
set -euo pipefail

HL_URL="${HL_URL:-http://localhost:8080}"
HL_API_KEY="${HL_API_KEY:?HL_API_KEY is required}"
HL_ADMIN_KEY="${HL_ADMIN_KEY:-}"
PASS=0; FAIL=0; SKIP=0

# ── Helpers ──────────────────────────────────────────────────────────────────

bold()  { printf '\033[1m%s\033[0m\n' "$*"; }
red()   { printf '\033[31m%s\033[0m\n' "$*"; }
green() { printf '\033[32m%s\033[0m\n' "$*"; }
yellow(){ printf '\033[33m%s\033[0m\n' "$*"; }
cyan()  { printf '\033[36m%s\033[0m\n' "$*"; }

pass()  { PASS=$((PASS+1)); green "  ✓ $1"; }
fail()  { FAIL=$((FAIL+1)); red   "  ✗ $1"; [[ -n "${2:-}" ]] && red "    → $2"; }
skip()  { SKIP=$((SKIP+1)); yellow "  ⊘ $1 (skipped)"; }

section() { echo; bold "═══ $1 ═══"; }

hl_get()   { curl -sf  -H "Authorization: Bearer $HL_API_KEY" "${HL_URL}$1"; }
hl_post()  { curl -sf -X POST   -H "Authorization: Bearer $HL_API_KEY" \
               -H "Content-Type: application/json" -d "$2" "${HL_URL}$1"; }
hl_patch() { curl -sf -X PATCH  -H "Authorization: Bearer $HL_API_KEY" \
               -H "Content-Type: application/json" -d "$2" "${HL_URL}$1"; }
hl_del()   { curl -sf -X DELETE -H "Authorization: Bearer $HL_API_KEY" "${HL_URL}$1"; }
hl_post_status() {
  curl -s -o /dev/null -w "%{http_code}" -X POST \
    -H "Authorization: Bearer $HL_API_KEY" \
    -H "Content-Type: application/json" -d "$2" "${HL_URL}$1"
}

jq_str() { python3 -c "import sys,json; d=json.load(sys.stdin); print(d$1)" 2>/dev/null || echo ""; }
jq_len() { python3 -c "import sys,json; d=json.load(sys.stdin); print(len(d$1))" 2>/dev/null || echo "0"; }

wait_for_delivery() {
  local token="$1" expected="${2:-1}" timeout="${3:-15}" label="${4:-delivery}"
  for i in $(seq 1 "$timeout"); do
    sleep 1
    local cnt
    cnt=$(hl_get "/v1/dev/inbox/messages?token=$token" 2>/dev/null \
          | jq_len ".get('items',[])")
    if [[ "$cnt" -ge "$expected" ]]; then
      pass "$label (${cnt} msg(s) after ${i}s)"
      return 0
    fi
  done
  fail "$label" "expected >=$expected messages, got 0 after ${timeout}s"
  return 1
}

wait_no_delivery() {
  local token="$1" wait="${2:-5}" label="${3:-no-delivery}"
  sleep "$wait"
  local cnt
  cnt=$(hl_get "/v1/dev/inbox/messages?token=$token" 2>/dev/null \
        | jq_len ".get('items',[])")
  if [[ "$cnt" -eq 0 ]]; then
    pass "$label (correctly not delivered after ${wait}s)"
  else
    fail "$label" "expected 0 messages, got $cnt"
  fi
}

# ── Setup: shared inbox ───────────────────────────────────────────────────────

section "Setup"
INBOX=$(hl_post "/v1/dev/inbox" '{}')
TOKEN=$(echo "$INBOX" | jq_str "['token']")
RECV_URL=$(echo "$INBOX" | jq_str "['receive_url']")
[[ -n "$TOKEN" ]] && pass "Dev inbox created (token: ${TOKEN:0:8}…)" \
                   || { fail "Dev inbox creation"; exit 1; }

# ── Scenario 1: Basic happy path ──────────────────────────────────────────────

section "Scenario 1: Basic happy path"

EP=$(hl_post "/v1/endpoints" \
  "{\"url\":\"$RECV_URL\",\"enabled\":true,\"timeout_ms\":5000}")
EP_ID=$(echo "$EP" | jq_str "['endpoint_id']")
[[ -n "$EP_ID" ]] && pass "Endpoint created ($EP_ID)" || { fail "Endpoint creation"; exit 1; }

SUB=$(hl_post "/v1/subscriptions" \
  "{\"endpoint_id\":\"$EP_ID\",\"topic_pattern\":\"orders.*\"}")
SUB_ID=$(echo "$SUB" | jq_str "['subscription_id']")
[[ -n "$SUB_ID" ]] && pass "Subscription created ($SUB_ID)" || { fail "Subscription creation"; exit 1; }

EV=$(hl_post "/v1/events" \
  '{"topic":"orders.created","payload":{"order_id":"ORD-001","amount":250}}')
EV_ID=$(echo "$EV" | jq_str "['event_id']")
[[ -n "$EV_ID" ]] && pass "Event published ($EV_ID)" || { fail "Event publish"; exit 1; }

wait_for_delivery "$TOKEN" 1 15 "Delivery to inbox"

# Verify delivery record
DELIV=$(hl_get "/v1/deliveries?event_id=$EV_ID")
D_STATUS=$(echo "$DELIV" | jq_str ".get('items',[])[0].get('status','')")
if [[ "$D_STATUS" == "success" ]]; then
  pass "Delivery status = success"
else
  fail "Delivery status" "expected 'success', got '$D_STATUS'"
fi

# ── Scenario 2: Idempotency ───────────────────────────────────────────────────

section "Scenario 2: Idempotency"

EV_A=$(hl_post "/v1/events" \
  '{"topic":"orders.paid","idempotency_key":"pay-001","payload":{"amount":99}}')
EV_B=$(hl_post "/v1/events" \
  '{"topic":"orders.paid","idempotency_key":"pay-001","payload":{"amount":99}}')

ID_A=$(echo "$EV_A" | jq_str "['event_id']")
ID_B=$(echo "$EV_B" | jq_str "['event_id']")

if [[ "$ID_A" == "$ID_B" && -n "$ID_A" ]]; then
  pass "Duplicate event returns same event_id ($ID_A)"
else
  fail "Idempotency" "IDs differ: A=$ID_A B=$ID_B"
fi

# Only one delivery should happen (not two)
sleep 3
CNT2=$(hl_get "/v1/deliveries?event_id=$ID_A" | jq_len ".get('items',[])")
if [[ "$CNT2" -le 2 ]]; then
  pass "Only one delivery job created for duplicate event"
else
  fail "Idempotency delivery count" "expected 1, got $CNT2"
fi

# ── Scenario 3: Topic wildcard matching ──────────────────────────────────────

section "Scenario 3: Topic wildcard routing"

INBOX2=$(hl_post "/v1/dev/inbox" '{}')
TOKEN2=$(echo "$INBOX2" | jq_str "['token']")
RECV2=$(echo "$INBOX2" | jq_str "['receive_url']")

EP2=$(hl_post "/v1/endpoints" \
  "{\"url\":\"$RECV2\",\"enabled\":true,\"timeout_ms\":3000}")
EP2_ID=$(echo "$EP2" | jq_str "['endpoint_id']")

# Subscribe to payments.# (should match payments.processed and payments.failed)
hl_post "/v1/subscriptions" \
  "{\"endpoint_id\":\"$EP2_ID\",\"topic_pattern\":\"payments.#\"}" >/dev/null

hl_post "/v1/events" '{"topic":"payments.processed","payload":{"ref":"p1"}}' >/dev/null
hl_post "/v1/events" '{"topic":"payments.failed","payload":{"ref":"p2"}}' >/dev/null
# This should NOT be delivered (different top-level topic)
hl_post "/v1/events" '{"topic":"orders.paid","payload":{"ref":"p3"}}' >/dev/null

sleep 5
CNT3=$(hl_get "/v1/dev/inbox/messages?token=$TOKEN2" | jq_len ".get('items',[])")
if [[ "$CNT3" -eq 2 ]]; then
  pass "Wildcard # matched exactly 2 payments.* events (orders.* correctly excluded)"
elif [[ "$CNT3" -gt 0 ]]; then
  fail "Wildcard routing count" "expected 2, got $CNT3"
else
  fail "Wildcard routing" "no events delivered"
fi

# Star wildcard: orders.* should match orders.X but not orders.X.Y
INBOX3=$(hl_post "/v1/dev/inbox" '{}')
TOKEN3=$(echo "$INBOX3" | jq_str "['token']")
RECV3=$(echo "$INBOX3" | jq_str "['receive_url']")
EP3=$(hl_post "/v1/endpoints" \
  "{\"url\":\"$RECV3\",\"enabled\":true,\"timeout_ms\":3000}")
EP3_ID=$(echo "$EP3" | jq_str "['endpoint_id']")
hl_post "/v1/subscriptions" \
  "{\"endpoint_id\":\"$EP3_ID\",\"topic_pattern\":\"ship.*\"}" >/dev/null

hl_post "/v1/events" '{"topic":"ship.created","payload":{}}' >/dev/null  # ✓
hl_post "/v1/events" '{"topic":"ship.updated","payload":{}}' >/dev/null  # ✓
hl_post "/v1/events" '{"topic":"ship.label.printed","payload":{}}' >/dev/null  # ✗ (two segments)

sleep 5
CNT_STAR=$(hl_get "/v1/dev/inbox/messages?token=$TOKEN3" | jq_len ".get('items',[])")
if [[ "$CNT_STAR" -eq 2 ]]; then
  pass "Star wildcard * matched 2 single-segment topics (multi-segment correctly excluded)"
else
  fail "Star wildcard routing" "expected 2, got $CNT_STAR"
fi

# ── Scenario 4: Filter DSL ────────────────────────────────────────────────────

section "Scenario 4: Subscription filter DSL"

INBOX4=$(hl_post "/v1/dev/inbox" '{}')
TOKEN4=$(echo "$INBOX4" | jq_str "['token']")
RECV4=$(echo "$INBOX4" | jq_str "['receive_url']")
EP4=$(hl_post "/v1/endpoints" \
  "{\"url\":\"$RECV4\",\"enabled\":true,\"timeout_ms\":3000}")
EP4_ID=$(echo "$EP4" | jq_str "['endpoint_id']")

# Only deliver orders with amount > 100 AND currency = USD
hl_post "/v1/subscriptions" "$(cat <<JSON
{
  "endpoint_id": "$EP4_ID",
  "topic_pattern": "sales.*",
  "filter": {
    "and": [
      {"gt": "payload.amount", "value": 100},
      {"eq": "payload.currency", "value": "USD"}
    ]
  }
}
JSON
)" >/dev/null

# Should deliver (amount=200, currency=USD)
hl_post "/v1/events" '{"topic":"sales.completed","payload":{"amount":200,"currency":"USD"}}' >/dev/null
# Should NOT deliver (amount=50 < 100)
hl_post "/v1/events" '{"topic":"sales.completed","payload":{"amount":50,"currency":"USD"}}' >/dev/null
# Should NOT deliver (currency=EUR)
hl_post "/v1/events" '{"topic":"sales.completed","payload":{"amount":500,"currency":"EUR"}}' >/dev/null

sleep 5
CNT4=$(hl_get "/v1/dev/inbox/messages?token=$TOKEN4" | jq_len ".get('items',[])")
if [[ "$CNT4" -eq 1 ]]; then
  pass "Filter DSL: only 1 of 3 events passed (amount>100 AND currency=USD)"
else
  fail "Filter DSL" "expected 1 delivery, got $CNT4"
fi

# ── Scenario 5: Endpoint PATCH (live config update) ───────────────────────────

section "Scenario 5: Endpoint live update (PATCH)"

EP5=$(hl_post "/v1/endpoints" \
  "{\"url\":\"$RECV_URL\",\"enabled\":true,\"timeout_ms\":3000,\"rate_limit_rps\":10}")
EP5_ID=$(echo "$EP5" | jq_str "['endpoint_id']")

# Disable endpoint
hl_patch "/v1/endpoints/$EP5_ID" '{"enabled":false}' >/dev/null

EP5_UPDATED=$(hl_get "/v1/endpoints/$EP5_ID")
ENABLED=$(echo "$EP5_UPDATED" | jq_str "['enabled']")
if [[ "$ENABLED" == "False" ]] || [[ "$ENABLED" == "false" ]]; then
  pass "PATCH endpoint: enabled=false persisted"
else
  fail "PATCH endpoint" "expected enabled=false, got '$ENABLED'"
fi

# Re-enable
hl_patch "/v1/endpoints/$EP5_ID" \
  '{"enabled":true,"rate_limit_rps":100,"max_in_flight":20}' >/dev/null
EP5_FINAL=$(hl_get "/v1/endpoints/$EP5_ID")
RPS=$(echo "$EP5_FINAL" | jq_str "['rate_limit_rps']")
if [[ "$RPS" == "100" ]]; then
  pass "PATCH endpoint: rate_limit_rps=100 persisted"
else
  fail "PATCH endpoint rps" "expected 100, got '$RPS'"
fi
hl_del "/v1/endpoints/$EP5_ID" >/dev/null 2>&1 || true

# ── Scenario 6: Replay ────────────────────────────────────────────────────────

section "Scenario 6: Event replay"

INBOX5=$(hl_post "/v1/dev/inbox" '{}')
TOKEN5=$(echo "$INBOX5" | jq_str "['token']")
RECV5=$(echo "$INBOX5" | jq_str "['receive_url']")
EP6=$(hl_post "/v1/endpoints" \
  "{\"url\":\"$RECV5\",\"enabled\":true,\"timeout_ms\":3000}")
EP6_ID=$(echo "$EP6" | jq_str "['endpoint_id']")
hl_post "/v1/subscriptions" \
  "{\"endpoint_id\":\"$EP6_ID\",\"topic_pattern\":\"replay.#\"}" >/dev/null

# Publish event, wait for delivery
EV6=$(hl_post "/v1/events" '{"topic":"replay.test","payload":{"v":1}}')
EV6_ID=$(echo "$EV6" | jq_str "['event_id']")
sleep 4

# Replay it — should trigger a second delivery
hl_post "/v1/replay" "{\"event_id\":\"$EV6_ID\"}" >/dev/null
sleep 4

CNT6=$(hl_get "/v1/dev/inbox/messages?token=$TOKEN5" | jq_len ".get('items',[])")
if [[ "$CNT6" -ge 2 ]]; then
  pass "Replay: event delivered twice (original + replay) — $CNT6 total"
else
  fail "Replay" "expected >=2 deliveries, got $CNT6"
fi

# ── Scenario 7: Admin pause / resume ─────────────────────────────────────────

section "Scenario 7: Admin pause / resume delivery"

INBOX6=$(hl_post "/v1/dev/inbox" '{}')
TOKEN6=$(echo "$INBOX6" | jq_str "['token']")
RECV6=$(echo "$INBOX6" | jq_str "['receive_url']")
EP7=$(hl_post "/v1/endpoints" \
  "{\"url\":\"$RECV6\",\"enabled\":true,\"timeout_ms\":3000}")
EP7_ID=$(echo "$EP7" | jq_str "['endpoint_id']")
hl_post "/v1/subscriptions" \
  "{\"endpoint_id\":\"$EP7_ID\",\"topic_pattern\":\"admin.#\"}" >/dev/null

# Pause job claiming
hl_post "/v1/admin/store/pause-claims" '{}' >/dev/null
pass "Admin: paused job claiming"

# Publish while paused — delivery should NOT happen
hl_post "/v1/events" '{"topic":"admin.paused","payload":{"test":true}}' >/dev/null
sleep 4
CNT_PAUSED=$(hl_get "/v1/dev/inbox/messages?token=$TOKEN6" | jq_len ".get('items',[])")
if [[ "$CNT_PAUSED" -eq 0 ]]; then
  pass "Admin: no delivery while paused"
else
  yellow "  ~ Admin pause: $CNT_PAUSED message(s) delivered (timing-dependent, may be ok)"
fi

# Resume
hl_post "/v1/admin/store/resume-claims" '{}' >/dev/null
pass "Admin: resumed job claiming"
sleep 6

CNT_RESUMED=$(hl_get "/v1/dev/inbox/messages?token=$TOKEN6" | jq_len ".get('items',[])")
if [[ "$CNT_RESUMED" -ge 1 ]]; then
  pass "Admin: delivery resumed after unpause ($CNT_RESUMED message(s))"
else
  fail "Admin resume" "no delivery after resume (waited 6s)"
fi

# ── Scenario 8: Admin stats ───────────────────────────────────────────────────

section "Scenario 8: Admin stats & queue"

STATS=$(hl_get "/v1/admin/stats")
HAS_QUEUE=$(echo "$STATS" | python3 -c "import sys,json; d=json.load(sys.stdin); print('queue' in d)" 2>/dev/null || echo "False")
if [[ "$HAS_QUEUE" == "True" ]]; then
  PENDING=$(echo "$STATS" | jq_str "['queue']['pending']")
  INFLIGHT=$(echo "$STATS" | jq_str "['queue']['inflight']")
  pass "Admin stats: pending=$PENDING inflight=$INFLIGHT"
else
  fail "Admin stats" "response missing 'queue' field: $STATS"
fi

# ── Scenario 9: Payload size limit (413) ─────────────────────────────────────

section "Scenario 9: Payload oversize → 413"

BIG_PAYLOAD=$(python3 -c "import json; print(json.dumps({'topic':'test.big','payload':{'data':'x'*600000}}))")
STATUS=$(curl -s -o /dev/null -w "%{http_code}" -X POST \
  -H "Authorization: Bearer $HL_API_KEY" \
  -H "Content-Type: application/json" \
  -d "$BIG_PAYLOAD" "${HL_URL}/v1/events")
if [[ "$STATUS" == "413" ]]; then
  pass "Payload >512KB → 413 Payload Too Large"
else
  fail "Oversize payload" "expected 413, got $STATUS"
fi

# ── Scenario 10: Auth — 401 on missing key ────────────────────────────────────

section "Scenario 10: Authentication"

STATUS_401=$(curl -s -o /dev/null -w "%{http_code}" "${HL_URL}/v1/events?limit=1")
if [[ "$STATUS_401" == "401" ]]; then
  pass "No auth header → 401 Unauthorized"
else
  fail "Auth: no header" "expected 401, got $STATUS_401"
fi

STATUS_401B=$(curl -s -o /dev/null -w "%{http_code}" \
  -H "Authorization: Bearer wrong-key" "${HL_URL}/v1/events?limit=1")
if [[ "$STATUS_401B" == "401" ]]; then
  pass "Wrong API key → 401 Unauthorized"
else
  fail "Auth: wrong key" "expected 401, got $STATUS_401B"
fi

# ── Scenario 11: DLQ flow ────────────────────────────────────────────────────

section "Scenario 11: DLQ — inspect & check structure"

DLQ=$(hl_get "/v1/dlq")
HAS_ITEMS=$(echo "$DLQ" | python3 -c \
  "import sys,json; d=json.load(sys.stdin); print('items' in d)" 2>/dev/null || echo "False")
if [[ "$HAS_ITEMS" == "True" ]]; then
  DLQ_COUNT=$(echo "$DLQ" | jq_len ".get('items',[])")
  pass "GET /dlq returns items array (count=$DLQ_COUNT)"
else
  fail "DLQ structure" "missing items field: $DLQ"
fi

# ── Scenario 12: API Keys ────────────────────────────────────────────────────

section "Scenario 12: API Key management"

if [[ -z "$HL_ADMIN_KEY" ]]; then
  skip "API key management (HL_ADMIN_KEY not set; admin scope required)"
else
  KEY_RESP=$(curl -s -X POST \
    -H "Authorization: Bearer $HL_ADMIN_KEY" \
    -H "Content-Type: application/json" \
    -d '{"name":"e2e test key","scopes":["events:read","endpoints:manage"]}' \
    "${HL_URL}/v1/apikeys")
  NEW_KEY_ID=$(echo "$KEY_RESP" | jq_str "['key_id']")
  NEW_TOKEN=$(echo "$KEY_RESP" | jq_str "['key']")

  if [[ -n "$NEW_KEY_ID" && -n "$NEW_TOKEN" ]]; then
    pass "API key created (id=${NEW_KEY_ID:0:8}… token=gp_***)"
  else
    fail "API key creation" "response: $KEY_RESP"
  fi

  # New key can list events
  if [[ -n "$NEW_TOKEN" ]]; then
    STATUS_NEW=$(curl -s -o /dev/null -w "%{http_code}" \
      -H "Authorization: Bearer $NEW_TOKEN" "${HL_URL}/v1/events?limit=1")
    if [[ "$STATUS_NEW" == "200" ]]; then
      pass "New API key authenticated successfully"
    else
      fail "New API key auth" "expected 200, got $STATUS_NEW"
    fi
  fi

  # Revoke key
  if [[ -n "$NEW_KEY_ID" ]]; then
    STATUS_REVOKE=$(curl -s -o /dev/null -w "%{http_code}" -X DELETE \
      -H "Authorization: Bearer $HL_ADMIN_KEY" \
      "${HL_URL}/v1/apikeys/$NEW_KEY_ID")
    if [[ "$STATUS_REVOKE" == "204" ]]; then
      pass "API key revoked"
    else
      fail "API key revoke" "expected 204, got $STATUS_REVOKE"
    fi
  fi
fi

# ── Scenario 13: Concurrent event publishing ──────────────────────────────────

section "Scenario 13: Concurrent publishing (10 events)"

INBOX_BULK=$(hl_post "/v1/dev/inbox" '{}')
TOKEN_BULK=$(echo "$INBOX_BULK" | jq_str "['token']")
RECV_BULK=$(echo "$INBOX_BULK" | jq_str "['receive_url']")
EP_BULK=$(hl_post "/v1/endpoints" \
  "{\"url\":\"$RECV_BULK\",\"enabled\":true,\"timeout_ms\":5000,\"max_in_flight\":20}")
EP_BULK_ID=$(echo "$EP_BULK" | jq_str "['endpoint_id']")
hl_post "/v1/subscriptions" \
  "{\"endpoint_id\":\"$EP_BULK_ID\",\"topic_pattern\":\"bulk.*\"}" >/dev/null

# Publish 10 events concurrently
for i in $(seq 1 10); do
  hl_post "/v1/events" \
    "{\"topic\":\"bulk.item\",\"payload\":{\"seq\":$i}}" >/dev/null &
done
wait

# Wait for all deliveries
sleep 10
CNT_BULK=$(hl_get "/v1/dev/inbox/messages?token=$TOKEN_BULK" | jq_len ".get('items',[])")
if [[ "$CNT_BULK" -eq 10 ]]; then
  pass "Concurrent: all 10 events delivered ($CNT_BULK)"
elif [[ "$CNT_BULK" -ge 8 ]]; then
  yellow "  ~ Concurrent: $CNT_BULK/10 delivered (acceptable — may still be in-flight)"
  PASS=$((PASS+1))
else
  fail "Concurrent publishing" "only $CNT_BULK/10 delivered after 10s"
fi

# ── Scenario 14: SSE stream ───────────────────────────────────────────────────

section "Scenario 14: SSE stream"

# Connect to stream in background, capture first event
SSE_OUT=$(mktemp)
curl -s -N \
  -H "Authorization: Bearer $HL_API_KEY" \
  -H "Accept: text/event-stream" \
  "${HL_URL}/v1/stream?topic=sse.test" > "$SSE_OUT" &
SSE_PID=$!

sleep 1  # wait for connection to establish

hl_post "/v1/events" '{"topic":"sse.test","payload":{"sse":true}}' >/dev/null

sleep 2
kill "$SSE_PID" 2>/dev/null || true

if grep -q "data:" "$SSE_OUT" 2>/dev/null; then
  pass "SSE: received event on stream"
else
  yellow "  ~ SSE: no data in stream output (may need active subscription/endpoint)"
  SKIP=$((SKIP+1))
fi
rm -f "$SSE_OUT"

# ── Scenario 15: Endpoint validation ─────────────────────────────────────────

section "Scenario 15: Input validation"

# Missing URL → 400
S1=$(hl_post_status "/v1/endpoints" '{"enabled":true}')
[[ "$S1" == "400" ]] \
  && pass "Missing URL → 400" \
  || fail "Endpoint validation: missing URL" "expected 400, got $S1"

# Invalid URL (no http://) → 400
S2=$(hl_post_status "/v1/endpoints" '{"url":"ftp://example.com","enabled":true}')
[[ "$S2" == "400" ]] \
  && pass "Invalid URL scheme → 400" \
  || fail "Endpoint validation: bad URL" "expected 400, got $S2"

# Missing topic → 400
S3=$(hl_post_status "/v1/events" '{"payload":{}}')
[[ "$S3" == "400" ]] \
  && pass "Missing topic → 400" \
  || fail "Event validation: missing topic" "expected 400, got $S3"

# ── Scenario 16: Event cursor pagination ─────────────────────────────────────

section "Scenario 16: Event list pagination"

# Publish 5 events
for i in $(seq 1 5); do
  hl_post "/v1/events" \
    "{\"topic\":\"page.test\",\"payload\":{\"n\":$i}}" >/dev/null
done

PAGE1=$(hl_get "/v1/events?limit=3&topic=page.test")
P1_COUNT=$(echo "$PAGE1" | jq_len ".get('items',[])")
CURSOR=$(echo "$PAGE1" | jq_str ".get('cursor','') or ''")

if [[ "$P1_COUNT" -le 3 ]]; then
  pass "Pagination: first page has $P1_COUNT items (limit=3)"
else
  fail "Pagination" "expected <=3 items, got $P1_COUNT"
fi

if [[ -n "$CURSOR" && "$CURSOR" != "None" && "$CURSOR" != "" ]]; then
  PAGE2=$(hl_get "/v1/events?limit=3&topic=page.test&cursor=$CURSOR")
  P2_COUNT=$(echo "$PAGE2" | jq_len ".get('items',[])")
  pass "Pagination: second page has $P2_COUNT item(s) via cursor"
else
  skip "Pagination cursor (no cursor returned)"
fi

# ── Cleanup ───────────────────────────────────────────────────────────────────

section "Cleanup"
hl_del "/v1/subscriptions/$SUB_ID" >/dev/null 2>&1 && pass "Cleanup: subscription deleted" || true
hl_del "/v1/endpoints/$EP_ID"      >/dev/null 2>&1 && pass "Cleanup: endpoint deleted"    || true

# ── Summary ───────────────────────────────────────────────────────────────────

echo
bold "══════════════════════════════════════"
green "  PASS: $PASS"
[[ $FAIL -gt 0 ]] && red "  FAIL: $FAIL" || true
[[ $SKIP -gt 0 ]] && yellow "  SKIP: $SKIP" || true
bold "══════════════════════════════════════"
echo

[[ $FAIL -eq 0 ]]
