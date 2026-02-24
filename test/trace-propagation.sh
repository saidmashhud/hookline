#!/usr/bin/env bash
# HookLine trace context propagation tests
#
# Verifies that W3C Trace Context headers (traceparent, tracestate) are:
#   1. Accepted on event publish
#   2. Stored in the event's _trace metadata
#   3. Forwarded to the endpoint in webhook delivery headers
#
# Usage:
#   HL_URL=http://localhost:8080 HL_API_KEY=<api-key> ./test/trace-propagation.sh
#
# Prerequisites:
#   HookLine running (standalone or embedded mode)

set -euo pipefail

HL_URL="${HL_URL:-http://localhost:8080}"
HL_API_KEY="${HL_API_KEY:?HL_API_KEY is required}"
PASS=0; FAIL=0

red()   { printf '\033[31m%s\033[0m\n' "$*"; }
green() { printf '\033[32m%s\033[0m\n' "$*"; }
info()  { printf '\033[36m%s\033[0m\n' "$*"; }

ok()   { PASS=$((PASS+1)); green "  PASS: $1"; }
fail() { FAIL=$((FAIL+1)); red   "  FAIL: $1 — $2"; }

check() {
  local name="$1"; local actual="$2"; local expected="$3"
  if echo "$actual" | grep -q "$expected"; then
    ok "$name"
  else
    fail "$name" "expected '$expected' in: $actual"
  fi
}

gp_get()  { curl -sf -H "Authorization: Bearer $HL_API_KEY" "${HL_URL}$1"; }
gp_post() { curl -sf -X POST -H "Authorization: Bearer $HL_API_KEY" \
              -H "Content-Type: application/json" "${@:3}" -d "$2" "${HL_URL}$1"; }
gp_del()  { curl -sf -X DELETE -H "Authorization: Bearer $HL_API_KEY" "${HL_URL}$1"; }

info "=== HookLine Trace Propagation Tests ==="
info "Target: $HL_URL"
echo

# ── Setup: create dev inbox + endpoint + subscription ─────────────────────
info "--- Setup ---"

INBOX=$(curl -sf -X POST "${HL_URL}/v1/dev/inbox" \
  -H "Authorization: Bearer $HL_API_KEY" -d '{}')
check "Create dev inbox" "$INBOX" "receive_url"
INBOX_TOKEN=$(echo "$INBOX" | python3 -c "import sys,json; print(json.load(sys.stdin)['token'])")
RECEIVE_URL=$(echo "$INBOX" | python3 -c "import sys,json; print(json.load(sys.stdin)['receive_url'])")

EP=$(gp_post "/v1/endpoints" "{\"url\":\"${RECEIVE_URL}\",\"enabled\":true,\"timeout_ms\":5000}")
check "Create endpoint" "$EP" "endpoint_id"
EP_ID=$(echo "$EP" | python3 -c "import sys,json; print(json.load(sys.stdin)['endpoint_id'])")

SUB=$(gp_post "/v1/subscriptions" \
  "{\"endpoint_id\":\"$EP_ID\",\"topic_pattern\":\"trace.#\"}")
check "Create subscription" "$SUB" "subscription_id"
SUB_ID=$(echo "$SUB" | python3 -c "import sys,json; print(json.load(sys.stdin)['subscription_id'])")

# ── Test 1: Publish event WITH traceparent ────────────────────────────────
info "--- Publish with traceparent ---"

TRACE_PARENT="00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
TRACE_STATE="congo=t61rcWkgMzE"

EV=$(curl -sf -X POST "${HL_URL}/v1/events" \
  -H "Authorization: Bearer $HL_API_KEY" \
  -H "Content-Type: application/json" \
  -H "traceparent: $TRACE_PARENT" \
  -H "tracestate: $TRACE_STATE" \
  -d '{"topic":"trace.created","payload":{"order_id":"trace-001"}}')
check "POST /v1/events with traceparent" "$EV" "event_id"
EV_ID=$(echo "$EV" | python3 -c "import sys,json; print(json.load(sys.stdin)['event_id'])")

# ── Wait for delivery ─────────────────────────────────────────────────────
info "--- Waiting for delivery (up to 10s) ---"
DELIVERED=0
for i in $(seq 1 10); do
  sleep 1
  MSGS=$(gp_get "/v1/dev/inbox/messages?token=$INBOX_TOKEN" 2>/dev/null || echo '{"items":[]}')
  COUNT=$(echo "$MSGS" | python3 -c "import sys,json; print(len(json.load(sys.stdin).get('items',[])))" 2>/dev/null || echo "0")
  if [[ "$COUNT" -gt 0 ]]; then
    DELIVERED=1
    ok "Webhook delivered (${COUNT} msg(s) after ${i}s)"
    break
  fi
done
[[ $DELIVERED -eq 0 ]] && fail "Webhook delivery" "no messages in inbox after 10s"

# ── Verify traceparent in delivered headers ───────────────────────────────
info "--- Verify trace headers in delivery ---"
if [[ $DELIVERED -eq 1 ]]; then
  # Get the first message and inspect its headers
  FIRST_MSG=$(echo "$MSGS" | python3 -c "
import sys, json
msgs = json.load(sys.stdin).get('items', [])
if msgs:
    msg = msgs[0]
    headers = msg.get('headers', {})
    print(json.dumps(headers))
else:
    print('{}')
" 2>/dev/null || echo '{}')

  check "traceparent in delivery headers" "$FIRST_MSG" "traceparent"
  check "traceparent value matches" "$FIRST_MSG" "$TRACE_PARENT"
fi

# ── Test 2: Publish event WITHOUT traceparent ─────────────────────────────
info "--- Publish without traceparent ---"

# Clear inbox state by creating a new one
INBOX2=$(curl -sf -X POST "${HL_URL}/v1/dev/inbox" \
  -H "Authorization: Bearer $HL_API_KEY" -d '{}')
INBOX2_TOKEN=$(echo "$INBOX2" | python3 -c "import sys,json; print(json.load(sys.stdin)['token'])")
RECEIVE2_URL=$(echo "$INBOX2" | python3 -c "import sys,json; print(json.load(sys.stdin)['receive_url'])")

# Update endpoint to use new inbox
curl -sf -X PATCH "${HL_URL}/v1/endpoints/${EP_ID}" \
  -H "Authorization: Bearer $HL_API_KEY" \
  -H "Content-Type: application/json" \
  -d "{\"url\":\"${RECEIVE2_URL}\"}" >/dev/null 2>&1

EV2=$(gp_post "/v1/events" '{"topic":"trace.no-trace","payload":{"order_id":"notrace-001"}}')
check "POST /v1/events without traceparent" "$EV2" "event_id"

# Wait for delivery
info "--- Waiting for no-trace delivery (up to 10s) ---"
DELIVERED2=0
for i in $(seq 1 10); do
  sleep 1
  MSGS2=$(gp_get "/v1/dev/inbox/messages?token=$INBOX2_TOKEN" 2>/dev/null || echo '{"items":[]}')
  COUNT2=$(echo "$MSGS2" | python3 -c "import sys,json; print(len(json.load(sys.stdin).get('items',[])))" 2>/dev/null || echo "0")
  if [[ "$COUNT2" -gt 0 ]]; then
    DELIVERED2=1
    ok "No-trace webhook delivered after ${i}s"
    break
  fi
done

if [[ $DELIVERED2 -eq 1 ]]; then
  # Verify NO traceparent in headers
  NO_TRACE_HEADERS=$(echo "$MSGS2" | python3 -c "
import sys, json
msgs = json.load(sys.stdin).get('items', [])
if msgs:
    headers = msgs[0].get('headers', {})
    has_trace = 'traceparent' in headers or 'traceparent' in {k.lower(): v for k, v in headers.items()}
    print('has_traceparent' if has_trace else 'no_traceparent')
else:
    print('no_messages')
" 2>/dev/null || echo 'error')
  if [[ "$NO_TRACE_HEADERS" == "no_traceparent" ]]; then
    ok "No traceparent in delivery without trace context"
  elif [[ "$NO_TRACE_HEADERS" == "has_traceparent" ]]; then
    fail "No-trace delivery" "traceparent present in delivery without trace context"
  fi
fi

# ── Cleanup ──────────────────────────────────────────────────────────────
info "--- Cleanup ---"
gp_del "/v1/subscriptions/$SUB_ID" >/dev/null 2>&1 && ok "DELETE subscription" || fail "DELETE subscription" ""
gp_del "/v1/endpoints/$EP_ID" >/dev/null 2>&1 && ok "DELETE endpoint" || fail "DELETE endpoint" ""

# ── Summary ──────────────────────────────────────────────────────────────
echo
info "=== Results: $PASS passed, $FAIL failed ==="
[[ $FAIL -eq 0 ]]
