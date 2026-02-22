#!/usr/bin/env bash
# GatePulse integration smoke test
# Usage: GP_URL=http://localhost:8080 GP_API_KEY=dev-secret ./test/integration.sh
set -euo pipefail

GP_URL="${GP_URL:-http://localhost:8080}"
GP_API_KEY="${GP_API_KEY:-dev-secret}"
PASS=0; FAIL=0

red()   { printf '\033[31m%s\033[0m\n' "$*"; }
green() { printf '\033[32m%s\033[0m\n' "$*"; }
info()  { printf '\033[36m%s\033[0m\n' "$*"; }

ok()   { PASS=$((PASS+1)); green "  PASS: $1"; }
fail() { FAIL=$((FAIL+1)); red   "  FAIL: $1 — $2"; }

gp_get()  { curl -sf -H "Authorization: Bearer $GP_API_KEY" "${GP_URL}$1"; }
gp_post() { curl -sf -X POST -H "Authorization: Bearer $GP_API_KEY" \
              -H "Content-Type: application/json" -d "$2" "${GP_URL}$1"; }
gp_del()  { curl -sf -X DELETE -H "Authorization: Bearer $GP_API_KEY" "${GP_URL}$1"; }

check() {
  local name="$1"; local actual="$2"; local expected="$3"
  if echo "$actual" | grep -q "$expected"; then
    ok "$name"
  else
    fail "$name" "expected '$expected' in: $actual"
  fi
}

info "=== GatePulse Integration Tests ==="
info "Target: $GP_URL"
echo

# ── Health ────────────────────────────────────────────────────────────────────
info "--- Health ---"
check "GET /healthz" "$(gp_get /healthz)" '"status"'
check "GET /readyz"  "$(gp_get /readyz)"  '"status"'

# ── Dev inbox (use as delivery target) ────────────────────────────────────────
info "--- Dev inbox ---"
INBOX=$(gp_post "/v1/dev/inbox" '{}')
check "POST /dev/inbox" "$INBOX" "receive_url"
TOKEN=$(echo "$INBOX" | python3 -c "import sys,json; print(json.load(sys.stdin)['token'])")

# ── Endpoint ──────────────────────────────────────────────────────────────────
info "--- Endpoints ---"
EP_BODY=$(cat <<JSON
{"url":"${GP_URL}/v1/dev/inbox/receive/${TOKEN}","enabled":true,"timeout_ms":5000}
JSON
)
EP=$(gp_post "/v1/endpoints" "$EP_BODY")
check "POST /endpoints" "$EP" "endpoint_id"
EP_ID=$(echo "$EP" | python3 -c "import sys,json; print(json.load(sys.stdin)['endpoint_id'])")

check "GET /endpoints" "$(gp_get /v1/endpoints)" "items"
check "GET /endpoints/:id" "$(gp_get /v1/endpoints/$EP_ID)" "$EP_ID"

# ── Subscription ──────────────────────────────────────────────────────────────
info "--- Subscriptions ---"
SUB=$(gp_post "/v1/subscriptions" \
  "{\"endpoint_id\":\"$EP_ID\",\"topic_pattern\":\"test.#\"}")
check "POST /subscriptions" "$SUB" "subscription_id"
SUB_ID=$(echo "$SUB" | python3 -c "import sys,json; print(json.load(sys.stdin)['subscription_id'])")

check "GET /subscriptions" "$(gp_get /v1/subscriptions)" "items"
check "GET /subscriptions?endpoint_id" \
  "$(gp_get "/v1/subscriptions?endpoint_id=$EP_ID")" "$SUB_ID"

# ── Events ────────────────────────────────────────────────────────────────────
info "--- Events ---"
EV=$(gp_post "/v1/events" '{"topic":"test.created","payload":{"order_id":"123"}}')
check "POST /events" "$EV" "event_id"
EV_ID=$(echo "$EV" | python3 -c "import sys,json; print(json.load(sys.stdin)['event_id'])")

check "GET /events" "$(gp_get "/v1/events?limit=5")" "items"
check "GET /events/:id" "$(gp_get "/v1/events/$EV_ID")" "$EV_ID"

# ── Idempotency ───────────────────────────────────────────────────────────────
info "--- Idempotency ---"
EV2=$(gp_post "/v1/events" '{"topic":"test.idem","payload":{},"idempotency_key":"idem-001"}')
EV3=$(gp_post "/v1/events" '{"topic":"test.idem","payload":{},"idempotency_key":"idem-001"}')
check "Idempotency duplicate returns same id" \
  "$(echo "$EV3" | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('event_id',''))")" \
  "$(echo "$EV2" | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('event_id',''))")"

# ── Wait for delivery ─────────────────────────────────────────────────────────
info "--- Waiting for delivery (up to 10s) ---"
DELIVERED=0
for i in $(seq 1 10); do
  sleep 1
  MSGS=$(gp_get "/v1/dev/inbox/messages?token=$TOKEN" 2>/dev/null || echo '{"items":[]}')
  COUNT=$(echo "$MSGS" | python3 -c "import sys,json; print(len(json.load(sys.stdin).get('items',[])))")
  if [[ "$COUNT" -gt 0 ]]; then
    DELIVERED=1
    ok "Webhook delivered (${COUNT} msg(s) in inbox after ${i}s)"
    break
  fi
done
[[ $DELIVERED -eq 0 ]] && fail "Webhook delivery" "no messages in inbox after 10s"

# ── Deliveries API ────────────────────────────────────────────────────────────
info "--- Deliveries ---"
DELIV=$(gp_get "/v1/deliveries?event_id=$EV_ID")
check "GET /deliveries?event_id" "$DELIV" "items"
ATTEMPT_ID=$(echo "$DELIV" | python3 -c \
  "import sys,json; items=json.load(sys.stdin).get('items',[]); print(items[0].get('attempt_id','') if items else '')")
if [[ -n "$ATTEMPT_ID" ]]; then
  check "GET /deliveries/:id" "$(gp_get "/v1/deliveries/$ATTEMPT_ID")" "$ATTEMPT_ID"
fi

# ── Replay ────────────────────────────────────────────────────────────────────
info "--- Replay ---"
REPLAY=$(gp_post "/v1/replay" "{\"event_id\":\"$EV_ID\"}")
check "POST /replay" "$REPLAY" "replay_id"
REPLAY_ID=$(echo "$REPLAY" | python3 -c "import sys,json; print(json.load(sys.stdin).get('replay_id',''))")
if [[ -n "$REPLAY_ID" ]]; then
  check "GET /replay/:id" "$(gp_get "/v1/replay/$REPLAY_ID")" "$REPLAY_ID"
fi

# ── DLQ ───────────────────────────────────────────────────────────────────────
info "--- DLQ ---"
check "GET /dlq" "$(gp_get /v1/dlq)" "items"

# ── Subscription DELETE ────────────────────────────────────────────────────────
info "--- Cleanup ---"
gp_del "/v1/subscriptions/$SUB_ID" >/dev/null 2>&1 && ok "DELETE /subscriptions/:id" || fail "DELETE /subscriptions/:id" ""
gp_del "/v1/endpoints/$EP_ID" >/dev/null 2>&1 && ok "DELETE /endpoints/:id" || fail "DELETE /endpoints/:id" ""

# ── Metrics ───────────────────────────────────────────────────────────────────
info "--- Metrics ---"
check "GET /metrics" "$(curl -sf "${GP_URL}/metrics")" "gatepulse"
check "GET /openapi.yaml" "$(curl -sf "${GP_URL}/openapi.yaml")" "openapi"

# ── Summary ───────────────────────────────────────────────────────────────────
echo
info "=== Results: $PASS passed, $FAIL failed ==="
[[ $FAIL -eq 0 ]]
