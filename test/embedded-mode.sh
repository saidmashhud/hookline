#!/usr/bin/env bash
# HookLine embedded mode integration tests
#
# Tests scope restrictions when HL_EMBEDDED_MODE=true with service_token auth.
#
# Usage:
#   HL_URL=http://localhost:8080 \
#   HL_SERVICE_TOKEN=super-secret \
#   ./test/embedded-mode.sh
#
# Prerequisites:
#   HookLine running with:
#     HL_AUTH_MODE=service_token
#     HL_SERVICE_TOKEN=<same as above>
#     HL_SINGLE_TENANT=false
#     HL_EMBEDDED_MODE=true

set -euo pipefail

HL_URL="${HL_URL:-http://localhost:8080}"
HL_SERVICE_TOKEN="${HL_SERVICE_TOKEN:-super-secret}"
TENANT_ID="${HL_TENANT_ID:-embed-test-tenant}"
ALT_TENANT_ID="${HL_ALT_TENANT_ID:-${TENANT_ID}-alt}"
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

check_status() {
  local name="$1"; local actual="$2"; local expected="$3"
  if [[ "$actual" == "$expected" ]]; then
    ok "$name"
  else
    fail "$name" "expected status $expected, got $actual"
  fi
}

hl_get() {
  curl -sf -H "Authorization: Bearer $HL_SERVICE_TOKEN" \
    -H "X-Tenant-Id: $TENANT_ID" "${HL_URL}$1"
}

hl_post() {
  curl -sf -X POST -H "Authorization: Bearer $HL_SERVICE_TOKEN" \
    -H "X-Tenant-Id: $TENANT_ID" \
    -H "Content-Type: application/json" -d "$2" "${HL_URL}$1"
}

hl_del() {
  curl -sf -X DELETE -H "Authorization: Bearer $HL_SERVICE_TOKEN" \
    -H "X-Tenant-Id: $TENANT_ID" "${HL_URL}$1"
}

hl_status() {
  curl -s -o /dev/null -w "%{http_code}" -H "Authorization: Bearer $HL_SERVICE_TOKEN" \
    -H "X-Tenant-Id: $TENANT_ID" "$@"
}

hl_get_tenant() {
  local tenant="$1"; shift
  curl -sf -H "Authorization: Bearer $HL_SERVICE_TOKEN" \
    -H "X-Tenant-Id: $tenant" "${HL_URL}$1"
}

hl_post_tenant() {
  local tenant="$1"; shift
  curl -sf -X POST -H "Authorization: Bearer $HL_SERVICE_TOKEN" \
    -H "X-Tenant-Id: $tenant" \
    -H "Content-Type: application/json" -d "$2" "${HL_URL}$1"
}

hl_status_tenant() {
  local tenant="$1"; shift
  curl -s -o /dev/null -w "%{http_code}" \
    -H "Authorization: Bearer $HL_SERVICE_TOKEN" \
    -H "X-Tenant-Id: $tenant" "$@"
}

info "=== HookLine Embedded Mode Tests ==="
info "Target: $HL_URL"
info "Tenant: $TENANT_ID"
echo

# ── Verify embedded mode is active ────────────────────────────────────────
info "--- Embedded Mode Status ---"
HEALTH=$(curl -sf "${HL_URL}/v1/health/embedded" 2>/dev/null || echo '{}')
check "GET /v1/health/embedded" "$HEALTH" '"embedded"'

# ── Data-plane operations (should be ALLOWED) ─────────────────────────────
info "--- Allowed: Data-plane operations ---"

# events.publish
EV=$(hl_post "/v1/events" '{"topic":"embed.test","payload":{"key":"value"}}' 2>/dev/null || echo '{}')
check "POST /v1/events (events.publish)" "$EV" "event_id"

# endpoints.read
STATUS=$(hl_status "${HL_URL}/v1/endpoints" 2>/dev/null || echo "000")
check_status "GET /v1/endpoints (endpoints.read)" "$STATUS" "200"

# endpoints.write
EP=$(hl_post "/v1/endpoints" '{"url":"http://example.com/test-hook","enabled":true}' 2>/dev/null || echo '{}')
check "POST /v1/endpoints (endpoints.write)" "$EP" "endpoint_id"
EP_ID=$(echo "$EP" | python3 -c "import sys,json; print(json.load(sys.stdin).get('endpoint_id',''))" 2>/dev/null || echo "")

# subscriptions.read
STATUS=$(hl_status "${HL_URL}/v1/subscriptions" 2>/dev/null || echo "000")
check_status "GET /v1/subscriptions (subscriptions.read)" "$STATUS" "200"

# deliveries.read
STATUS=$(hl_status "${HL_URL}/v1/deliveries" 2>/dev/null || echo "000")
check_status "GET /v1/deliveries (deliveries.read)" "$STATUS" "200"

# dlq.read
STATUS=$(hl_status "${HL_URL}/v1/dlq" 2>/dev/null || echo "000")
check_status "GET /v1/dlq (dlq.read)" "$STATUS" "200"

# ── Admin operations (should be BLOCKED in embedded mode) ─────────────────
info "--- Blocked: Admin operations ---"

# GET /v1/tenants → requires admin scope
STATUS=$(hl_status "${HL_URL}/v1/tenants" 2>/dev/null || echo "000")
check_status "GET /v1/tenants → 403" "$STATUS" "403"

# POST /v1/admin/compact → requires admin scope
STATUS=$(curl -s -o /dev/null -w "%{http_code}" -X POST \
  -H "Authorization: Bearer $HL_SERVICE_TOKEN" \
  -H "X-Tenant-Id: $TENANT_ID" \
  -H "Content-Type: application/json" \
  "${HL_URL}/v1/admin/store/pause-claims" -d '{}' 2>/dev/null || echo "000")
check_status "POST /v1/admin/store/pause-claims → 403" "$STATUS" "403"

# GET /v1/admin/stats → requires admin scope
STATUS=$(hl_status "${HL_URL}/v1/admin/stats" 2>/dev/null || echo "000")
check_status "GET /v1/admin/stats → 403" "$STATUS" "403"

# GET /v1/admin/audit → requires admin scope
STATUS=$(hl_status "${HL_URL}/v1/admin/audit" 2>/dev/null || echo "000")
check_status "GET /v1/admin/audit → 403" "$STATUS" "403"

# POST /v1/apikeys → requires admin scope
STATUS=$(curl -s -o /dev/null -w "%{http_code}" -X POST \
  -H "Authorization: Bearer $HL_SERVICE_TOKEN" \
  -H "X-Tenant-Id: $TENANT_ID" \
  -H "Content-Type: application/json" \
  "${HL_URL}/v1/apikeys" -d '{"scopes":["*"]}' 2>/dev/null || echo "000")
check_status "POST /v1/apikeys → 403" "$STATUS" "403"

# ── Console lockdown ─────────────────────────────────────────────────────
info "--- Console lockdown ---"

# In embedded mode, /console should not be publicly accessible
STATUS=$(curl -s -o /dev/null -w "%{http_code}" "${HL_URL}/console" 2>/dev/null || echo "000")
# Should be 401 or 403 (not 200) in embedded mode
if [[ "$STATUS" == "200" ]]; then
  fail "GET /console" "should not be 200 in embedded mode"
else
  ok "GET /console blocked in embedded mode (status: $STATUS)"
fi

# ── Cross-tenant isolation (P0 tenant safety) ───────────────────────────
info "--- Cross-tenant isolation ---"
info "Tenant A: $TENANT_ID"
info "Tenant B: $ALT_TENANT_ID"
if [[ "$TENANT_ID" == "$ALT_TENANT_ID" ]]; then
  fail "Cross-tenant setup" "HL_ALT_TENANT_ID must differ from HL_TENANT_ID"
fi

INBOX_A=$(hl_post_tenant "$TENANT_ID" "/v1/dev/inbox" '{}' 2>/dev/null || echo '{}')
TOKEN_A=$(echo "$INBOX_A" | python3 -c "import sys,json; print(json.load(sys.stdin).get('token',''))" 2>/dev/null || echo "")
RECV_A=$(echo "$INBOX_A" | python3 -c "import sys,json; print(json.load(sys.stdin).get('receive_url',''))" 2>/dev/null || echo "")

if [[ -z "$TOKEN_A" || -z "$RECV_A" ]]; then
  fail "Setup tenant A inbox" "failed to create dev inbox"
else
  EP_A=$(hl_post_tenant "$TENANT_ID" "/v1/endpoints" \
    "{\"url\":\"$RECV_A\",\"enabled\":true,\"timeout_ms\":3000}" 2>/dev/null || echo '{}')
  EP_A_ID=$(echo "$EP_A" | python3 -c "import sys,json; print(json.load(sys.stdin).get('endpoint_id',''))" 2>/dev/null || echo "")
  if [[ -z "$EP_A_ID" ]]; then
    fail "Setup tenant A endpoint" "failed to create endpoint"
  else
    STATUS=$(hl_status_tenant "$ALT_TENANT_ID" "${HL_URL}/v1/endpoints/$EP_A_ID" 2>/dev/null || echo "000")
    check_status "Tenant B GET /v1/endpoints/:id (tenant A endpoint) -> 404" "$STATUS" "404"

    SUB_A=$(hl_post_tenant "$TENANT_ID" "/v1/subscriptions" \
      "{\"endpoint_id\":\"$EP_A_ID\",\"topic_pattern\":\"tenant.safety.#\"}" 2>/dev/null || echo '{}')
    SUB_A_ID=$(echo "$SUB_A" | python3 -c "import sys,json; print(json.load(sys.stdin).get('subscription_id',''))" 2>/dev/null || echo "")

    if [[ -z "$SUB_A_ID" ]]; then
      fail "Setup tenant A subscription" "failed to create subscription"
    else
      STATUS=$(hl_status_tenant "$ALT_TENANT_ID" -X DELETE "${HL_URL}/v1/subscriptions/$SUB_A_ID" 2>/dev/null || echo "000")
      check_status "Tenant B DELETE /v1/subscriptions/:id (tenant A sub) -> 404" "$STATUS" "404"

      EV_A=$(hl_post_tenant "$TENANT_ID" "/v1/events" \
        '{"topic":"tenant.safety.created","payload":{"case":"tenant-isolation"}}' 2>/dev/null || echo '{}')
      EV_A_ID=$(echo "$EV_A" | python3 -c "import sys,json; print(json.load(sys.stdin).get('event_id',''))" 2>/dev/null || echo "")
      if [[ -n "$EV_A_ID" ]]; then
        ATTEMPT_A_ID=""
        for i in $(seq 1 10); do
          sleep 1
          DEL_A=$(hl_get_tenant "$TENANT_ID" "/v1/deliveries?event_id=$EV_A_ID" 2>/dev/null || echo '{"items":[]}')
          ATTEMPT_A_ID=$(echo "$DEL_A" | python3 -c \
            "import sys,json; items=json.load(sys.stdin).get('items',[]); print(items[0].get('attempt_id','') if items else '')" \
            2>/dev/null || echo "")
          [[ -n "$ATTEMPT_A_ID" ]] && break
        done
        if [[ -n "$ATTEMPT_A_ID" ]]; then
          STATUS=$(hl_status_tenant "$ALT_TENANT_ID" "${HL_URL}/v1/deliveries/$ATTEMPT_A_ID" 2>/dev/null || echo "000")
          check_status "Tenant B GET /v1/deliveries/:id (tenant A attempt) -> 404" "$STATUS" "404"
        else
          fail "Tenant isolation deliveries" "attempt_id for tenant A event not found"
        fi
      else
        fail "Tenant A publish event" "failed to create tenant safety event"
      fi
    fi

    STATUS=$(hl_status_tenant "$ALT_TENANT_ID" -X DELETE "${HL_URL}/v1/endpoints/$EP_A_ID" 2>/dev/null || echo "000")
    check_status "Tenant B DELETE /v1/endpoints/:id (tenant A endpoint) -> 404" "$STATUS" "404"

    [[ -n "${SUB_A_ID:-}" ]] && hl_status_tenant "$TENANT_ID" -X DELETE "${HL_URL}/v1/subscriptions/$SUB_A_ID" >/dev/null 2>&1 || true
    hl_status_tenant "$TENANT_ID" -X DELETE "${HL_URL}/v1/endpoints/$EP_A_ID" >/dev/null 2>&1 || true
  fi
fi

# ── Cleanup ──────────────────────────────────────────────────────────────
info "--- Cleanup ---"
if [[ -n "$EP_ID" ]]; then
  hl_del "/v1/endpoints/$EP_ID" >/dev/null 2>&1 && ok "DELETE test endpoint" || fail "DELETE test endpoint" "failed"
fi

# ── Summary ──────────────────────────────────────────────────────────────
echo
info "=== Results: $PASS passed, $FAIL failed ==="
[[ $FAIL -eq 0 ]]
