#!/usr/bin/env bash
# HookLine production-readiness local gate
# Covers: load burst, short soak, restart-chaos recovery.
#
# Auth modes:
#   api_key:
#     HL_AUTH_MODE=api_key HL_API_KEY=<key> ./test/production-readiness.sh
#   service_token:
#     HL_AUTH_MODE=service_token HL_SERVICE_TOKEN=<token> HL_TENANT_ID=<tenant> ./test/production-readiness.sh
#
# Env knobs:
#   HL_URL=http://localhost:8080
#   LOAD_EVENTS=200
#   LOAD_CONCURRENCY=20
#   SOAK_SECONDS=120
#   SOAK_INTERVAL_SECONDS=1
#   CHAOS_RESTART=true
#   HL_CONTAINER=hookline
set -euo pipefail

HL_URL="${HL_URL:-http://localhost:8080}"
HL_AUTH_MODE="${HL_AUTH_MODE:-api_key}"
LOAD_EVENTS="${LOAD_EVENTS:-200}"
LOAD_CONCURRENCY="${LOAD_CONCURRENCY:-20}"
SOAK_SECONDS="${SOAK_SECONDS:-120}"
SOAK_INTERVAL_SECONDS="${SOAK_INTERVAL_SECONDS:-1}"
CHAOS_RESTART="${CHAOS_RESTART:-true}"
HL_CONTAINER="${HL_CONTAINER:-hookline}"
DELIVERY_WAIT_SECONDS="${DELIVERY_WAIT_SECONDS:-120}"

HL_API_KEY="${HL_API_KEY:-}"
HL_SERVICE_TOKEN="${HL_SERVICE_TOKEN:-}"
HL_TENANT_ID="${HL_TENANT_ID:-}"

PASS=0
FAIL=0
SUB_ID=""
EP_ID=""

red() { printf '\033[31m%s\033[0m\n' "$*"; }
green() { printf '\033[32m%s\033[0m\n' "$*"; }
cyan() { printf '\033[36m%s\033[0m\n' "$*"; }
yellow() { printf '\033[33m%s\033[0m\n' "$*"; }

ok() { PASS=$((PASS+1)); green "PASS: $1"; }
ko() { FAIL=$((FAIL+1)); red "FAIL: $1"; [[ -n "${2:-}" ]] && red "  -> $2"; }

case "$HL_AUTH_MODE" in
  api_key)
    if [[ -z "$HL_API_KEY" ]]; then
      echo "HL_API_KEY is required in api_key mode" >&2
      exit 1
    fi
    ;;
  service_token)
    if [[ -z "$HL_SERVICE_TOKEN" || -z "$HL_TENANT_ID" ]]; then
      echo "HL_SERVICE_TOKEN and HL_TENANT_ID are required in service_token mode" >&2
      exit 1
    fi
    ;;
  *)
    echo "Unsupported HL_AUTH_MODE=$HL_AUTH_MODE" >&2
    exit 1
    ;;
esac

AUTH_ARGS=()
TENANT_ARGS=()
if [[ "$HL_AUTH_MODE" == "api_key" ]]; then
  AUTH_ARGS=(-H "Authorization: Bearer $HL_API_KEY")
else
  AUTH_ARGS=(-H "Authorization: Bearer $HL_SERVICE_TOKEN")
  TENANT_ARGS=(-H "X-Tenant-Id: $HL_TENANT_ID")
fi

api() {
  local method="$1"
  local path="$2"
  local body="${3:-}"
  if [[ -n "$body" ]]; then
    curl -fsS -X "$method" "${AUTH_ARGS[@]}" "${TENANT_ARGS[@]}" -H "Content-Type: application/json" -d "$body" "${HL_URL}${path}"
  else
    curl -fsS -X "$method" "${AUTH_ARGS[@]}" "${TENANT_ARGS[@]}" "${HL_URL}${path}"
  fi
}

json_field() {
  local expr="$1"
  python3 -c "import json,sys; d=json.load(sys.stdin); print(${expr})"
}

count_inbox() {
  local token="$1"
  api GET "/v1/dev/inbox/messages?token=${token}" \
    | python3 -c "import json,sys; d=json.load(sys.stdin); print(len(d.get('items', [])))"
}

count_deliveries() {
  local endpoint_id="$1"
  api GET "/v1/deliveries?endpoint_id=${endpoint_id}&limit=1000" \
    | python3 -c "import json,sys; d=json.load(sys.stdin); print(d.get('count', len(d.get('items', []))))"
}

wait_inbox_at_least() {
  local token="$1"
  local want="$2"
  local timeout="$3"
  local got=0
  for _ in $(seq 1 "$timeout"); do
    sleep 1
    got="$(count_inbox "$token" || echo 0)"
    if [[ "$got" -ge "$want" ]]; then
      echo "$got"
      return 0
    fi
  done
  echo "$got"
  return 1
}

wait_deliveries_at_least() {
  local endpoint_id="$1"
  local want="$2"
  local timeout="$3"
  local got=0
  for _ in $(seq 1 "$timeout"); do
    sleep 1
    got="$(count_deliveries "$endpoint_id" || echo 0)"
    if [[ "$got" -ge "$want" ]]; then
      echo "$got"
      return 0
    fi
  done
  echo "$got"
  return 1
}

cleanup() {
  if [[ -n "$SUB_ID" ]]; then
    api DELETE "/v1/subscriptions/${SUB_ID}" >/dev/null 2>&1 || true
  fi
  if [[ -n "$EP_ID" ]]; then
    api DELETE "/v1/endpoints/${EP_ID}" >/dev/null 2>&1 || true
  fi
}
trap cleanup EXIT

cyan "== HookLine Production Readiness Gate =="
cyan "Target: ${HL_URL}"
cyan "Auth mode: ${HL_AUTH_MODE}"

if curl -fsS "${HL_URL}/healthz" >/dev/null && curl -fsS "${HL_URL}/readyz" >/dev/null; then
  ok "healthz/readyz reachable"
else
  ko "healthz/readyz check"
  exit 1
fi

INBOX="$(api POST "/v1/dev/inbox" '{}')"
TOKEN="$(echo "$INBOX" | json_field "d.get('token', '')")"
RECV_URL="$(echo "$INBOX" | json_field "d.get('receive_url', '')")"
if [[ -n "$TOKEN" && -n "$RECV_URL" ]]; then
  ok "dev inbox provisioned"
else
  ko "dev inbox provisioned" "$INBOX"
  exit 1
fi

EP="$(api POST "/v1/endpoints" "{\"url\":\"${RECV_URL}\",\"enabled\":true,\"timeout_ms\":5000}")"
EP_ID="$(echo "$EP" | json_field "d.get('endpoint_id', '')")"
if [[ -n "$EP_ID" ]]; then
  ok "endpoint created"
else
  ko "endpoint created" "$EP"
  exit 1
fi

SUB="$(api POST "/v1/subscriptions" "{\"endpoint_id\":\"${EP_ID}\",\"topic_pattern\":\"prodgate.#\"}")"
SUB_ID="$(echo "$SUB" | json_field "d.get('subscription_id', '')")"
if [[ -n "$SUB_ID" ]]; then
  ok "subscription created"
else
  ko "subscription created" "$SUB"
  exit 1
fi

cyan "-- Load burst (${LOAD_EVENTS} events, concurrency=${LOAD_CONCURRENCY}) --"
BEFORE_LOAD="$(count_inbox "$TOKEN")"
LOAD_FAILS="$(HL_URL="$HL_URL" HL_AUTH_MODE="$HL_AUTH_MODE" HL_API_KEY="$HL_API_KEY" HL_SERVICE_TOKEN="$HL_SERVICE_TOKEN" HL_TENANT_ID="$HL_TENANT_ID" LOAD_EVENTS="$LOAD_EVENTS" LOAD_CONCURRENCY="$LOAD_CONCURRENCY" python3 - <<'PY'
import concurrent.futures
import json
import os
import urllib.request
import urllib.error

url = os.environ["HL_URL"].rstrip("/")
mode = os.environ["HL_AUTH_MODE"]
events = int(os.environ["LOAD_EVENTS"])
concurrency = int(os.environ["LOAD_CONCURRENCY"])

headers = {"Content-Type": "application/json"}
if mode == "api_key":
    headers["Authorization"] = f"Bearer {os.environ['HL_API_KEY']}"
else:
    headers["Authorization"] = f"Bearer {os.environ['HL_SERVICE_TOKEN']}"
    headers["X-Tenant-Id"] = os.environ["HL_TENANT_ID"]

def publish(i: int) -> bool:
    payload = {
        "topic": "prodgate.load",
        "payload": {"seq": i}
    }
    req = urllib.request.Request(
        f"{url}/v1/events",
        data=json.dumps(payload).encode("utf-8"),
        headers=headers,
        method="POST",
    )
    try:
        with urllib.request.urlopen(req, timeout=20) as resp:
            return 200 <= resp.getcode() < 300
    except Exception:
        return False

fails = 0
with concurrent.futures.ThreadPoolExecutor(max_workers=concurrency) as ex:
    for ok in ex.map(publish, range(1, events + 1)):
        if not ok:
            fails += 1
print(fails)
PY
)"

if [[ "$LOAD_FAILS" == "0" ]]; then
  ok "load publish accepted by API"
else
  ko "load publish accepted by API" "failed requests=${LOAD_FAILS}"
  exit 1
fi

TARGET_LOAD=$((BEFORE_LOAD + LOAD_EVENTS))
if LOAD_GOT="$(wait_inbox_at_least "$TOKEN" "$TARGET_LOAD" "$DELIVERY_WAIT_SECONDS")"; then
  ok "load deliveries observed ($LOAD_GOT >= $TARGET_LOAD)"
else
  ko "load deliveries observed" "got=${LOAD_GOT}, target=${TARGET_LOAD}"
  exit 1
fi

cyan "-- Soak (${SOAK_SECONDS}s, interval=${SOAK_INTERVAL_SECONDS}s) --"
SOAK_EVENTS=0
for sec in $(seq 1 "$SOAK_SECONDS"); do
  BODY="$(printf '{"topic":"prodgate.soak","payload":{"tick":%s}}' "$sec")"
  if api POST "/v1/events" "$BODY" >/dev/null 2>&1; then
    SOAK_EVENTS=$((SOAK_EVENTS + 1))
  else
    ko "soak publish" "failed at tick=${sec}"
    exit 1
  fi
  if (( sec % 10 == 0 )); then
    if curl -fsS "${HL_URL}/readyz" >/dev/null; then
      :
    else
      ko "readyz during soak" "tick=${sec}"
      exit 1
    fi
  fi
  sleep "$SOAK_INTERVAL_SECONDS"
done
ok "soak publish loop completed (${SOAK_EVENTS} events)"

TARGET_SOAK=$((TARGET_LOAD + SOAK_EVENTS))
if SOAK_GOT="$(wait_inbox_at_least "$TOKEN" "$TARGET_SOAK" "$DELIVERY_WAIT_SECONDS")"; then
  ok "soak deliveries observed ($SOAK_GOT >= $TARGET_SOAK)"
else
  ko "soak deliveries observed" "got=${SOAK_GOT}, target=${TARGET_SOAK}"
  exit 1
fi

if [[ "$CHAOS_RESTART" == "true" ]]; then
  cyan "-- Chaos restart (${HL_CONTAINER}) --"
  if command -v docker >/dev/null 2>&1 && docker ps --format '{{.Names}}' | grep -qx "$HL_CONTAINER"; then
    BEFORE_CHAOS="$(count_deliveries "$EP_ID")"
    api POST "/v1/events" '{"topic":"prodgate.chaos","payload":{"phase":"pre-restart"}}' >/dev/null
    PRE_CHAOS_TARGET=$((BEFORE_CHAOS + 1))
    if PRE_CHAOS_GOT="$(wait_deliveries_at_least "$EP_ID" "$PRE_CHAOS_TARGET" "$DELIVERY_WAIT_SECONDS")"; then
      ok "chaos pre-restart delivery observed ($PRE_CHAOS_GOT >= $PRE_CHAOS_TARGET)"
    else
      ko "chaos pre-restart delivery observed" "got=${PRE_CHAOS_GOT}, target=${PRE_CHAOS_TARGET}"
      exit 1
    fi
    docker restart "$HL_CONTAINER" >/dev/null
    for _ in $(seq 1 90); do
      if curl -fsS "${HL_URL}/healthz" >/dev/null; then
        break
      fi
      sleep 1
    done
    api POST "/v1/events" '{"topic":"prodgate.chaos","payload":{"phase":"post-restart"}}' >/dev/null
    TARGET_CHAOS=$((PRE_CHAOS_TARGET + 1))
    if CHAOS_GOT="$(wait_deliveries_at_least "$EP_ID" "$TARGET_CHAOS" "$DELIVERY_WAIT_SECONDS")"; then
      ok "chaos restart recovery deliveries observed ($CHAOS_GOT >= $TARGET_CHAOS)"
    else
      ko "chaos restart recovery deliveries observed" "got=${CHAOS_GOT}, target=${TARGET_CHAOS}"
      exit 1
    fi
  else
    yellow "SKIP: chaos restart (container '${HL_CONTAINER}' not running)"
  fi
fi

echo
cyan "== Gate Result =="
green "PASS: ${PASS}"
if [[ "$FAIL" -gt 0 ]]; then
  red "FAIL: ${FAIL}"
  exit 1
fi
