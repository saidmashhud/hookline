# GatePulse

**GatePulse** is an open-source webhook delivery service — a self-hosted alternative to Svix/Hookdeck. It accepts events, durably stores them in a custom C-based append-only log, and delivers them to your HTTP endpoints with retries, signatures, and replay.

## Features

- **At-least-once delivery** with exponential backoff and jitter
- **Per-endpoint concurrency** (`max_in_flight`) and **rate limiting** (`rate_limit_rps`)
- **HMAC-SHA256 signatures** on every webhook request
- **Dead Letter Queue** — failed events stored, inspectable, and requeueable
- **Replay** — re-deliver events by `event_id`, time range, topic, or endpoint
- **Real-time SSE stream** — subscribe to events with topic filtering and `Last-Event-ID` resume
- **Dev Inbox** — built-in test endpoint for local development
- **Prometheus metrics** at `/metrics`
- **Single Docker container** — no external dependencies

---

## Quickstart

```bash
# Start GatePulse
docker-compose up -d

# Create a webhook endpoint
curl -X POST http://localhost:8080/v1/endpoints \
  -H "Authorization: Bearer dev-secret" \
  -H "Content-Type: application/json" \
  -d '{"url":"http://localhost:3001/webhook","enabled":true,"secret":"my-secret"}'

# Create a subscription (endpoint → topic)
curl -X POST http://localhost:8080/v1/subscriptions \
  -H "Authorization: Bearer dev-secret" \
  -H "Content-Type: application/json" \
  -d '{"endpoint_id":"<endpoint_id>","topic_pattern":"orders.#"}'

# Publish an event
curl -X POST http://localhost:8080/v1/events \
  -H "Authorization: Bearer dev-secret" \
  -H "Content-Type: application/json" \
  -d '{"topic":"orders.created","payload":{"order_id":"123","amount":99}}'

# Check delivery attempts
curl "http://localhost:8080/v1/deliveries?event_id=<event_id>" \
  -H "Authorization: Bearer dev-secret"
```

---

## Dev Inbox

Create a local webhook receiver without any external service:

```bash
# Create an inbox
INBOX=$(curl -s -X POST http://localhost:8080/v1/dev/inbox \
  -H "Authorization: Bearer dev-secret" -d '{}')
TOKEN=$(echo $INBOX | jq -r .token)
RECEIVE_URL=$(echo $INBOX | jq -r .receive_url)

# Use the receive URL as your endpoint URL
# View received webhooks
curl "http://localhost:8080/v1/dev/inbox/messages?token=$TOKEN" \
  -H "Authorization: Bearer dev-secret" | jq .
```

---

## CLI

The `bin/gp` shell script wraps the API:

```bash
export GP_URL=http://localhost:8080
export GP_API_KEY=dev-secret

gp endpoints add --url https://my.app/hooks --secret s3cr3t
gp subs add --endpoint <id> --topic "orders.#"
gp events publish --topic orders.created --payload '{"id":1}'
gp deliveries ls --event <event_id>
gp dlq ls
gp replay --event <event_id>
gp stream --topic "orders.#"
gp inbox create
```

---

## Webhook Signature Verification

Every request includes:
- `X-GP-Event-Id` — unique event identifier
- `X-GP-Topic` — event topic
- `X-GP-Delivery-Id` — delivery attempt identifier
- `X-GP-Timestamp` — Unix timestamp in milliseconds
- `X-GP-Signature` — HMAC-SHA256 signature

**Signature format:** `v1=<hex_hmac_sha256(secret, "{timestamp}.{body}")>`

### Node.js
```javascript
const crypto = require('crypto');

function verifySignature(secret, timestamp, body, signature) {
  const expected = 'v1=' + crypto
    .createHmac('sha256', secret)
    .update(`${timestamp}.${body}`)
    .digest('hex');
  return crypto.timingSafeEqual(
    Buffer.from(signature),
    Buffer.from(expected)
  );
}

app.post('/webhook', (req, res) => {
  const ts  = req.headers['x-gp-timestamp'];
  const sig = req.headers['x-gp-signature'];
  if (!verifySignature('my-secret', ts, req.body, sig)) {
    return res.status(401).send('Unauthorized');
  }
  // Process idempotently using x-gp-event-id
  const eventId = req.headers['x-gp-event-id'];
  // ... handle event
  res.sendStatus(200);
});
```

### Python
```python
import hmac, hashlib

def verify_signature(secret: str, timestamp: str, body: bytes, signature: str) -> bool:
    msg = f"{timestamp}.".encode() + body
    expected = "v1=" + hmac.new(secret.encode(), msg, hashlib.sha256).hexdigest()
    return hmac.compare_digest(expected, signature)

@app.route('/webhook', methods=['POST'])
def webhook():
    ts  = request.headers.get('X-GP-Timestamp')
    sig = request.headers.get('X-GP-Signature')
    if not verify_signature('my-secret', ts, request.data, sig):
        return 'Unauthorized', 401
    event_id = request.headers.get('X-GP-Event-Id')
    # Idempotency: skip if event_id already processed
    return 'OK', 200
```

---

## Configuration

| Variable | Default | Description |
|----------|---------|-------------|
| `GP_LISTEN_ADDR` | `:8080` | HTTP listen address |
| `GP_API_KEY` | `dev-secret` | API key (single-tenant mode) |
| `GP_TENANT_ID` | `default` | Tenant ID (single-tenant mode) |
| `GP_SINGLE_TENANT` | `true` | Single vs multi-tenant mode |
| `GP_STORE_SOCKET` | `/tmp/gp_store.sock` | Path to C store daemon socket |
| `GP_STORE_DATA_DIR` | `/var/lib/gatepulse` | Storage directory |
| `GP_RETRY_MAX_ATTEMPTS` | `10` | Max delivery attempts |
| `GP_RETENTION_SECS` | `604800` | Event retention (7 days) |
| `GP_DELIVERY_WORKERS` | `20` | Concurrent delivery workers |

---

## API Reference

Full OpenAPI spec available at **`/openapi.yaml`** when the server is running, or in [`openapi/openapi.yaml`](openapi/openapi.yaml).

Key endpoints:

| Method | Path | Description |
|--------|------|-------------|
| `POST` | `/v1/events` | Publish an event |
| `GET`  | `/v1/events` | List events (paginated) |
| `GET`  | `/v1/events/:id` | Get event by ID |
| `POST` | `/v1/endpoints` | Create endpoint |
| `GET`  | `/v1/endpoints` | List endpoints |
| `PATCH`| `/v1/endpoints/:id` | Update endpoint |
| `DELETE`| `/v1/endpoints/:id` | Delete endpoint |
| `POST` | `/v1/subscriptions` | Create subscription |
| `GET`  | `/v1/subscriptions` | List subscriptions |
| `DELETE`| `/v1/subscriptions/:id` | Delete subscription |
| `GET`  | `/v1/deliveries` | List delivery attempts |
| `GET`  | `/v1/deliveries/:id` | Get attempt by ID |
| `GET`  | `/v1/dlq` | List DLQ entries |
| `POST` | `/v1/dlq/:id/requeue` | Requeue DLQ entry |
| `DELETE`| `/v1/dlq/:id` | Delete DLQ entry |
| `POST` | `/v1/replay` | Replay events |
| `GET`  | `/v1/replay/:id` | Replay status |
| `GET`  | `/v1/stream` | SSE event stream |
| `GET`  | `/healthz` | Liveness probe |
| `GET`  | `/readyz` | Readiness probe |
| `GET`  | `/metrics` | Prometheus metrics |

---

## Architecture

```
┌─────────────────────────────────────────────────────┐
│                   GatePulse (Erlang/OTP)            │
│                                                     │
│  gp_api ──► gp_core ──► gp_store_client ──┐        │
│     │                                      │        │
│     └──► gp_stream (SSE)      gp_delivery ─┤        │
│                                            │        │
│  gp_dev_inbox (ETS)                        │        │
└────────────────────────────────────────────┼────────┘
                                             │ UNIX socket
                                             ▼
                              ┌─────────────────────────┐
                              │   gp_store (C daemon)   │
                              │                         │
                              │  append-only log        │
                              │  hash index             │
                              │  job queue              │
                              │  DLQ                    │
                              └─────────────────────────┘
```

**gp_api** — Cowboy HTTP server, auth middleware, all REST handlers
**gp_core** — validation, topic matching, delivery job creation
**gp_delivery** — poller + workers: claim → HTTP POST → ack/nack/DLQ
**gp_stream** — SSE loop handler with pubsub and replay
**gp_store** — C11 daemon: segmented append-only log with CRC32 integrity

---

## Development

**Requirements:** Erlang/OTP 28+, rebar3, GCC, Docker

```bash
# Build C daemon
cd c && make

# Run C tests
cd c && make test && ./build/gp_store_tests

# Build Erlang
rebar3 compile

# Run Erlang unit tests
rebar3 eunit

# Run integration tests (requires running instance)
./test/integration.sh

# Start locally
docker-compose up
```

---

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md).

## Security

See [SECURITY.md](SECURITY.md) for reporting vulnerabilities.

## License

Apache-2.0 — see [LICENSE](LICENSE).
