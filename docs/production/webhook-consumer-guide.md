# Webhook Consumer Guide

This guide explains how to receive and process webhooks delivered by HookLine.

## Request format

HookLine delivers webhooks as `POST` requests with the following headers:

| Header | Description |
|--------|-------------|
| `Content-Type` | `application/json` |
| `x-gp-event-id` | Unique event ID |
| `x-gp-topic` | Event topic (e.g. `orders.created`) |
| `x-gp-tenant-id` | Tenant identifier |
| `x-gp-timestamp` | Unix millisecond timestamp |
| `x-gp-signature` | HMAC-SHA256 signature (if endpoint has a secret) |
| `x-gp-attempt` | Delivery attempt number (1-based) |

The body is the raw JSON event payload.

## Verifying signatures

When an endpoint has a `secret` configured, HookLine signs each delivery:

```
x-gp-signature: v1=<hex-encoded HMAC-SHA256>
```

The signature is computed as:

`HMAC-SHA256(secret, "{x-gp-timestamp}.{raw_body}")`

### Node.js

```javascript
const crypto = require("crypto");

function verifySignature(rawBody, timestamp, signatureHeader, secret) {
  const signedPayload = `${timestamp}.${rawBody}`;
  const expected = "v1=" + crypto
    .createHmac("sha256", secret)
    .update(signedPayload)
    .digest("hex");
  return crypto.timingSafeEqual(
    Buffer.from(signatureHeader),
    Buffer.from(expected)
  );
}

// In your Express handler:
app.post("/webhook", express.raw({ type: "*/*" }), (req, res) => {
  const ts = req.headers["x-gp-timestamp"];
  const sig = req.headers["x-gp-signature"];
  if (!ts || !sig || !verifySignature(req.body, ts, sig, process.env.WEBHOOK_SECRET)) {
    return res.status(401).json({ error: "invalid signature" });
  }
  const event = JSON.parse(req.body);
  // ... process event
  res.json({ ok: true });
});
```

### Python

```python
import hmac, hashlib

def verify_signature(raw_body: bytes, timestamp: str, signature_header: str, secret: str) -> bool:
    signed_payload = f"{timestamp}.".encode() + raw_body
    expected = "v1=" + hmac.new(
        secret.encode(), signed_payload, hashlib.sha256
    ).hexdigest()
    return hmac.compare_digest(expected, signature_header)

# In your Flask/FastAPI handler:
@app.post("/webhook")
async def webhook(request: Request):
    body = await request.body()
    ts = request.headers.get("x-gp-timestamp", "")
    sig = request.headers.get("x-gp-signature", "")
    if not ts or not verify_signature(body, ts, sig, os.environ["WEBHOOK_SECRET"]):
        return JSONResponse({"error": "invalid signature"}, status_code=401)
    event = json.loads(body)
    # ... process event
    return {"ok": True}
```

## Responding correctly

- Return `2xx` within **30 seconds** to acknowledge delivery.
- Any non-2xx response or timeout causes HookLine to retry.
- Return the same 2xx on duplicate deliveries (idempotent handling).

## Retry schedule

| Attempt | Delay |
|---------|-------|
| 1 | Immediate |
| 2 | ~2s |
| 3 | ~4s |
| 4 | ~8s |
| 5 | ~16s |
| ... | Exponential backoff with full jitter, capped at 1 hour |

After `max_attempts` (default: 10) the job moves to the Dead Letter Queue.

## Handling duplicates

HookLine delivers **at least once**. Always implement idempotent handlers:

```javascript
// Store processed event IDs in your database
const eventId = req.headers["x-gp-event-id"];
if (await db.eventAlreadyProcessed(eventId)) {
  return res.json({ ok: true, skipped: true });
}
await db.processEvent(event);
await db.markEventProcessed(eventId);
```

## Local development with Dev Inbox

During development, use the built-in Dev Inbox instead of a real HTTPS endpoint:

```bash
# Start HookLine
docker-compose up -d

# Create an inbox
curl -X POST http://localhost:8080/v1/dev/inbox \
  -H "Authorization: Bearer dev-secret" | jq .

# Visit the UI
open http://localhost:8080/v1/dev/inbox/ui
```

Create an endpoint pointing at your inbox's receive URL, then publish events —
they appear in the UI within 2 seconds.
