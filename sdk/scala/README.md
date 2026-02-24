# HookLine Scala SDK

ZIO 2 + Scala 3 client for the [HookLine](https://github.com/saidmashhud/hookline) webhook delivery platform.

## Installation

```scala
// build.sbt
libraryDependencies += "io.hookline" %% "hookline-sdk" % "0.2.0"

resolvers += "GitHub Packages" at "https://maven.pkg.github.com/saidmashhud/hookline"
```

Add GitHub credentials to `~/.sbt/1.0/credentials`:

```
realm=GitHub Package Registry
host=maven.pkg.github.com
user=<your-github-username>
password=<your-github-token>
```

## Quick start

```scala
import io.hookline.*
import io.hookline.models.*
import zio.*
import zio.json.ast.Json

object Main extends ZIOAppDefault:
  def run =
    val program = for
      client <- ZIO.service[HookLineClient]
      event  <- client.publishEvent(
                  "orders.created",
                  Json.Obj(Chunk("order_id" -> Json.Str("ord_123"), "amount" -> Json.Num(99)))
                )
      _      <- Console.printLine(s"Published: ${event.event_id}")
    yield ()

    program.provide(HookLineClient.live("https://api.hookline.io", sys.env("HOOKLINE_API_KEY")))
```

## Webhook signature verification

```scala
import io.hookline.HookLine

// In your HTTP handler:
val valid = HookLine.verifySignature(
  secret    = endpointSecret,
  timestamp = request.header("x-gp-timestamp"),
  signature = request.header("x-gp-signature"),
  body      = rawBodyBytes
)
if !valid then ZIO.fail("Invalid signature")
else handleWebhook(body)
```

## All operations

| Method | Description |
|--------|-------------|
| `publishEvent(topic, payload)` | Publish a new event |
| `getEvent(eventId)` | Fetch event by ID |
| `listEvents(limit)` | List recent events |
| `createEndpoint(req)` | Register a webhook endpoint |
| `getEndpoint(id)` | Fetch endpoint |
| `listEndpoints()` | List all endpoints |
| `updateEndpoint(id, req)` | Update endpoint config |
| `deleteEndpoint(id)` | Delete endpoint |
| `createSubscription(req)` | Subscribe endpoint to topic |
| `getSubscription(id)` | Fetch subscription |
| `listSubscriptions()` | List all subscriptions |
| `deleteSubscription(id)` | Delete subscription |
| `listDeliveries(limit)` | List delivery jobs |
| `listDlq(limit)` | List dead-letter entries |
| `replayEvent(eventId)` | Re-deliver a failed event |
| `createApiKey(req)` | Create API key |
| `listApiKeys()` | List API keys |
| `deleteApiKey(keyId)` | Delete API key |
| `health()` | Health check |

## Error handling

All methods return `IO[HookLineError, A]`:

```scala
client.getEvent("missing").catchAll {
  case HookLineError.NotFound(msg)      => ZIO.logWarning(s"Not found: $msg")
  case HookLineError.Unauthorized(msg)  => ZIO.fail(s"Auth error: $msg")
  case HookLineError.ApiError(code, b)  => ZIO.fail(s"API $code: $b")
  case HookLineError.NetworkError(cause) => ZIO.fail(cause)
}
```

## Building

```bash
cd sdk/scala
sbt compile
sbt test
sbt package
```
