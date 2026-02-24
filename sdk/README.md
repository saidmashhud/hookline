# HookLine SDKs

Client libraries for the [HookLine](https://github.com/saidmashhud/hookline) webhook delivery platform.

## Available SDKs

| Language | Directory | Package | Version | Status |
|----------|-----------|---------|---------|--------|
| Go | [go/](./go/) | `github.com/hookline/hookline-go` | 0.1.0 | Alpha |
| JavaScript / TypeScript | [js/](./js/) | `hookline` (npm) | 0.1.0 | Alpha |
| Python | [python/](./python/) | `hookline` (PyPI) | 0.1.0 | Alpha |
| Scala 3 (ZIO 2) | [scala/](./scala/) | `io.hookline:hookline-sdk` | 0.2.0 | Alpha |

Every SDK provides:

- **Client** -- create endpoints, publish events, manage subscriptions and deliveries.
- **Webhook verification** -- validate `HookLine-Signature` headers with HMAC-SHA256.

## Getting Started

See the [examples/](./examples/) directory for runnable demos:

| Example | Stack | Description |
|---------|-------|-------------|
| [express](./examples/express/) | Node.js + Express | Receive and verify webhooks |
| [fastapi](./examples/fastapi/) | Python + FastAPI | Receive and verify webhooks |
| [go-chi](./examples/go-chi/) | Go + chi | Receive and verify webhooks |

Each SDK also has inline examples in its own directory (e.g. `js/examples/`, `python/examples/`).
