# Changelog

All notable changes to GatePulse will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Initial release of GatePulse
- C11 append-only segmented log storage engine (`gp_store` daemon)
- Erlang/OTP 27 umbrella project with 6 apps
- HTTP API (Cowboy 2.x): events, endpoints, subscriptions, deliveries, DLQ, replay
- SSE stream (`/v1/stream`) with topic pattern filtering
- Developer inbox for webhook testing (`/v1/dev/inbox`)
- Exponential backoff with full jitter retry strategy
- HMAC-SHA256 webhook signing
- Prometheus metrics endpoint
- OpenAPI 3.0 specification
- Docker + docker-compose deployment
- GitHub Actions CI/CD (build, test, release)
- MkDocs Material documentation
