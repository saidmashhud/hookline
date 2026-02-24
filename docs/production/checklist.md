# Production Checklist

Use this checklist before going live with HookLine.

## Environment Variables

### Required

| Variable | Description | Example |
|----------|-------------|---------|
| `HL_API_KEY` | Master API key — treat as a secret | `openssl rand -hex 32` |
| `HL_DATA_DIR` | Persistent volume path | `/var/lib/hookline` |

### Recommended

| Variable | Default | Recommended |
|----------|---------|-------------|
| `HL_TENANT_ID` | `default` | Your org slug |
| `HL_DELIVERY_WORKERS` | `16` | CPU count × 2 |
| `HL_STORE_POOL_SIZE` | `8` | 8–16 |
| `HL_MAX_PAYLOAD_BYTES` | `524288` | Keep at 512 KB |
| `HL_MAX_QUEUE_DEPTH` | `100000` | Tune to your throughput |
| `HL_RETENTION_SECS` | `604800` | 7–30 days |
| `HL_COMPACT_INTERVAL_MS` | `3600000` | 1h |
| `HL_MASTER_KEY` | — | Set for secret encryption (v2.0) |

### Full Reference

```bash
HL_PORT=8080
HL_LISTEN_ADDR=0.0.0.0
HL_API_KEY=<strong-random-key>
HL_TENANT_ID=your-org
HL_SINGLE_TENANT=true

# Store
HL_STORE_SOCKET=/run/hookline/gp_store.sock
HL_DATA_DIR=/var/lib/hookline
HL_STORE_POOL_SIZE=8

# Delivery
HL_DELIVERY_WORKERS=16
HL_MAX_PAYLOAD_BYTES=524288
HL_MAX_QUEUE_DEPTH=100000
HL_MAX_INFLIGHT_GLOBAL=500
HL_RETENTION_SECS=604800
HL_COMPACT_INTERVAL_MS=3600000

# Encryption (v2.0)
HL_MASTER_KEY=<32-byte-hex>
```

## Infrastructure Checklist

- [ ] `HL_DATA_DIR` is on a **persistent volume** (not ephemeral container storage)
- [ ] Volume has at least **50 GB** free (or autoscales)
- [ ] Container restart policy is `unless-stopped` or `always`
- [ ] `/healthz` and `/readyz` are wired to load balancer health checks
- [ ] Prometheus scrape configured for port `8080/metrics`
- [ ] Grafana dashboard imported (`grafana/dashboards/hookline.json`)
- [ ] Alert rules loaded (`prometheus/alerts.yml`)
- [ ] Firewall: port `8080` accessible only from trusted networks

## Backup

- [ ] Hourly backup cron configured (`gp store snapshot create`)
- [ ] Daily full backup to object storage (S3/GCS)
- [ ] Restore procedure tested at least once
- [ ] See [`backup.md`](backup.md) for full instructions

## Security

- [ ] `HL_API_KEY` is not the default (`change-me` / `dev-secret`)
- [ ] `HL_API_KEY` is injected via secrets manager (not hardcoded in yaml)
- [ ] Endpoint `secret` fields set for HMAC verification on consumer side
- [ ] `HL_MASTER_KEY` set and endpoint secrets encrypted (v2.0)
- [ ] TLS terminated at ingress/load balancer
- [ ] `/v1/admin/*` endpoints not exposed publicly (restrict at network level)

## Monitoring Alerts

Minimum alerts to configure (see `prometheus/alerts.yml`):

| Alert | Threshold | Action |
|-------|-----------|--------|
| DLQ growing | Any increase over 10m | Check failing endpoints |
| Queue depth | > 80k pending | Scale up workers or fix consumer |
| Failure rate | > 5% | Check endpoint health |
| P99 latency | > 10s | Check endpoint response times |
| Overload drops | Any rate | Immediate — events being lost |

## Pre-go-live Test

Run the e2e test suite against your production-like environment:

```bash
HL_URL=https://your-hookline.example.com \
HL_API_KEY=your-production-key \
bash test/e2e.sh
```

All 16 scenarios must pass (SSE scenario may be skipped in restricted networks).

Run the production-readiness gate (load + soak + chaos):

```bash
HL_URL=https://your-hookline.example.com \
HL_AUTH_MODE=api_key \
HL_API_KEY=your-production-key \
bash test/production-readiness.sh
```

For full release decision criteria (including rollback/restore + DR drills), use
[`readiness-gate.md`](readiness-gate.md).

## Capacity Planning

| Metric | Single node (16 workers) |
|--------|--------------------------|
| Ingest | ~5,000 events/s |
| Delivery | ~800 HTTP POST/s (20ms endpoint latency) |
| Storage | ~1 KB/event → 50 GB ≈ 50M events |
| Memory | ~256 MB Erlang + ~128 MB C store |

For higher throughput: increase `HL_DELIVERY_WORKERS`, run multiple API nodes (v1.3 HA), or use Postgres backend (v2.0).
