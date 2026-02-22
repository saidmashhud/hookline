# Production Checklist

Use this checklist before going live with GatePulse.

## Environment Variables

### Required

| Variable | Description | Example |
|----------|-------------|---------|
| `GP_API_KEY` | Master API key — treat as a secret | `openssl rand -hex 32` |
| `GP_DATA_DIR` | Persistent volume path | `/var/lib/gatepulse` |

### Recommended

| Variable | Default | Recommended |
|----------|---------|-------------|
| `GP_TENANT_ID` | `default` | Your org slug |
| `GP_DELIVERY_WORKERS` | `16` | CPU count × 2 |
| `GP_STORE_POOL_SIZE` | `8` | 8–16 |
| `GP_MAX_PAYLOAD_BYTES` | `524288` | Keep at 512 KB |
| `GP_MAX_QUEUE_DEPTH` | `100000` | Tune to your throughput |
| `GP_RETENTION_SECS` | `604800` | 7–30 days |
| `GP_COMPACT_INTERVAL_MS` | `3600000` | 1h |
| `GP_MASTER_KEY` | — | Set for secret encryption (v2.0) |

### Full Reference

```bash
GP_PORT=8080
GP_LISTEN_ADDR=0.0.0.0
GP_API_KEY=<strong-random-key>
GP_TENANT_ID=your-org
GP_SINGLE_TENANT=true

# Store
GP_STORE_SOCKET=/run/gatepulse/gp_store.sock
GP_DATA_DIR=/var/lib/gatepulse
GP_STORE_POOL_SIZE=8

# Delivery
GP_DELIVERY_WORKERS=16
GP_MAX_PAYLOAD_BYTES=524288
GP_MAX_QUEUE_DEPTH=100000
GP_MAX_INFLIGHT_GLOBAL=500
GP_RETENTION_SECS=604800
GP_COMPACT_INTERVAL_MS=3600000

# Encryption (v2.0)
GP_MASTER_KEY=<32-byte-hex>
```

## Infrastructure Checklist

- [ ] `GP_DATA_DIR` is on a **persistent volume** (not ephemeral container storage)
- [ ] Volume has at least **50 GB** free (or autoscales)
- [ ] Container restart policy is `unless-stopped` or `always`
- [ ] `/healthz` and `/readyz` are wired to load balancer health checks
- [ ] Prometheus scrape configured for port `8080/metrics`
- [ ] Grafana dashboard imported (`grafana/dashboards/gatepulse.json`)
- [ ] Alert rules loaded (`prometheus/alerts.yml`)
- [ ] Firewall: port `8080` accessible only from trusted networks

## Backup

- [ ] Hourly backup cron configured (`gp store snapshot create`)
- [ ] Daily full backup to object storage (S3/GCS)
- [ ] Restore procedure tested at least once
- [ ] See [`backup.md`](backup.md) for full instructions

## Security

- [ ] `GP_API_KEY` is not the default (`change-me` / `dev-secret`)
- [ ] `GP_API_KEY` is injected via secrets manager (not hardcoded in yaml)
- [ ] Endpoint `secret` fields set for HMAC verification on consumer side
- [ ] `GP_MASTER_KEY` set and endpoint secrets encrypted (v2.0)
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
GP_URL=https://your-gatepulse.example.com \
GP_API_KEY=your-production-key \
bash test/e2e.sh
```

All 16 scenarios must pass (SSE scenario may be skipped in restricted networks).

## Capacity Planning

| Metric | Single node (16 workers) |
|--------|--------------------------|
| Ingest | ~5,000 events/s |
| Delivery | ~800 HTTP POST/s (20ms endpoint latency) |
| Storage | ~1 KB/event → 50 GB ≈ 50M events |
| Memory | ~256 MB Erlang + ~128 MB C store |

For higher throughput: increase `GP_DELIVERY_WORKERS`, run multiple API nodes (v1.3 HA), or use Postgres backend (v2.0).
