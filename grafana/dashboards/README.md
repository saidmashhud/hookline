# GatePulse Grafana Dashboard

## Import

1. Open Grafana → Dashboards → Import
2. Upload `gatepulse.json` or paste its contents
3. Select your Prometheus datasource when prompted
4. Click **Import**

## Panels

| Panel | Metric | Description |
|-------|--------|-------------|
| Events Published | `gatepulse_events_published_total` | Rate of events published per second |
| Queue Depth | `gatepulse_queue_depth` | Pending jobs (yellow >50k, red >90k) |
| DLQ Total | `gatepulse_dlq_total` | Dead letter queue size (red on any value) |
| SSE Clients | `gatepulse_stream_clients` | Active SSE stream connections |
| Delivery Throughput | `gatepulse_deliveries_total` | Success / retried / failed breakdown |
| Delivery Latency | `gatepulse_delivery_latency_ms` | P50 / P95 / P99 histogram quantiles |
| In-Flight | `gatepulse_inflight` | Concurrent in-flight HTTP deliveries |
| Overload Drops | `gatepulse_overload_drops_total` | Events dropped due to queue overflow |
| Store IPC Ops | `gatepulse_store_ops_total` | C daemon IPC call rate by command |
| Rate Limited | `gatepulse_rate_limited_total` | Deliveries skipped by rate limiter |
| SSE Events Sent | `gatepulse_stream_events_total` | Events pushed to SSE subscribers |

## Alerting (example PromQL)

```promql
# DLQ growing
increase(gatepulse_dlq_total[10m]) > 0

# Queue depth near limit
sum(gatepulse_queue_depth) > 80000

# Delivery failure rate > 5%
sum(rate(gatepulse_deliveries_total{status="failed"}[5m]))
  / sum(rate(gatepulse_deliveries_total[5m])) > 0.05

# P99 latency > 10s
histogram_quantile(0.99,
  sum(rate(gatepulse_delivery_latency_ms_bucket[5m])) by (le)) > 10000
```
