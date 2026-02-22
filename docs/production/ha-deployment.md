# HA
GatePulse v1.3+ supports multi-node HA via Erlang clustering.

## Leader Election

Uses Erlang global module:
- First node to global:register_name/2 becomes leader
- Leader runs the delivery poller and worker pool
- When leader dies, a follower detects DOWN and re-campaigns
- No job loss: expired lease jobs are re-claimed automatically

## Kubernetes Setup

Run helm install with replicaCount=3 and cluster.enabled=true.

## Failure Scenarios

| Failure | Impact | Recovery |
|---------|--------|----------|
| API node dies | Deliveries on surviving nodes continue | Automatic |
| Leader dies | ~10s delivery pause | Follower becomes leader |
| Store daemon dies | Deliveries stop | Manual restart; data preserved |
