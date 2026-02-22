# PostgreSQL Backend

GatePulse v2.0 supports PostgreSQL as an alternative to the embedded C store daemon.
Use this when you need stronger consistency guarantees, existing PostgreSQL infrastructure,
or want to use standard database tooling for backups and observability.

## Prerequisites

- PostgreSQL 14+
- A dedicated database (e.g. `gatepulse`)
- A user with `CREATE TABLE`, `INSERT`, `UPDATE`, `DELETE`, `SELECT` on that database

## Setup

### 1. Create the database

```sql
CREATE DATABASE gatepulse;
CREATE USER gatepulse_user WITH PASSWORD 'your-password';
GRANT ALL PRIVILEGES ON DATABASE gatepulse TO gatepulse_user;
```

### 2. Run migrations

```bash
psql postgres://gatepulse_user:your-password@localhost:5432/gatepulse \
  -f priv/sql/001_initial.sql
```

The migration creates the following tables:
`events`, `endpoints`, `subscriptions`, `jobs`, `attempts`, `dlq`, `api_keys`

### 3. Configure GatePulse

```bash
export GP_STORE_BACKEND=postgres
export GP_POSTGRES_URL=postgres://gatepulse_user:your-password@localhost:5432/gatepulse
```

Or in `docker-compose.yml`:

```yaml
environment:
  GP_STORE_BACKEND: postgres
  GP_POSTGRES_URL: postgres://gatepulse_user:your-password@db:5432/gatepulse
```

## Switching from C Store to PostgreSQL

1. Stop GatePulse
2. Take a final snapshot: `gp store snapshot create --dest /tmp/final-snapshot`
3. Run the migration script to import snapshot data into Postgres:
   ```bash
   # (manual migration — export events/endpoints/subscriptions via API and re-import)
   ```
4. Set `GP_STORE_BACKEND=postgres` and start GatePulse
5. Verify with `gp doctor`

> **Note:** There is no automated migration tool from the C store format to PostgreSQL.
> For production migrations, stop ingestion, export data via the REST API, and replay.

## Differences from C Store

| Feature | C Store | PostgreSQL |
|---------|---------|-----------|
| Backup | Pause/rsync/resume | `pg_dump` / streaming replication |
| Scalability | Single node | Connection pooling (PgBouncer) |
| Query flexibility | Cursor-based scan | Full SQL |
| Operational complexity | Zero deps | Requires Postgres |
| Max throughput | ~50k events/s | ~10k events/s (single PG) |

## Connection Pooling

For production, use PgBouncer in transaction mode:

```ini
[databases]
gatepulse = host=localhost port=5432 dbname=gatepulse

[pgbouncer]
pool_mode = transaction
max_client_conn = 1000
default_pool_size = 50
```

Set `GP_POSTGRES_URL` to point at PgBouncer instead of PostgreSQL directly.

## Monitoring

Key queries for observability:

```sql
-- Queue depth by tenant
SELECT tenant_id, COUNT(*) FROM jobs WHERE status = 'pending' GROUP BY tenant_id;

-- DLQ size
SELECT COUNT(*) FROM dlq;

-- Failed deliveries in last hour
SELECT COUNT(*) FROM attempts
WHERE status = 'failure' AND started_at > NOW() - INTERVAL '1 hour';

-- P99 latency (milliseconds)
SELECT PERCENTILE_CONT(0.99) WITHIN GROUP (ORDER BY duration_ms) AS p99
FROM attempts
WHERE started_at > NOW() - INTERVAL '5 minutes';
```
