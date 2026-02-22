# Backup and Restore

GatePulse stores all data in append-only segment files under `GP_DATA_DIR`.
Backing up is a simple file copy, but requires pausing job claims to avoid
split-brain writes during the copy.

## Creating a snapshot

```bash
# 1. Pause job claiming (stops the delivery poller from dequeuing new jobs)
curl -X POST http://localhost:8080/v1/admin/store/pause-claims \
  -H "Authorization: Bearer $GP_API_KEY"

# 2. Copy the data directory
rsync -av --progress $GP_DATA_DIR/ /path/to/backup/$(date +%Y%m%d-%H%M%S)/

# 3. Resume job claiming
curl -X POST http://localhost:8080/v1/admin/store/resume-claims \
  -H "Authorization: Bearer $GP_API_KEY"
```

Or use the `gp` CLI:

```bash
gp store snapshot create --dest /path/to/backups
```

## Restoring from a snapshot

> **Warning:** Stop GatePulse before restoring. Starting over existing data without stopping first will corrupt the store.

```bash
# 1. Stop GatePulse
docker-compose down   # or: kill $(cat /var/run/gatepulse.pid)

# 2. Validate backup (check segment magic bytes)
gp store snapshot restore --from /path/to/backup/20260101-120000 --validate-only

# 3. Restore
gp store snapshot restore --from /path/to/backup/20260101-120000

# 4. Start GatePulse
docker-compose up -d
```

## Backup strategy

| Frequency | Method | Retention |
|-----------|--------|-----------|
| Hourly    | Incremental rsync (pause/copy/resume) | 24 snapshots |
| Daily     | Full copy to object storage (S3/GCS) | 30 days |
| Weekly    | Archive to cold storage | 1 year |

## Notes

- Segments are immutable once sealed (when they reach 256MB). Only the active segment
  is being written to; all others can be copied without pausing.
- If `GP_DATA_DIR` is on a filesystem with snapshot support (ZFS, LVM), use
  filesystem snapshots instead of the pause/copy/resume approach.
- The backup includes all events, endpoints, subscriptions, jobs, attempts, DLQ,
  and API keys. It does **not** include in-memory queue state — any in-flight jobs
  at backup time will be re-tried after restore (safe due to at-least-once delivery).
