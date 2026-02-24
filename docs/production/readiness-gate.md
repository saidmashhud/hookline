# HookLine Production Readiness Gate

Этот runbook закрывает production-gate перед релизом HookLine без CI-автоматизации.

## Scope

Gate состоит из двух частей:

1. Автоматическая локальная валидация:
`load + soak + chaos-restart` через `/Users/saidmashhud/Projects/personal/hookline/test/production-readiness.sh`.
2. Обязательные ручные drills:
`rollback/restore + DR`.

## 1) Automated Gate

### api_key mode

```bash
cd /Users/saidmashhud/Projects/personal/hookline
HL_URL=http://localhost:8080 \
HL_AUTH_MODE=api_key \
HL_API_KEY=<api-key> \
LOAD_EVENTS=500 \
LOAD_CONCURRENCY=50 \
SOAK_SECONDS=300 \
CHAOS_RESTART=true \
HL_CONTAINER=hookline \
./test/production-readiness.sh
```

### service_token embedded mode

```bash
cd /Users/saidmashhud/Projects/personal/hookline
HL_URL=http://localhost:8080 \
HL_AUTH_MODE=service_token \
HL_SERVICE_TOKEN=<service-token> \
HL_TENANT_ID=<tenant-id> \
LOAD_EVENTS=500 \
LOAD_CONCURRENCY=50 \
SOAK_SECONDS=300 \
CHAOS_RESTART=true \
HL_CONTAINER=hookline \
./test/production-readiness.sh
```

### Pass criteria

- Все publish в load/soak получают 2xx.
- Количество доставок в Dev Inbox не меньше ожидаемого после каждой фазы.
- После restart-chaos сервис поднимается, события продолжают доставляться.

## 2) Rollback / Restore Drill (Mandatory)

Сценарий: проверить, что snapshot можно валидно восстановить.

1. Создать snapshot:

```bash
gp store snapshot create --dest /path/to/backups
```

2. Проверить snapshot без восстановления:

```bash
gp store snapshot restore --from /path/to/backups/<snapshot-id> --validate-only
```

3. Восстановить в отдельный canary-инстанс и выполнить smoke:

- `POST /v1/events`
- `GET /v1/events`
- `GET /v1/deliveries`
- `GET /v1/dlq`

4. Зафиксировать фактические метрики:

- время восстановления `RTO`
- потеря данных `RPO`

Pass criteria:

- `RTO` в пределах SLO.
- `RPO` не превышает согласованное окно.
- Canary smoke проходит без ошибок.

## 3) DR Drill (Mandatory)

Сценарий: потеря инстанса/процесса и восстановление работы.

1. Зафиксировать базовые показатели:

- ingest rate, delivery success rate, queue depth, DLQ size.

2. Смоделировать отказ:

- остановить активный инстанс / writer-node.

3. Выполнить восстановление:

- поднять инстанс,
- дождаться `healthz` + `readyz`,
- убедиться в возобновлении доставок.

4. Повторить smoke:

- publish -> delivery -> retry/replay.

Pass criteria:

- восстановление в SLO по времени,
- нет tenant-leak между tenant-ами,
- backlog не растет бесконтрольно после восстановления.

## 4) Release Decision

`GO` только если:

- automated gate прошел полностью,
- rollback/restore drill завершен успешно,
- DR drill завершен успешно,
- риски и лимиты (RTO/RPO/SLO) зафиксированы в release note.

