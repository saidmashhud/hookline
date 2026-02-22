# ТЗ: Эволюция GatePulse (post-v1 roadmap)

## 1) Цели эволюции

1. Сделать GatePulse **операционно “железобетонным”** (наблюдаемость, бэкапы, восстановление, защита от перегруза).
2. Улучшить **DX** (SDKs, CLI, локальная разработка, инспекция).
3. Довести streaming/реалтайм до полноценного режима (cursor/resume, backfill).
4. Добавить **enterprise-ready** элементы без потери простоты OSS (RBAC/scopes, secrets encryption, HA).
5. Обеспечить **стабильность API** и управляемые миграции.

---

## 2) Принципы версионирования и совместимости

### 2.1. SemVer

- `v1.x` — без breaking changes в `/v1` API.
- `v2.0` — допускаются breaking changes, но только с migration guide.

### 2.2. Политика деприкации

- Любое изменение поведения → фича-флаг/конфиг + предупреждение в логах + в docs.
- Deprecated endpoints остаются минимум на 2 minor релиза.

### 2.3. Версионирование storage

- Каждая запись в store имеет `record_version`.
- Store содержит `store_meta(version, created_at, last_compaction_at)`.
- При старте: если store version < binary supported → auto-migrate (где возможно) или “read-only mode + инструкция”.

---

# 3) Релизы и требования

## v1.1 — Hardening & DX (операционка + удобство)

**Цель:** чтобы GatePulse уверенно работал в проде на одной ноде, и им было удобно пользоваться как dev-tool.

### 3.1. Операционные фичи

1. **Backpressure & overload protection**

- Ввести глобальные лимиты:
  - `max_queue_depth_per_tenant`
  - `max_payload_size`
  - `max_inflight_global`

- Поведение при перегрузе:
  - `POST /events` возвращает `429` с `Retry-After` или `503` (configurable)
  - Метрика `gatepulse_overload_drops_total`

2. **Retention & compaction v1.1**

- Полноценные политики retention (events/attempts/dlq).
- Компакция по расписанию:
  - `compaction_cron`
  - метрики: `compaction_duration_seconds`, `segments_deleted_total`

- Команда CLI: `gp store compact`, `gp store stats`

3. **Backup/Restore**

- `gp store snapshot create` → создаёт consistent snapshot (с паузой claim или copy-on-write режимом).
- `gp store snapshot restore` → восстановление.
- Документация: “как бэкапить на проде” (cron + rotate).

### 3.2. DX

1. **SDKs (минимум)**

- `gatepulse-js` (Node/TS): publish event, manage endpoints/subs, verify signature helper.
- `gatepulse-python`: publish event, verify signature.
- Требования: 80% покрытие тестами, примеры в `examples/`.

2. **CLI v1.1**

- Улучшить UX:
  - `gp init` (создаёт config, проверяет соединение)
  - `gp doctor` (проверка store socket, permissions, health endpoints)
  - `gp tail --endpoint X` (стрим логов попыток через API)

3. **Docs v1.1**

- “Production checklist”:
  - лимиты, timeouts, retries, сигнатуры, алерты

- “Webhook consumer best practices”:
  - idempotency, обработка ретраев, хранение delivery-id/event-id

### 3.3. API изменения (без breaking)

- `POST /events` возвращает также `deduped: true/false` (если idempotency hit).
- `GET /stats` (admin/tenant): queue depth, inflight, retry backlog.

### 3.4. Acceptance Criteria v1.1

- Перегруз тест: при лимите очереди API корректно отвечает 429/503, очередь не “взрывается”.
- Snapshot: создать snapshot под нагрузкой, восстановить и продолжить delivery без потери данных.
- SDK: publish + verify signature примеры работают копипастой.
- Документирован production deploy + backup.

---

## v1.2 — Streaming 1.0 + Inbox UX

**Цель:** сделать realtime-stream реально полезным: cursor/resume + возможность backfill, а также улучшить инспекцию интеграций.

### 4.1. Streaming (SSE) как “первоклассная фича”

1. **Stable cursor**

- Ввести `stream_cursor` (монотонный offset) на уровне tenant.
- SSE:
  - `id: <cursor>`
  - поддержка `Last-Event-ID` для resume

- API: `GET /stream?topic=...&from_cursor=...` (from_cursor optional)

2. **Backfill mode**

- Ограниченный backfill:
  - `backfill_max_events`
  - `backfill_max_age_days`

- Поведение:
  - сначала отдать исторические события (backfill), затем live.

3. **Delivery → Stream bridge**

- Возможность стримить не только события, но и **статусы доставок**:
  - topic: `deliveries.status`
  - payload: attempt_id, endpoint_id, status, http_code, latency

### 4.2. Dev Inbox 2.0

- UI-lite (простая страница) в `/dev/inbox/ui`:
  - список inbox endpoints
  - просмотр request headers/body
  - кнопка “copy curl replay”

- Retention inbox сообщений отдельно (например 7 дней).

### 4.3. Observability дополнения

- Метрики streaming:
  - `gatepulse_stream_clients`
  - `gatepulse_stream_events_sent_total`

- Логи connect/disconnect с tenant_id.

### 4.4. Acceptance Criteria v1.2

- SSE клиент перезапускается и продолжает с `Last-Event-ID`.
- Backfill отдаёт события за период в пределах лимитов.
- Inbox UI показывает доставленные payload’ы и headers без ошибок.

---

## v1.3 — HA / Multi-node (кластер без боли)

**Цель:** GatePulse можно запускать в 2+ репликах с безопасной доставкой и без дублей “из-за гонок”.

### 5.1. Режимы HA

**Mode A (рекомендуемый OSS): Active-Active stateless API + single store leader**

- API реплики масштабируются горизонтально.
- Delivery workers — только на лидере (или через распределённый lock).
- Store daemon в 1 экземпляре (на volume), или лидер с failover вручную/автоматизировано.

**Mode B (advanced): Active-Active delivery**

- Требует распределённого claim/lease на store уровне или внешнего consensus.
- Для v1.3 допускается только при явном выборе в конфиге (experimental).

### 5.2. Leader election

- Варианты:
  - встроенный (Erlang cluster + net_kernel + leader election lib)
  - внешний lock (Redis/etcd) — опционально

- Acceptance: в момент падения лидера другой поднимает delivery в течение N секунд, без “вечных” claimed jobs.

### 5.3. Store improvements для HA

- Lease ownership: `claimed_by=node_id`, `lease_until`.
- Cleanup: periodic reaper возвращает job в очередь после lease expiry.

### 5.4. Deployment

- Helm chart:
  - API deployment replicas
  - store statefulset (1)
  - leader election config

- Docs: “HA deployment guide”.

### 5.5. Acceptance Criteria v1.3

- При kill -9 лидера delivery продолжает другой узел, jobs не теряются.
- Дубли возможны только в рамках at-least-once и документированы.

---

# 6) v2.0 — Enterprise-grade OSS (без потери простоты)

**Цель:** безопасность, управляемость, UI, расширяемость. Тут допускаем breaking changes (при необходимости).

## 6.1. Security & Access Control

1. **Scopes / RBAC**

- API keys со scope:
  - `events:write`, `events:read`, `endpoints:manage`, `replay:write`, `dlq:manage`

- (Опционально) роли пользователей, если появится UI-логин.

2. **Secrets encryption at rest**

- KMS-like:
  - master key через env
  - rotation (key versions)

- Храним endpoint secret зашифрованным.
- Миграция store: encrypt existing secrets.

3. **Audit log**

- Любая admin-операция пишется в audit stream:
  - create/delete endpoints, replay, dlq requeue, key revoke.

## 6.2. Transformations & Filters (plugin layer)

1. **Event filters per subscription**

- Простой DSL:
  - по полям payload: `payload.amount > 100`
  - allowlist/denylist

- Ограничения: безопасность и производительность, лимит сложности.

2. **Transformations**

- Webhook payload mapping (переименование полей, добавление computed fields).
- Реализация безопасно:
  - встроенный минимальный mapping engine (v2.0)
  - расширение через WASM plugins (v2.1 optional)

## 6.3. UI Console (минимальный, но полезный)

- Просмотр:
  - endpoints/subscriptions
  - deliveries timeline
  - DLQ
  - replay

- Live view: SSE deliveries.status
- Авторизация: API key или отдельный admin token (v2.0 lite).

## 6.4. Storage strategy v2

Варианты:

- оставить C store как default (fast, self-contained)
- добавить backend `Postgres` как optional (enterprise-friendly)
  Требование: единый интерфейс `StoreBehaviour`, переключение backend через конфиг.

## 6.5. Acceptance Criteria v2.0

- Scopes работают: ключ без `replay:write` не может делать replay.
- Secrets зашифрованы, есть rotation.
- UI позволяет сделать 80% стандартных задач без CLI.
- Store backend можно выбрать (C или Postgres) без изменения API.

---

# 7) Изменения репозитория (пост-v1)

Добавить:

- `docs/production/` (checklist, HA, backup, tuning)
- `sdk/js`, `sdk/python`
- `ui/console` (v2)
- `store/backends/postgres` (v2 optional)
- `benchmarks/` + reproducible нагрузочный прогон
- `grafana/` dashboards + `prometheus/` alert rules

---

# 8) Тестирование и качество (эволюция)

## 8.1. Обязательные новые тесты по релизам

- v1.1: overload tests + snapshot restore integration
- v1.2: SSE resume/backfill tests
- v1.3: chaos tests (leader failover, lease reaping)
- v2: security tests (scopes, encryption), UI e2e smoke

## 8.2. SLO (операционные цели)

- Delivery success p95 latency (на healthy endpoint) — фиксируется в docs как baseline.
- Retry scheduling correctness (no starvation).
- Store recovery time limit for N GB сегментов.

---

# 9) Документация по эволюции (обязательное)

Для каждого релиза:

- `docs/releases/vX.Y.md`:
  - изменения
  - миграции
  - breaking/deprecated

- “Upgrade guide”:
  - как обновить docker image
  - как обновить store (если нужно)
  - rollback стратегия

---

# 10) Итоговая линейка (сжатая)

- **v1.1:** hardening (overload, compaction, backup) + SDK/CLI/docs
- **v1.2:** streaming 1.0 (cursor/resume/backfill) + inbox UI
- **v1.3:** HA/multi-node (leader election + safe leases)
- **v2.0:** scopes/RBAC, encryption, audit, transformations, UI console, optional Postgres backend
