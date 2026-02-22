#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>
#include <errno.h>
#include <time.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/select.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <arpa/inet.h>
#include <stdint.h>

#include "store.h"
#include "index.h"
#include "queue.h"
#include "ipc.h"
#include "dlq.h"

/* ---------- naive JSON helpers ---------- */
static const char *json_find(const char *json, const char *key) {
    char needle[128];
    snprintf(needle, sizeof(needle), "\"%s\"", key);
    const char *p = strstr(json, needle);
    if (!p) return NULL;
    p += strlen(needle);
    while (*p == ' ' || *p == ':') p++;
    return p;
}

static int json_str(const char *json, const char *key, char *out, size_t outlen) {
    const char *p = json_find(json, key);
    if (!p || *p != '"') return -1;
    p++;
    size_t i = 0;
    while (*p && *p != '"' && i < outlen - 1) {
        if (*p == '\\' && *(p+1)) {
            p++;
            switch (*p) {
                case '"':  out[i++] = '"';  break;
                case '\\': out[i++] = '\\'; break;
                case '/':  out[i++] = '/';  break;
                case 'n':  out[i++] = '\n'; break;
                case 'r':  out[i++] = '\r'; break;
                case 't':  out[i++] = '\t'; break;
                default:   out[i++] = *p;   break;
            }
        } else {
            out[i++] = *p;
        }
        p++;
    }
    out[i] = '\0';
    return 0;
}

static int json_int(const char *json, const char *key, int *out) {
    const char *p = json_find(json, key);
    if (!p) return -1;
    *out = atoi(p);
    return 0;
}

static int64_t json_int64(const char *json, const char *key, int64_t def) {
    const char *p = json_find(json, key);
    if (!p) return def;
    return (int64_t)strtoll(p, NULL, 10);
}

static int parse_cursor(const char *cursor, uint32_t *seg, uint64_t *off) {
    if (!cursor || !cursor[0]) return -1;
    char *end;
    *seg = (uint32_t)strtoul(cursor, &end, 10);
    if (!end || *end != ':') return -1;
    *off = (uint64_t)strtoull(end + 1, NULL, 10);
    return 0;
}

/* ---------- global state ---------- */
static hl_store_t g_store;
static hl_index_t g_index;
static hl_queue_t g_queue;
static hl_dlq_t   g_dlq;
static volatile int g_running = 1;

/* ---------- tenant event secondary index ----------
 * Maps tenant_id → ordered list of hl_offset_t (chronological).
 * Built during recovery; updated on every append_event.
 * Enables O(tenant_events) list_events instead of O(all_events).
 */
#define HL_TEIDX_BUCKETS 256

typedef struct {
    char         tenant_id[64];
    hl_offset_t *offsets;
    int          count;
    int          cap;
} hl_tenant_events_t;

typedef struct hl_teidx_node {
    hl_tenant_events_t        te;
    struct hl_teidx_node     *next;
} hl_teidx_node_t;

static hl_teidx_node_t *g_teidx[HL_TEIDX_BUCKETS];

static unsigned teidx_hash(const char *s) {
    unsigned h = 5381;
    while (*s) h = ((h << 5) + h) ^ (unsigned char)*s++;
    return h % HL_TEIDX_BUCKETS;
}

static hl_tenant_events_t *teidx_get(const char *tenant_id) {
    unsigned h = teidx_hash(tenant_id);
    hl_teidx_node_t *n = g_teidx[h];
    while (n) {
        if (strcmp(n->te.tenant_id, tenant_id) == 0) return &n->te;
        n = n->next;
    }
    return NULL;
}

static hl_tenant_events_t *teidx_get_or_create(const char *tenant_id) {
    hl_tenant_events_t *te = teidx_get(tenant_id);
    if (te) return te;
    hl_teidx_node_t *n = calloc(1, sizeof(*n));
    if (!n) return NULL;
    strncpy(n->te.tenant_id, tenant_id, sizeof(n->te.tenant_id) - 1);
    n->te.cap     = 64;
    n->te.offsets = malloc(sizeof(hl_offset_t) * (size_t)n->te.cap);
    if (!n->te.offsets) { free(n); return NULL; }
    unsigned h    = teidx_hash(tenant_id);
    n->next       = g_teidx[h];
    g_teidx[h]   = n;
    return &n->te;
}

static void teidx_add(const char *tenant_id, hl_offset_t off) {
    if (!tenant_id || !tenant_id[0]) return;
    hl_tenant_events_t *te = teidx_get_or_create(tenant_id);
    if (!te) return;
    if (te->count == te->cap) {
        int newcap = te->cap * 2;
        hl_offset_t *tmp = realloc(te->offsets,
                                   sizeof(hl_offset_t) * (size_t)newcap);
        if (!tmp) return;
        te->offsets = tmp;
        te->cap     = newcap;
    }
    te->offsets[te->count++] = off;
}

static void teidx_free(void) {
    for (int b = 0; b < HL_TEIDX_BUCKETS; b++) {
        hl_teidx_node_t *n = g_teidx[b];
        while (n) {
            hl_teidx_node_t *nx = n->next;
            free(n->te.offsets);
            free(n);
            n = nx;
        }
        g_teidx[b] = NULL;
    }
}

static void handle_signal(int sig) { (void)sig; g_running = 0; }

/* ---------- crash recovery ----------
 * Scans all segments sequentially. Rebuilds:
 *   - index   (event_id, endpoint_id, subscription_id, attempt_id → offset)
 *   - queue   (pending/in-flight jobs not ACKed and not in DLQ)
 *   - dlq     (DLQ entries)
 *
 * ACK tombstone records (HL_TYPE_ACK) mark jobs as completed so they are
 * not re-enqueued after restart. This gives at-least-once + crash safety.
 */
typedef struct job_node { hl_job_t job; struct job_node *next; } job_node_t;

static void recover_from_store(void) {
    hl_index_t acked;   hl_index_init(&acked);
    hl_index_t in_dlq;  hl_index_init(&in_dlq);
    job_node_t *jobs_head = NULL;
    int events_recovered = 0, jobs_recovered = 0, dlq_recovered = 0;

    for (int seg = 0; seg <= g_store.current_seg; seg++) {
        /* build path manually (seg_path is in store.c, not exported) */
        char path[512];
        snprintf(path, sizeof(path), "%s/seg-%06d.gps", g_store.data_dir, seg);
        int fd = open(path, O_RDONLY);
        if (fd < 0) break;

        uint64_t offset = 0;
        for (;;) {
            uint8_t hdr[HL_RECORD_HEADER_SIZE];
            if (read(fd, hdr, HL_RECORD_HEADER_SIZE) != HL_RECORD_HEADER_SIZE) break;

            uint32_t magic_be;
            memcpy(&magic_be, hdr, 4);
            if (ntohl(magic_be) != HL_MAGIC) break;  /* corrupt / truncated */

            uint8_t type = hdr[5];
            uint32_t plen_be;
            memcpy(&plen_be, hdr + 6, 4);
            uint32_t plen = ntohl(plen_be);

            uint8_t *payload = malloc(plen + 1);
            if (!payload) break;
            if ((uint32_t)read(fd, payload, plen) != plen) { free(payload); break; }
            payload[plen] = '\0';

            uint32_t crc_be;
            if (read(fd, &crc_be, 4) != 4) { free(payload); break; }

            /* verify CRC */
            uint8_t *full = malloc(HL_RECORD_HEADER_SIZE + plen);
            if (!full) { free(payload); break; }
            memcpy(full, hdr, HL_RECORD_HEADER_SIZE);
            memcpy(full + HL_RECORD_HEADER_SIZE, payload, plen);
            uint32_t computed = hl_crc32(full, HL_RECORD_HEADER_SIZE + plen);
            free(full);
            if (computed != ntohl(crc_be)) { free(payload); break; } /* truncated write */

            hl_offset_t off = {(uint32_t)seg, offset};

            switch (type) {
                case HL_TYPE_EVENT: {
                    char event_id[64]    = {0};
                    char ev_tenant[64]   = {0};
                    json_str((char *)payload, "event_id",  event_id,  sizeof(event_id));
                    json_str((char *)payload, "tenant_id", ev_tenant, sizeof(ev_tenant));
                    if (event_id[0]) {
                        hl_index_put(&g_index, event_id, off);
                        events_recovered++;
                    }
                    if (ev_tenant[0]) teidx_add(ev_tenant, off);
                    break;
                }
                case HL_TYPE_ENDPOINT: {
                    char endpoint_id[64] = {0};
                    json_str((char *)payload, "endpoint_id", endpoint_id, sizeof(endpoint_id));
                    if (endpoint_id[0]) hl_index_put(&g_index, endpoint_id, off);
                    break;
                }
                case HL_TYPE_SUBSCRIPTION: {
                    char sub_id[64] = {0};
                    json_str((char *)payload, "subscription_id", sub_id, sizeof(sub_id));
                    if (sub_id[0]) hl_index_put(&g_index, sub_id, off);
                    break;
                }
                case HL_TYPE_ATTEMPT: {
                    char attempt_id[64] = {0};
                    json_str((char *)payload, "attempt_id", attempt_id, sizeof(attempt_id));
                    if (attempt_id[0]) hl_index_put(&g_index, attempt_id, off);
                    break;
                }
                case HL_TYPE_JOB: {
                    job_node_t *n = calloc(1, sizeof(*n));
                    if (!n) { free(payload); goto done; }
                    json_str((char *)payload, "job_id",      n->job.job_id,      sizeof(n->job.job_id));
                    json_str((char *)payload, "event_id",    n->job.event_id,    sizeof(n->job.event_id));
                    json_str((char *)payload, "endpoint_id", n->job.endpoint_id, sizeof(n->job.endpoint_id));
                    json_str((char *)payload, "tenant_id",   n->job.tenant_id,   sizeof(n->job.tenant_id));
                    int max_att = 5;
                    json_int((char *)payload, "max_attempts", &max_att);
                    n->job.max_attempts    = max_att;
                    n->job.store_offset    = off;
                    n->job.next_attempt_at = 0;
                    n->next = jobs_head;
                    jobs_head = n;
                    break;
                }
                case HL_TYPE_ACK: {
                    char job_id[HL_JOB_ID_LEN] = {0};
                    json_str((char *)payload, "job_id", job_id, sizeof(job_id));
                    if (job_id[0]) {
                        hl_offset_t sentinel = {0, 0};
                        hl_index_put(&acked, job_id, sentinel);
                    }
                    break;
                }
                case HL_TYPE_TOMBSTONE: {
                    char resource_id[128] = {0};
                    json_str((char *)payload, "id", resource_id, sizeof(resource_id));
                    if (resource_id[0]) hl_index_delete(&g_index, resource_id);
                    break;
                }
                case HL_TYPE_DLQ: {
                    hl_dlq_entry_t e = {0};
                    json_str((char *)payload, "job_id",      e.job_id,      sizeof(e.job_id));
                    json_str((char *)payload, "event_id",    e.event_id,    sizeof(e.event_id));
                    json_str((char *)payload, "endpoint_id", e.endpoint_id, sizeof(e.endpoint_id));
                    json_str((char *)payload, "tenant_id",   e.tenant_id,   sizeof(e.tenant_id));
                    json_str((char *)payload, "reason",      e.reason,      sizeof(e.reason));
                    int att = 0; json_int((char *)payload, "attempt_count", &att);
                    e.attempt_count = att;
                    if (e.job_id[0]) {
                        hl_dlq_put(&g_dlq, &e);
                        hl_offset_t sentinel = {0, 0};
                        hl_index_put(&in_dlq, e.job_id, sentinel);
                        dlq_recovered++;
                    }
                    break;
                }
            }

            offset += HL_RECORD_HEADER_SIZE + plen + HL_RECORD_FOOTER_SIZE;
            free(payload);
        }
        close(fd);
    }

done:
    /* Enqueue jobs not yet ACKed and not in DLQ */
    {
        job_node_t *n = jobs_head;
        while (n) {
            hl_offset_t dummy;
            if (n->job.job_id[0] &&
                hl_index_get(&acked,  n->job.job_id, &dummy) != 0 &&
                hl_index_get(&in_dlq, n->job.job_id, &dummy) != 0) {
                hl_queue_enqueue(&g_queue, &n->job);
                jobs_recovered++;
            }
            job_node_t *next = n->next;
            free(n);
            n = next;
        }
    }

    hl_index_free(&acked);
    hl_index_free(&in_dlq);

    fprintf(stdout, "Recovery: %d events, %d jobs re-queued, %d DLQ entries\n",
            events_recovered, jobs_recovered, dlq_recovered);
    fflush(stdout);
}

/* ---------- IPC command dispatch ---------- */
static void dispatch(int client_fd, const char *json) {
    char cmd[64] = {0};
    json_str(json, "cmd", cmd, sizeof(cmd));

    if (strcmp(cmd, "store.append_event") == 0) {
        char event_id[64]  = {0};
        char tenant_id[64] = {0};
        char payload_val[65536] = {0};
        json_str(json, "event_id",  event_id,  sizeof(event_id));
        json_str(json, "tenant_id", tenant_id, sizeof(tenant_id));
        json_str(json, "payload",   payload_val, sizeof(payload_val));

        hl_offset_t off;
        int rc = hl_store_append(&g_store, HL_TYPE_EVENT,
                                 (uint8_t *)payload_val,
                                 (uint32_t)strlen(payload_val), &off);
        if (rc == 0) {
            hl_index_put(&g_index, event_id, off);
            if (tenant_id[0]) teidx_add(tenant_id, off);
            char resp[128];
            snprintf(resp, sizeof(resp),
                     "{\"ok\":true,\"segment\":%u,\"offset\":%llu}",
                     off.segment, (unsigned long long)off.offset);
            hl_ipc_send_str(client_fd, resp);
        } else {
            hl_ipc_send_str(client_fd, "{\"ok\":false,\"error\":\"append failed\"}");
        }

    } else if (strcmp(cmd, "store.get_event") == 0) {
        char event_id[64]  = {0};
        char tenant_id[64] = {0};
        json_str(json, "event_id",  event_id,  sizeof(event_id));
        json_str(json, "tenant_id", tenant_id, sizeof(tenant_id));
        hl_offset_t off;
        if (hl_index_get(&g_index, event_id, &off) != 0) {
            hl_ipc_send_str(client_fd, "{\"ok\":false,\"error\":\"not found\"}");
            return;
        }
        uint8_t *payload = NULL; uint32_t plen; uint8_t type;
        if (hl_store_read(&g_store, off, &payload, &plen, &type) != 0) {
            hl_ipc_send_str(client_fd, "{\"ok\":false,\"error\":\"read failed\"}");
            return;
        }
        /* Tenant isolation: verify the stored event belongs to the caller */
        if (tenant_id[0]) {
            char ev_tenant[64] = {0};
            json_str((char *)payload, "tenant_id", ev_tenant, sizeof(ev_tenant));
            if (strcmp(ev_tenant, tenant_id) != 0) {
                free(payload);
                hl_ipc_send_str(client_fd, "{\"ok\":false,\"error\":\"not found\"}");
                return;
            }
        }
        char *resp = malloc(plen + 32);
        if (resp) {
            snprintf(resp, plen + 32, "{\"ok\":true,\"event\":%s}", (char *)payload);
            hl_ipc_send_str(client_fd, resp);
            free(resp);
        }
        free(payload);

    } else if (strcmp(cmd, "store.enqueue_job") == 0) {
        hl_job_t job = {0};
        json_str(json, "job_id",      job.job_id,      sizeof(job.job_id));
        json_str(json, "event_id",    job.event_id,    sizeof(job.event_id));
        json_str(json, "endpoint_id", job.endpoint_id, sizeof(job.endpoint_id));
        json_str(json, "tenant_id",   job.tenant_id,   sizeof(job.tenant_id));
        int max_att = 5;
        json_int(json, "max_attempts", &max_att);
        job.max_attempts    = max_att;
        job.next_attempt_at = 0;

        hl_offset_t off;
        hl_store_append(&g_store, HL_TYPE_JOB,
                        (uint8_t *)json, (uint32_t)strlen(json), &off);
        job.store_offset = off;

        if (hl_queue_enqueue(&g_queue, &job) == 0)
            hl_ipc_send_str(client_fd, "{\"ok\":true}");
        else
            hl_ipc_send_str(client_fd, "{\"ok\":false,\"error\":\"enqueue failed\"}");

    } else if (strcmp(cmd, "store.claim_jobs") == 0) {
        int max_count = 10, lease_secs = 30;
        char claim_tenant[64] = {0};
        json_int(json, "max_count",  &max_count);
        json_int(json, "lease_secs", &lease_secs);
        json_str(json, "tenant_id",  claim_tenant, sizeof(claim_tenant));

        hl_queue_reap_leases(&g_queue);

        hl_job_t **claimed = calloc((size_t)max_count, sizeof(*claimed));
        if (!claimed) {
            hl_ipc_send_str(client_fd, "{\"ok\":false,\"error\":\"oom\"}");
            return;
        }
        int n = hl_queue_claim_tenant(&g_queue, claimed, max_count, lease_secs,
                                      claim_tenant[0] ? claim_tenant : NULL);

        char *resp = malloc((size_t)n * 256 + 64);
        if (!resp) { free(claimed); hl_ipc_send_str(client_fd, "{\"ok\":false,\"error\":\"oom\"}"); return; }
        int pos = 0;
        pos += sprintf(resp + pos, "{\"ok\":true,\"jobs\":[");
        for (int i = 0; i < n; i++) {
            pos += sprintf(resp + pos,
                "{\"job_id\":\"%s\",\"event_id\":\"%s\","
                "\"endpoint_id\":\"%s\",\"tenant_id\":\"%s\","
                "\"attempt_count\":%d,\"max_attempts\":%d}%s",
                claimed[i]->job_id, claimed[i]->event_id,
                claimed[i]->endpoint_id, claimed[i]->tenant_id,
                claimed[i]->attempt_count, claimed[i]->max_attempts,
                i < n-1 ? "," : "");
        }
        sprintf(resp + pos, "]}");
        hl_ipc_send_str(client_fd, resp);
        free(resp); free(claimed);

    } else if (strcmp(cmd, "store.ack_job") == 0) {
        char job_id[HL_JOB_ID_LEN] = {0};
        json_str(json, "job_id", job_id, sizeof(job_id));

        /* Write ACK tombstone for crash recovery */
        char ack_json[128];
        snprintf(ack_json, sizeof(ack_json), "{\"job_id\":\"%s\"}", job_id);
        hl_store_append(&g_store, HL_TYPE_ACK,
                        (uint8_t *)ack_json, (uint32_t)strlen(ack_json), NULL);

        if (hl_queue_ack(&g_queue, job_id) == 0)
            hl_ipc_send_str(client_fd, "{\"ok\":true}");
        else
            hl_ipc_send_str(client_fd, "{\"ok\":false,\"error\":\"not found\"}");

    } else if (strcmp(cmd, "store.nack_job") == 0) {
        char job_id[HL_JOB_ID_LEN] = {0};
        int delay = 30;
        json_str(json, "job_id", job_id, sizeof(job_id));
        json_int(json, "delay_secs", &delay);
        if (hl_queue_nack(&g_queue, job_id, delay) == 0)
            hl_ipc_send_str(client_fd, "{\"ok\":true}");
        else
            hl_ipc_send_str(client_fd, "{\"ok\":false,\"error\":\"not found\"}");

    } else if (strcmp(cmd, "store.put_dlq") == 0) {
        hl_dlq_entry_t e = {0};
        json_str(json, "job_id",      e.job_id,      sizeof(e.job_id));
        json_str(json, "event_id",    e.event_id,    sizeof(e.event_id));
        json_str(json, "endpoint_id", e.endpoint_id, sizeof(e.endpoint_id));
        json_str(json, "tenant_id",   e.tenant_id,   sizeof(e.tenant_id));
        json_str(json, "reason",      e.reason,      sizeof(e.reason));
        int att = 0; json_int(json, "attempt_count", &att);
        e.attempt_count = att;
        e.failed_at = time(NULL);

        hl_store_append(&g_store, HL_TYPE_DLQ,
                        (uint8_t *)json, (uint32_t)strlen(json), &e.store_offset);

        if (hl_dlq_put(&g_dlq, &e) == 0)
            hl_ipc_send_str(client_fd, "{\"ok\":true}");
        else
            hl_ipc_send_str(client_fd, "{\"ok\":false,\"error\":\"dlq put failed\"}");

    } else if (strcmp(cmd, "store.list_dlq") == 0) {
        char tenant_id[64] = {0};
        json_str(json, "tenant_id", tenant_id, sizeof(tenant_id));
        hl_dlq_entry_t *arr = NULL; int count = 0;
        hl_dlq_list(&g_dlq, tenant_id[0] ? tenant_id : NULL, &arr, &count);
        char *resp = malloc((size_t)count * 512 + 64);
        if (!resp) { hl_ipc_send_str(client_fd, "{\"ok\":false,\"error\":\"oom\"}"); free(arr); return; }
        int pos = sprintf(resp, "{\"ok\":true,\"items\":[");
        for (int i = 0; i < count; i++) {
            pos += sprintf(resp + pos,
                "{\"job_id\":\"%s\",\"event_id\":\"%s\","
                "\"endpoint_id\":\"%s\",\"reason\":\"%s\","
                "\"attempt_count\":%d}%s",
                arr[i].job_id, arr[i].event_id, arr[i].endpoint_id,
                arr[i].reason, arr[i].attempt_count, i < count-1 ? "," : "");
        }
        sprintf(resp + pos, "]}");
        hl_ipc_send_str(client_fd, resp);
        free(resp); free(arr);

    } else if (strcmp(cmd, "store.requeue_dlq") == 0) {
        char job_id[HL_DLQ_JOB_ID_LEN] = {0};
        json_str(json, "job_id", job_id, sizeof(job_id));
        if (hl_dlq_delete(&g_dlq, job_id) == 0)
            hl_ipc_send_str(client_fd, "{\"ok\":true}");
        else
            hl_ipc_send_str(client_fd, "{\"ok\":false,\"error\":\"not found\"}");

    } else if (strcmp(cmd, "store.append_attempt") == 0) {
        char attempt_id[64] = {0};
        json_str(json, "attempt_id", attempt_id, sizeof(attempt_id));
        hl_offset_t off;
        hl_store_append(&g_store, HL_TYPE_ATTEMPT,
                        (uint8_t *)json, (uint32_t)strlen(json), &off);
        if (attempt_id[0]) hl_index_put(&g_index, attempt_id, off);
        char resp[128];
        snprintf(resp, sizeof(resp),
                 "{\"ok\":true,\"segment\":%u,\"offset\":%llu}",
                 off.segment, (unsigned long long)off.offset);
        hl_ipc_send_str(client_fd, resp);

    } else if (strcmp(cmd, "store.put_endpoint") == 0) {
        char endpoint_id[64] = {0};
        json_str(json, "endpoint_id", endpoint_id, sizeof(endpoint_id));
        /* payload field contains the full endpoint JSON */
        char payload_val[65536] = {0};
        json_str(json, "payload", payload_val, sizeof(payload_val));
        const char *to_store = payload_val[0] ? payload_val : json;
        hl_offset_t off;
        hl_store_append(&g_store, HL_TYPE_ENDPOINT,
                        (uint8_t *)to_store, (uint32_t)strlen(to_store), &off);
        if (endpoint_id[0]) hl_index_put(&g_index, endpoint_id, off);
        hl_ipc_send_str(client_fd, "{\"ok\":true}");

    } else if (strcmp(cmd, "store.get_endpoint") == 0) {
        char endpoint_id[64] = {0};
        json_str(json, "endpoint_id", endpoint_id, sizeof(endpoint_id));
        hl_offset_t off;
        if (hl_index_get(&g_index, endpoint_id, &off) != 0) {
            hl_ipc_send_str(client_fd, "{\"ok\":false,\"error\":\"not found\"}");
            return;
        }
        uint8_t *payload = NULL; uint32_t plen; uint8_t type;
        if (hl_store_read(&g_store, off, &payload, &plen, &type) != 0) {
            hl_ipc_send_str(client_fd, "{\"ok\":false,\"error\":\"read failed\"}");
            return;
        }
        char *resp = malloc(plen + 32);
        if (resp) {
            snprintf(resp, plen + 32, "{\"ok\":true,\"endpoint\":%s}", (char *)payload);
            hl_ipc_send_str(client_fd, resp);
            free(resp);
        }
        free(payload);

    } else if (strcmp(cmd, "store.list_endpoints") == 0) {
        char tenant_id[64] = {0};
        json_str(json, "tenant_id", tenant_id, sizeof(tenant_id));

        char *resp = malloc(4 * 1024 * 1024);
        if (!resp) { hl_ipc_send_str(client_fd, "{\"ok\":false,\"error\":\"oom\"}"); return; }
        int pos = sprintf(resp, "{\"ok\":true,\"items\":[");
        int first = 1;

        for (int seg = 0; seg <= g_store.current_seg; seg++) {
            char path[512];
            snprintf(path, sizeof(path), "%s/seg-%06d.gps", g_store.data_dir, seg);
            int fd = open(path, O_RDONLY);
            if (fd < 0) break;

            uint64_t cur_offset = 0;
            for (;;) {
                uint8_t hdr[HL_RECORD_HEADER_SIZE];
                if (read(fd, hdr, HL_RECORD_HEADER_SIZE) != HL_RECORD_HEADER_SIZE) break;
                uint32_t magic_be; memcpy(&magic_be, hdr, 4);
                if (ntohl(magic_be) != HL_MAGIC) break;
                uint8_t type = hdr[5];
                uint32_t plen_be; memcpy(&plen_be, hdr + 6, 4);
                uint32_t plen = ntohl(plen_be);

                uint8_t *payload = malloc(plen + 1);
                if (!payload) break;
                if ((uint32_t)read(fd, payload, plen) != plen) { free(payload); break; }
                payload[plen] = '\0';
                uint32_t crc_be;
                if (read(fd, &crc_be, 4) != 4) { free(payload); break; }

                if (type == HL_TYPE_ENDPOINT) {
                    char endpoint_id[64] = {0};
                    char ep_tenant[64]   = {0};
                    json_str((char *)payload, "endpoint_id", endpoint_id, sizeof(endpoint_id));
                    json_str((char *)payload, "tenant_id",   ep_tenant,   sizeof(ep_tenant));
                    hl_offset_t idx_off;
                    /* Only include if index still points to this record (latest, not tombstoned) */
                    if (endpoint_id[0] &&
                        hl_index_get(&g_index, endpoint_id, &idx_off) == 0 &&
                        idx_off.segment == (uint32_t)seg &&
                        idx_off.offset  == cur_offset &&
                        (!tenant_id[0] || strcmp(ep_tenant, tenant_id) == 0)) {
                        if (!first) pos += sprintf(resp + pos, ",");
                        if ((size_t)(pos + plen + 8) < 4 * 1024 * 1024) {
                            memcpy(resp + pos, payload, plen);
                            pos += plen;
                            first = 0;
                        }
                    }
                }
                cur_offset += HL_RECORD_HEADER_SIZE + plen + HL_RECORD_FOOTER_SIZE;
                free(payload);
            }
            close(fd);
        }
        sprintf(resp + pos, "]}");
        hl_ipc_send_str(client_fd, resp);
        free(resp);

    } else if (strcmp(cmd, "store.delete_endpoint") == 0) {
        char endpoint_id[64] = {0};
        json_str(json, "endpoint_id", endpoint_id, sizeof(endpoint_id));
        if (!endpoint_id[0]) {
            hl_ipc_send_str(client_fd, "{\"ok\":false,\"error\":\"missing endpoint_id\"}");
            return;
        }
        char tomb[256];
        snprintf(tomb, sizeof(tomb), "{\"id\":\"%s\",\"type\":\"endpoint\"}", endpoint_id);
        hl_store_append(&g_store, HL_TYPE_TOMBSTONE,
                        (uint8_t *)tomb, (uint32_t)strlen(tomb), NULL);
        hl_index_delete(&g_index, endpoint_id);
        hl_ipc_send_str(client_fd, "{\"ok\":true}");

    } else if (strcmp(cmd, "store.delete_subscription") == 0) {
        char sub_id[64] = {0};
        json_str(json, "subscription_id", sub_id, sizeof(sub_id));
        if (!sub_id[0]) {
            hl_ipc_send_str(client_fd, "{\"ok\":false,\"error\":\"missing subscription_id\"}");
            return;
        }
        char tomb[256];
        snprintf(tomb, sizeof(tomb), "{\"id\":\"%s\",\"type\":\"subscription\"}", sub_id);
        hl_store_append(&g_store, HL_TYPE_TOMBSTONE,
                        (uint8_t *)tomb, (uint32_t)strlen(tomb), NULL);
        hl_index_delete(&g_index, sub_id);
        hl_ipc_send_str(client_fd, "{\"ok\":true}");

    } else if (strcmp(cmd, "store.list_events") == 0) {
        char tenant_id[64]  = {0};
        char after_id[64]   = {0};
        char cursor_str[64] = {0};
        int limit = 50;
        int64_t from_ms = 0, to_ms = INT64_MAX;
        json_str(json, "tenant_id", tenant_id, sizeof(tenant_id));
        json_str(json, "after_id",  after_id,  sizeof(after_id));
        json_str(json, "cursor",    cursor_str, sizeof(cursor_str));
        {int lv = 50; json_int(json, "limit", &lv); limit = (lv > 0 && lv <= 1000) ? lv : 50;}
        from_ms = json_int64(json, "from_ms", 0);
        to_ms   = json_int64(json, "to_ms",   INT64_MAX);

        char *resp = malloc(4 * 1024 * 1024);
        if (!resp) { hl_ipc_send_str(client_fd, "{\"ok\":false,\"error\":\"oom\"}"); return; }
        int pos = sprintf(resp, "{\"ok\":true,\"items\":[");
        int first = 1, count = 0;
        char next_cursor[64] = {0};

        if (tenant_id[0]) {
            /* Fast path: use secondary index — O(tenant_events) instead of O(all) */
            hl_tenant_events_t *te = teidx_get(tenant_id);
            int start_i = 0;

            if (te) {
                if (after_id[0]) {
                    hl_offset_t aoff;
                    if (hl_index_get(&g_index, after_id, &aoff) == 0) {
                        for (int i = 0; i < te->count; i++) {
                            if (te->offsets[i].segment == aoff.segment &&
                                te->offsets[i].offset  == aoff.offset) {
                                start_i = i + 1; break;
                            }
                        }
                    }
                } else if (cursor_str[0]) {
                    uint32_t cseg; uint64_t coff;
                    if (parse_cursor(cursor_str, &cseg, &coff) == 0) {
                        for (int i = 0; i < te->count; i++) {
                            if (te->offsets[i].segment == cseg &&
                                te->offsets[i].offset  == coff) {
                                start_i = i; break;
                            }
                        }
                    }
                }

                for (int i = start_i; i < te->count; i++) {
                    if (count >= limit) {
                        snprintf(next_cursor, sizeof(next_cursor), "%u:%llu",
                                 te->offsets[i].segment,
                                 (unsigned long long)te->offsets[i].offset);
                        break;
                    }
                    uint8_t *payload = NULL; uint32_t plen; uint8_t rtype;
                    if (hl_store_read(&g_store, te->offsets[i],
                                     &payload, &plen, &rtype) != 0) continue;
                    if (rtype != HL_TYPE_EVENT) { free(payload); continue; }

                    int64_t oat = json_int64((char *)payload, "occurred_at", 0);
                    if (oat < from_ms || (to_ms != INT64_MAX && oat > to_ms)) {
                        free(payload); continue;
                    }

                    if ((size_t)(pos + plen + 8) < 4 * 1024 * 1024) {
                        if (!first) pos += sprintf(resp + pos, ",");
                        memcpy(resp + pos, payload, plen);
                        pos += plen;
                        first = 0;
                        count++;
                    }
                    free(payload);
                }
            }
        } else {
            /* Full scan path: no tenant filter (admin / cross-tenant queries) */
            uint32_t start_seg = 0;
            uint64_t start_off = 0;
            int skip_after_id  = 0;

            if (after_id[0]) {
                hl_offset_t aoff;
                if (hl_index_get(&g_index, after_id, &aoff) == 0) {
                    start_seg = aoff.segment;
                    start_off = aoff.offset;
                    skip_after_id = 1;
                }
            } else if (cursor_str[0]) {
                parse_cursor(cursor_str, &start_seg, &start_off);
            }

            for (int seg = (int)start_seg; seg <= g_store.current_seg; seg++) {
                char path[512];
                snprintf(path, sizeof(path), "%s/seg-%06d.gps", g_store.data_dir, seg);
                int fd = open(path, O_RDONLY);
                if (fd < 0) break;

                uint64_t cur_offset = 0;
                if (seg == (int)start_seg && start_off > 0) {
                    lseek(fd, (off_t)start_off, SEEK_SET);
                    cur_offset = start_off;
                }

                int done_seg = 0;
                for (;;) {
                    uint8_t hdr[HL_RECORD_HEADER_SIZE];
                    if (read(fd, hdr, HL_RECORD_HEADER_SIZE) != HL_RECORD_HEADER_SIZE) break;
                    uint32_t magic_be; memcpy(&magic_be, hdr, 4);
                    if (ntohl(magic_be) != HL_MAGIC) break;
                    uint8_t type = hdr[5];
                    uint32_t plen_be; memcpy(&plen_be, hdr + 6, 4);
                    uint32_t plen = ntohl(plen_be);

                    uint8_t *payload = malloc(plen + 1);
                    if (!payload) { done_seg = 1; break; }
                    if ((uint32_t)read(fd, payload, plen) != plen) { free(payload); break; }
                    payload[plen] = '\0';
                    uint32_t crc_be;
                    if (read(fd, &crc_be, 4) != 4) { free(payload); break; }

                    uint64_t this_off = cur_offset;
                    cur_offset += HL_RECORD_HEADER_SIZE + plen + HL_RECORD_FOOTER_SIZE;

                    if (type == HL_TYPE_EVENT) {
                        if (skip_after_id && (uint32_t)seg == start_seg &&
                            this_off == start_off) {
                            free(payload);
                            skip_after_id = 0;
                            continue;
                        }
                        int64_t oat = json_int64((char *)payload, "occurred_at", 0);
                        if (oat < from_ms) { free(payload); continue; }
                        if (to_ms != INT64_MAX && oat > to_ms) { free(payload); continue; }

                        if (count >= limit) {
                            snprintf(next_cursor, sizeof(next_cursor), "%u:%llu",
                                     (uint32_t)seg, (unsigned long long)this_off);
                            free(payload);
                            done_seg = 1;
                            break;
                        }

                        if ((size_t)(pos + plen + 8) < 4 * 1024 * 1024) {
                            if (!first) pos += sprintf(resp + pos, ",");
                            memcpy(resp + pos, payload, plen);
                            pos += plen;
                            first = 0;
                            count++;
                        }
                    }
                    free(payload);
                }
                close(fd);
                if (done_seg) break;
            }
        }

        if (next_cursor[0])
            pos += sprintf(resp + pos, "],\"next_cursor\":\"%s\"}", next_cursor);
        else
            pos += sprintf(resp + pos, "]}");
        hl_ipc_send_str(client_fd, resp);
        free(resp);

    } else if (strcmp(cmd, "store.delete_dlq") == 0) {
        char job_id[HL_DLQ_JOB_ID_LEN] = {0};
        json_str(json, "job_id", job_id, sizeof(job_id));
        if (hl_dlq_delete(&g_dlq, job_id) == 0)
            hl_ipc_send_str(client_fd, "{\"ok\":true}");
        else
            hl_ipc_send_str(client_fd, "{\"ok\":false,\"error\":\"not found\"}");

    } else if (strcmp(cmd, "store.list_attempts") == 0) {
        char job_id_f[64]      = {0};
        char event_id_f[64]    = {0};
        char endpoint_id_f[64] = {0};
        char attempt_id_f[64]  = {0};
        int limit = 100;
        json_str(json, "job_id",      job_id_f,      sizeof(job_id_f));
        json_str(json, "event_id",    event_id_f,    sizeof(event_id_f));
        json_str(json, "endpoint_id", endpoint_id_f, sizeof(endpoint_id_f));
        json_str(json, "attempt_id",  attempt_id_f,  sizeof(attempt_id_f));
        {int lv = 100; json_int(json, "limit", &lv); limit = (lv > 0 && lv <= 1000) ? lv : 100;}

        char *resp = malloc(4 * 1024 * 1024);
        if (!resp) { hl_ipc_send_str(client_fd, "{\"ok\":false,\"error\":\"oom\"}"); return; }
        int pos = sprintf(resp, "{\"ok\":true,\"items\":[");
        int first = 1, count = 0;

        for (int seg = 0; seg <= g_store.current_seg && count < limit; seg++) {
            char path[512];
            snprintf(path, sizeof(path), "%s/seg-%06d.gps", g_store.data_dir, seg);
            int fd = open(path, O_RDONLY);
            if (fd < 0) break;

            for (;;) {
                uint8_t hdr[HL_RECORD_HEADER_SIZE];
                if (read(fd, hdr, HL_RECORD_HEADER_SIZE) != HL_RECORD_HEADER_SIZE) break;
                uint32_t magic_be; memcpy(&magic_be, hdr, 4);
                if (ntohl(magic_be) != HL_MAGIC) break;
                uint8_t type = hdr[5];
                uint32_t plen_be; memcpy(&plen_be, hdr + 6, 4);
                uint32_t plen = ntohl(plen_be);

                uint8_t *payload = malloc(plen + 1);
                if (!payload) break;
                if ((uint32_t)read(fd, payload, plen) != plen) { free(payload); break; }
                payload[plen] = '\0';
                uint32_t crc_be;
                if (read(fd, &crc_be, 4) != 4) { free(payload); break; }

                if (type == HL_TYPE_ATTEMPT && count < limit) {
                    int match = 1;
                    if (attempt_id_f[0]) {
                        char rec[64] = {0};
                        json_str((char *)payload, "attempt_id", rec, sizeof(rec));
                        if (strcmp(rec, attempt_id_f) != 0) match = 0;
                    }
                    if (match && job_id_f[0]) {
                        char rec[64] = {0};
                        json_str((char *)payload, "job_id", rec, sizeof(rec));
                        if (strcmp(rec, job_id_f) != 0) match = 0;
                    }
                    if (match && event_id_f[0]) {
                        char rec[64] = {0};
                        json_str((char *)payload, "event_id", rec, sizeof(rec));
                        if (strcmp(rec, event_id_f) != 0) match = 0;
                    }
                    if (match && endpoint_id_f[0]) {
                        char rec[64] = {0};
                        json_str((char *)payload, "endpoint_id", rec, sizeof(rec));
                        if (strcmp(rec, endpoint_id_f) != 0) match = 0;
                    }
                    if (match && (size_t)(pos + plen + 8) < 4 * 1024 * 1024) {
                        if (!first) pos += sprintf(resp + pos, ",");
                        memcpy(resp + pos, payload, plen);
                        pos += plen;
                        first = 0;
                        count++;
                    }
                }
                free(payload);
            }
            close(fd);
        }
        sprintf(resp + pos, "]}");
        hl_ipc_send_str(client_fd, resp);
        free(resp);

    } else if (strcmp(cmd, "store.compact") == 0) {
        int retention_secs = 7 * 24 * 3600;
        json_int(json, "retention_secs", &retention_secs);
        time_t cutoff = time(NULL) - retention_secs;
        int deleted = 0;
        long long bytes_freed = 0;
        long long oldest_seg_ts = 0;
        for (int seg = 0; seg < g_store.current_seg; seg++) {
            char path[512];
            snprintf(path, sizeof(path), "%s/seg-%06d.gps", g_store.data_dir, seg);
            struct stat st;
            if (stat(path, &st) != 0) continue;
            if (st.st_mtime > cutoff) {
                if (oldest_seg_ts == 0 || (long long)st.st_mtime < oldest_seg_ts)
                    oldest_seg_ts = (long long)st.st_mtime;
                continue;
            }
            bytes_freed += (long long)st.st_size;
            if (unlink(path) == 0) {
                hl_index_purge_segment(&g_index, (uint32_t)seg);
                deleted++;
            }
        }
        char resp_compact[192];
        snprintf(resp_compact, sizeof(resp_compact),
                 "{\"ok\":true,\"segments_deleted\":%d,\"bytes_freed\":%lld,\"oldest_seg_ts\":%lld}",
                 deleted, bytes_freed, oldest_seg_ts);
        hl_ipc_send_str(client_fd, resp_compact);

    } else if (strcmp(cmd, "store.put_subscription") == 0) {
        char sub_id[64] = {0};
        json_str(json, "subscription_id", sub_id, sizeof(sub_id));
        hl_offset_t off;
        hl_store_append(&g_store, HL_TYPE_SUBSCRIPTION,
                        (uint8_t *)json, (uint32_t)strlen(json), &off);
        if (sub_id[0]) hl_index_put(&g_index, sub_id, off);
        hl_ipc_send_str(client_fd, "{\"ok\":true}");

    } else if (strcmp(cmd, "store.list_subscriptions") == 0) {
        char tenant_id[64] = {0};
        json_str(json, "tenant_id", tenant_id, sizeof(tenant_id));

        char *resp = malloc(4 * 1024 * 1024);
        if (!resp) { hl_ipc_send_str(client_fd, "{\"ok\":false,\"error\":\"oom\"}"); return; }
        int pos = sprintf(resp, "{\"ok\":true,\"items\":[");
        int first = 1;

        for (int seg = 0; seg <= g_store.current_seg; seg++) {
            char path[512];
            snprintf(path, sizeof(path), "%s/seg-%06d.gps", g_store.data_dir, seg);
            int fd = open(path, O_RDONLY);
            if (fd < 0) break;

            uint64_t cur_offset = 0;
            for (;;) {
                uint8_t hdr[HL_RECORD_HEADER_SIZE];
                if (read(fd, hdr, HL_RECORD_HEADER_SIZE) != HL_RECORD_HEADER_SIZE) break;
                uint32_t magic_be; memcpy(&magic_be, hdr, 4);
                if (ntohl(magic_be) != HL_MAGIC) break;
                uint8_t type = hdr[5];
                uint32_t plen_be; memcpy(&plen_be, hdr + 6, 4);
                uint32_t plen = ntohl(plen_be);

                uint8_t *payload = malloc(plen + 1);
                if (!payload) break;
                if ((uint32_t)read(fd, payload, plen) != plen) { free(payload); break; }
                payload[plen] = '\0';

                uint32_t crc_be;
                if (read(fd, &crc_be, 4) != 4) { free(payload); break; }

                if (type == HL_TYPE_SUBSCRIPTION) {
                    char sub_id[64]     = {0};
                    char rec_tenant[64] = {0};
                    json_str((char *)payload, "subscription_id", sub_id,     sizeof(sub_id));
                    json_str((char *)payload, "tenant_id",       rec_tenant, sizeof(rec_tenant));

                    /* Skip tombstoned (deleted) subscriptions */
                    hl_offset_t idx_off;
                    int active = sub_id[0] &&
                                 hl_index_get(&g_index, sub_id, &idx_off) == 0 &&
                                 idx_off.segment == (uint32_t)seg &&
                                 idx_off.offset  == cur_offset;

                    if (active && (!tenant_id[0] || strcmp(rec_tenant, tenant_id) == 0)) {
                        if (!first) pos += sprintf(resp + pos, ",");
                        if ((size_t)(pos + plen + 8) < 4 * 1024 * 1024) {
                            memcpy(resp + pos, payload, plen);
                            pos += plen;
                            first = 0;
                        }
                    }
                }
                cur_offset += HL_RECORD_HEADER_SIZE + plen + HL_RECORD_FOOTER_SIZE;
                free(payload);
            }
            close(fd);
        }
        sprintf(resp + pos, "]}");
        hl_ipc_send_str(client_fd, resp);
        free(resp);

    } else if (strcmp(cmd, "store.queue_stats") == 0) {
        char qs_tenant[64] = {0};
        json_str(json, "tenant_id", qs_tenant, sizeof(qs_tenant));
        int pending = 0, inflight = 0;
        hl_job_t *j = g_queue.head;
        while (j) {
            if (qs_tenant[0] && strcmp(j->tenant_id, qs_tenant) != 0) {
                j = j->next; continue;
            }
            if (j->state == HL_JOB_STATE_PENDING ||
                j->state == HL_JOB_STATE_NACKED) {
                pending++;
            } else if (j->state == HL_JOB_STATE_CLAIMED) {
                inflight++;
            }
            j = j->next;
        }
        char resp_qs[128];
        snprintf(resp_qs, sizeof(resp_qs),
                 "{\"ok\":true,\"pending\":%d,\"inflight\":%d}", pending, inflight);
        hl_ipc_send_str(client_fd, resp_qs);

    } else if (strcmp(cmd, "store.append_audit") == 0) {
        /* Append an audit log record */
        char entry_val[65536] = {0};
        json_str(json, "entry", entry_val, sizeof(entry_val));
        hl_offset_t off_audit;
        const char *to_store_audit = entry_val[0] ? entry_val : json;
        hl_store_append(&g_store, HL_TYPE_AUDIT,
                        (uint8_t *)to_store_audit, (uint32_t)strlen(to_store_audit), &off_audit);
        char resp_audit[128];
        snprintf(resp_audit, sizeof(resp_audit),
                 "{\"ok\":true,\"segment\":%u,\"offset\":%llu}",
                 off_audit.segment, (unsigned long long)off_audit.offset);
        hl_ipc_send_str(client_fd, resp_audit);

    } else if (strcmp(cmd, "store.list_audit") == 0) {
        char audit_tenant[64] = {0};
        int audit_limit = 100;
        json_str(json, "tenant_id", audit_tenant, sizeof(audit_tenant));
        {int lv = 100; json_int(json, "limit", &lv); audit_limit = (lv > 0 && lv <= 1000) ? lv : 100;}

        char *resp_la = malloc(4 * 1024 * 1024);
        if (!resp_la) { hl_ipc_send_str(client_fd, "\"ok\":false,\"error\":\"oom\"}"); return; }
        int pos_la = sprintf(resp_la, "\"ok\":true,\"items\":[");
        int first_la = 1, count_la = 0;

        for (int seg_la = 0; seg_la <= g_store.current_seg && count_la < audit_limit; seg_la++) {
            char path_la[512];
            snprintf(path_la, sizeof(path_la), "%s/seg-%06d.gps", g_store.data_dir, seg_la);
            int fd_la = open(path_la, O_RDONLY);
            if (fd_la < 0) break;
            for (;;) {
                uint8_t hdr_la[HL_RECORD_HEADER_SIZE];
                if (read(fd_la, hdr_la, HL_RECORD_HEADER_SIZE) != HL_RECORD_HEADER_SIZE) break;
                uint32_t magic_la; memcpy(&magic_la, hdr_la, 4);
                if (ntohl(magic_la) != HL_MAGIC) break;
                uint8_t type_la = hdr_la[5];
                uint32_t plen_la_be; memcpy(&plen_la_be, hdr_la + 6, 4);
                uint32_t plen_la = ntohl(plen_la_be);
                uint8_t *payload_la = malloc(plen_la + 1);
                if (!payload_la) break;
                if ((uint32_t)read(fd_la, payload_la, plen_la) != plen_la) { free(payload_la); break; }
                payload_la[plen_la] = '\0';
                uint32_t crc_la_be;
                if (read(fd_la, &crc_la_be, 4) != 4) { free(payload_la); break; }
                if (type_la == HL_TYPE_AUDIT && count_la < audit_limit) {
                    char rec_tenant_la[64] = {0};
                    json_str((char *)payload_la, "tenant_id", rec_tenant_la, sizeof(rec_tenant_la));
                    if (!audit_tenant[0] || strcmp(rec_tenant_la, audit_tenant) == 0) {
                        if ((size_t)(pos_la + plen_la + 8) < 4 * 1024 * 1024) {
                            if (!first_la) pos_la += sprintf(resp_la + pos_la, ",");
                            memcpy(resp_la + pos_la, payload_la, plen_la);
                            pos_la += plen_la;
                            first_la = 0;
                            count_la++;
                        }
                    }
                }
                free(payload_la);
            }
            close(fd_la);
        }
        sprintf(resp_la + pos_la, "]}");
        hl_ipc_send_str(client_fd, resp_la);
        free(resp_la);

    } else {
        char resp[128];
        snprintf(resp, sizeof(resp), "{\"ok\":false,\"error\":\"unknown command: %s\"}", cmd);
        hl_ipc_send_str(client_fd, resp);
    }
}

/* ---------- main ---------- */
int main(int argc, char *argv[]) {
    const char *socket_path = argc > 1 ? argv[1] : "/tmp/hl_store.sock";
    const char *data_dir    = argc > 2 ? argv[2] : "/var/lib/gatepulse";

    signal(SIGINT,  handle_signal);
    signal(SIGTERM, handle_signal);
    signal(SIGPIPE, SIG_IGN);

    if (hl_store_init(&g_store, data_dir) < 0) {
        fprintf(stderr, "Failed to init store at %s: %s\n", data_dir, strerror(errno));
        return 1;
    }
    hl_index_init(&g_index);
    hl_queue_init(&g_queue);
    hl_dlq_init(&g_dlq);

    /* Rebuild in-memory state from persistent log */
    recover_from_store();

    /* Create UNIX socket */
    int server_fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (server_fd < 0) { perror("socket"); return 1; }

    unlink(socket_path);
    struct sockaddr_un addr = {0};
    addr.sun_family = AF_UNIX;
    strncpy(addr.sun_path, socket_path, sizeof(addr.sun_path) - 1);

    if (bind(server_fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        perror("bind"); return 1;
    }
    if (listen(server_fd, 64) < 0) { perror("listen"); return 1; }

    fprintf(stdout, "hl_store listening on %s, data at %s\n", socket_path, data_dir);
    fflush(stdout);

    int clients[256];
    int nclients = 0;
    memset(clients, -1, sizeof(clients));

    while (g_running) {
        fd_set read_fds;
        FD_ZERO(&read_fds);
        int max_fd = server_fd;
        FD_SET(server_fd, &read_fds);
        for (int i = 0; i < nclients; i++) {
            if (clients[i] >= 0) {
                FD_SET(clients[i], &read_fds);
                if (clients[i] > max_fd) max_fd = clients[i];
            }
        }

        struct timeval tv = {1, 0};
        int rc = select(max_fd + 1, &read_fds, NULL, NULL, &tv);
        if (rc < 0 && errno == EINTR) continue;
        if (rc < 0) break;

        if (FD_ISSET(server_fd, &read_fds)) {
            int cfd = accept(server_fd, NULL, NULL);
            if (cfd >= 0 && nclients < 256)
                clients[nclients++] = cfd;
        }

        for (int i = 0; i < nclients; i++) {
            if (clients[i] < 0 || !FD_ISSET(clients[i], &read_fds)) continue;
            uint8_t *buf = NULL; uint32_t len = 0;
            if (hl_ipc_recv(clients[i], &buf, &len) < 0) {
                close(clients[i]);
                clients[i] = -1;
                continue;
            }
            dispatch(clients[i], (char *)buf);
            free(buf);
        }
    }

    for (int i = 0; i < nclients; i++)
        if (clients[i] >= 0) close(clients[i]);
    close(server_fd);
    unlink(socket_path);
    hl_store_close(&g_store);
    hl_index_free(&g_index);
    hl_queue_free(&g_queue);
    hl_dlq_free(&g_dlq);
    teidx_free();

    fprintf(stdout, "hl_store exited cleanly\n");
    return 0;
}
