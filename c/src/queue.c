#include "queue.h"
#include <stdlib.h>
#include <string.h>
#include <time.h>

void hl_queue_init(hl_queue_t *q) {
    q->head = NULL;
    q->tail = NULL;
    q->count = 0;
}

void hl_queue_free(hl_queue_t *q) {
    hl_job_t *j = q->head;
    while (j) {
        hl_job_t *next = j->next;
        free(j);
        j = next;
    }
    q->head = NULL;
    q->tail = NULL;
    q->count = 0;
}

int hl_queue_enqueue(hl_queue_t *q, const hl_job_t *job) {
    hl_job_t *j = calloc(1, sizeof(*j));
    if (!j) return -1;
    *j = *job;
    j->next  = NULL;
    j->state = HL_JOB_STATE_PENDING;
    if (!q->tail) {
        q->head = q->tail = j;
    } else {
        q->tail->next = j;
        q->tail = j;
    }
    q->count++;
    return 0;
}

int hl_queue_claim(hl_queue_t *q, hl_job_t **out, int max_count, int lease_secs) {
    time_t now = time(NULL);
    int claimed = 0;
    hl_job_t *j = q->head;
    while (j && claimed < max_count) {
        if (j->state == HL_JOB_STATE_PENDING && j->next_attempt_at <= now) {
            j->state = HL_JOB_STATE_CLAIMED;
            j->lease_expires_at = now + lease_secs;
            j->attempt_count++;
            out[claimed++] = j;
        }
        j = j->next;
    }
    return claimed;
}

int hl_queue_claim_tenant(hl_queue_t *q, hl_job_t **out, int max_count,
                          int lease_secs, const char *tenant_id) {
    time_t now = time(NULL);
    int claimed = 0;
    hl_job_t *j = q->head;
    while (j && claimed < max_count) {
        if (j->state == HL_JOB_STATE_PENDING && j->next_attempt_at <= now) {
            if (!tenant_id || !tenant_id[0] ||
                strcmp(j->tenant_id, tenant_id) == 0) {
                j->state = HL_JOB_STATE_CLAIMED;
                j->lease_expires_at = now + lease_secs;
                j->attempt_count++;
                out[claimed++] = j;
            }
        }
        j = j->next;
    }
    return claimed;
}

int hl_queue_ack(hl_queue_t *q, const char *job_id) {
    hl_job_t *j = q->head;
    while (j) {
        if (strcmp(j->job_id, job_id) == 0) {
            j->state = HL_JOB_STATE_ACKED;
            q->count--;
            return 0;
        }
        j = j->next;
    }
    return -1;
}

int hl_queue_nack(hl_queue_t *q, const char *job_id, int delay_secs) {
    hl_job_t *j = q->head;
    while (j) {
        if (strcmp(j->job_id, job_id) == 0) {
            if (j->attempt_count >= j->max_attempts) {
                j->state = HL_JOB_STATE_DLQ;
            } else {
                j->state = HL_JOB_STATE_PENDING;
                j->next_attempt_at = time(NULL) + delay_secs;
            }
            return 0;
        }
        j = j->next;
    }
    return -1;
}

int hl_queue_reap_leases(hl_queue_t *q) {
    time_t now = time(NULL);
    int reaped = 0;
    hl_job_t *j = q->head;
    while (j) {
        if (j->state == HL_JOB_STATE_CLAIMED && j->lease_expires_at < now) {
            j->state = HL_JOB_STATE_PENDING;
            j->next_attempt_at = now;
            reaped++;
        }
        j = j->next;
    }
    return reaped;
}
