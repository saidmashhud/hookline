#include "queue.h"
#include <stdlib.h>
#include <string.h>
#include <time.h>

void gp_queue_init(gp_queue_t *q) {
    q->head = NULL;
    q->tail = NULL;
    q->count = 0;
}

void gp_queue_free(gp_queue_t *q) {
    gp_job_t *j = q->head;
    while (j) {
        gp_job_t *next = j->next;
        free(j);
        j = next;
    }
    q->head = NULL;
    q->tail = NULL;
    q->count = 0;
}

int gp_queue_enqueue(gp_queue_t *q, const gp_job_t *job) {
    gp_job_t *j = calloc(1, sizeof(*j));
    if (!j) return -1;
    *j = *job;
    j->next  = NULL;
    j->state = GP_JOB_STATE_PENDING;
    if (!q->tail) {
        q->head = q->tail = j;
    } else {
        q->tail->next = j;
        q->tail = j;
    }
    q->count++;
    return 0;
}

int gp_queue_claim(gp_queue_t *q, gp_job_t **out, int max_count, int lease_secs) {
    time_t now = time(NULL);
    int claimed = 0;
    gp_job_t *j = q->head;
    while (j && claimed < max_count) {
        if (j->state == GP_JOB_STATE_PENDING && j->next_attempt_at <= now) {
            j->state = GP_JOB_STATE_CLAIMED;
            j->lease_expires_at = now + lease_secs;
            j->attempt_count++;
            out[claimed++] = j;
        }
        j = j->next;
    }
    return claimed;
}

int gp_queue_ack(gp_queue_t *q, const char *job_id) {
    gp_job_t *j = q->head;
    while (j) {
        if (strcmp(j->job_id, job_id) == 0) {
            j->state = GP_JOB_STATE_ACKED;
            q->count--;
            return 0;
        }
        j = j->next;
    }
    return -1;
}

int gp_queue_nack(gp_queue_t *q, const char *job_id, int delay_secs) {
    gp_job_t *j = q->head;
    while (j) {
        if (strcmp(j->job_id, job_id) == 0) {
            if (j->attempt_count >= j->max_attempts) {
                j->state = GP_JOB_STATE_DLQ;
            } else {
                j->state = GP_JOB_STATE_PENDING;
                j->next_attempt_at = time(NULL) + delay_secs;
            }
            return 0;
        }
        j = j->next;
    }
    return -1;
}

int gp_queue_reap_leases(gp_queue_t *q) {
    time_t now = time(NULL);
    int reaped = 0;
    gp_job_t *j = q->head;
    while (j) {
        if (j->state == GP_JOB_STATE_CLAIMED && j->lease_expires_at < now) {
            j->state = GP_JOB_STATE_PENDING;
            j->next_attempt_at = now;
            reaped++;
        }
        j = j->next;
    }
    return reaped;
}
