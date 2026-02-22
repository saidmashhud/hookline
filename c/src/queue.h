#ifndef HL_QUEUE_H
#define HL_QUEUE_H

#include "store.h"
#include <stdint.h>
#include <time.h>

#define HL_JOB_STATE_PENDING  0
#define HL_JOB_STATE_CLAIMED  1
#define HL_JOB_STATE_ACKED    2
#define HL_JOB_STATE_NACKED   3
#define HL_JOB_STATE_DLQ      4

#define HL_JOB_ID_LEN    64
#define HL_EVENT_ID_LEN  64
#define HL_EP_ID_LEN     64

typedef struct hl_job {
    char     job_id[HL_JOB_ID_LEN];
    char     event_id[HL_EVENT_ID_LEN];
    char     endpoint_id[HL_EP_ID_LEN];
    char     tenant_id[64];
    int      state;
    int      attempt_count;
    int      max_attempts;
    time_t   next_attempt_at;
    time_t   lease_expires_at;
    hl_offset_t store_offset;
    struct hl_job *next;
} hl_job_t;

typedef struct {
    hl_job_t *head;
    hl_job_t *tail;
    int       count;
} hl_queue_t;

void hl_queue_init(hl_queue_t *q);
void hl_queue_free(hl_queue_t *q);

int  hl_queue_enqueue(hl_queue_t *q, const hl_job_t *job);
/* Claim up to max_count jobs, returns count actually claimed */
int  hl_queue_claim(hl_queue_t *q, hl_job_t **out, int max_count,
                    int lease_secs);
/* Claim jobs belonging to a specific tenant; pass NULL/empty to claim any */
int  hl_queue_claim_tenant(hl_queue_t *q, hl_job_t **out, int max_count,
                           int lease_secs, const char *tenant_id);
int  hl_queue_ack(hl_queue_t *q, const char *job_id);
int  hl_queue_nack(hl_queue_t *q, const char *job_id, int delay_secs);
int  hl_queue_reap_leases(hl_queue_t *q);  /* re-queue expired leases */

#endif /* HL_QUEUE_H */
