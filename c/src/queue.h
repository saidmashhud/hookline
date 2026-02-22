#ifndef GP_QUEUE_H
#define GP_QUEUE_H

#include "store.h"
#include <stdint.h>
#include <time.h>

#define GP_JOB_STATE_PENDING  0
#define GP_JOB_STATE_CLAIMED  1
#define GP_JOB_STATE_ACKED    2
#define GP_JOB_STATE_NACKED   3
#define GP_JOB_STATE_DLQ      4

#define GP_JOB_ID_LEN    64
#define GP_EVENT_ID_LEN  64
#define GP_EP_ID_LEN     64

typedef struct gp_job {
    char     job_id[GP_JOB_ID_LEN];
    char     event_id[GP_EVENT_ID_LEN];
    char     endpoint_id[GP_EP_ID_LEN];
    char     tenant_id[64];
    int      state;
    int      attempt_count;
    int      max_attempts;
    time_t   next_attempt_at;
    time_t   lease_expires_at;
    gp_offset_t store_offset;
    struct gp_job *next;
} gp_job_t;

typedef struct {
    gp_job_t *head;
    gp_job_t *tail;
    int       count;
} gp_queue_t;

void gp_queue_init(gp_queue_t *q);
void gp_queue_free(gp_queue_t *q);

int  gp_queue_enqueue(gp_queue_t *q, const gp_job_t *job);
/* Claim up to max_count jobs, returns count actually claimed */
int  gp_queue_claim(gp_queue_t *q, gp_job_t **out, int max_count,
                    int lease_secs);
int  gp_queue_ack(gp_queue_t *q, const char *job_id);
int  gp_queue_nack(gp_queue_t *q, const char *job_id, int delay_secs);
int  gp_queue_reap_leases(gp_queue_t *q);  /* re-queue expired leases */

#endif /* GP_QUEUE_H */
