#ifndef GP_DLQ_H
#define GP_DLQ_H

#include "store.h"
#include <stdint.h>
#include <time.h>

#define GP_DLQ_JOB_ID_LEN   64
#define GP_DLQ_EVENT_ID_LEN 64
#define GP_DLQ_EP_ID_LEN    64
#define GP_DLQ_REASON_LEN   256

typedef struct gp_dlq_entry {
    char     job_id[GP_DLQ_JOB_ID_LEN];
    char     event_id[GP_DLQ_EVENT_ID_LEN];
    char     endpoint_id[GP_DLQ_EP_ID_LEN];
    char     tenant_id[64];
    char     reason[GP_DLQ_REASON_LEN];
    int      attempt_count;
    time_t   failed_at;
    gp_offset_t store_offset;
    struct gp_dlq_entry *next;
} gp_dlq_entry_t;

typedef struct {
    gp_dlq_entry_t *head;
    int             count;
} gp_dlq_t;

void gp_dlq_init(gp_dlq_t *dlq);
void gp_dlq_free(gp_dlq_t *dlq);

int  gp_dlq_put(gp_dlq_t *dlq, const gp_dlq_entry_t *entry);
int  gp_dlq_list(gp_dlq_t *dlq, const char *tenant_id,
                 gp_dlq_entry_t **out, int *count);
int  gp_dlq_delete(gp_dlq_t *dlq, const char *job_id);

#endif /* GP_DLQ_H */
