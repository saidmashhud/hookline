#ifndef HL_DLQ_H
#define HL_DLQ_H

#include "store.h"
#include <stdint.h>
#include <time.h>

#define HL_DLQ_JOB_ID_LEN   64
#define HL_DLQ_EVENT_ID_LEN 64
#define HL_DLQ_EP_ID_LEN    64
#define HL_DLQ_REASON_LEN   256

typedef struct hl_dlq_entry {
    char     job_id[HL_DLQ_JOB_ID_LEN];
    char     event_id[HL_DLQ_EVENT_ID_LEN];
    char     endpoint_id[HL_DLQ_EP_ID_LEN];
    char     tenant_id[64];
    char     reason[HL_DLQ_REASON_LEN];
    int      attempt_count;
    time_t   failed_at;
    hl_offset_t store_offset;
    struct hl_dlq_entry *next;
} hl_dlq_entry_t;

typedef struct {
    hl_dlq_entry_t *head;
    int             count;
} hl_dlq_t;

void hl_dlq_init(hl_dlq_t *dlq);
void hl_dlq_free(hl_dlq_t *dlq);

int  hl_dlq_put(hl_dlq_t *dlq, const hl_dlq_entry_t *entry);
int  hl_dlq_list(hl_dlq_t *dlq, const char *tenant_id,
                 hl_dlq_entry_t **out, int *count);
int  hl_dlq_delete(hl_dlq_t *dlq, const char *job_id);
int  hl_dlq_delete_tenant(hl_dlq_t *dlq, const char *job_id,
                          const char *tenant_id);

#endif /* HL_DLQ_H */
