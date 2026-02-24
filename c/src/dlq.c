#include "dlq.h"
#include <stdlib.h>
#include <string.h>
#include <time.h>

void hl_dlq_init(hl_dlq_t *dlq) {
    dlq->head  = NULL;
    dlq->count = 0;
}

void hl_dlq_free(hl_dlq_t *dlq) {
    hl_dlq_entry_t *e = dlq->head;
    while (e) {
        hl_dlq_entry_t *next = e->next;
        free(e);
        e = next;
    }
    dlq->head  = NULL;
    dlq->count = 0;
}

int hl_dlq_put(hl_dlq_t *dlq, const hl_dlq_entry_t *entry) {
    hl_dlq_entry_t *e = calloc(1, sizeof(*e));
    if (!e) return -1;
    *e = *entry;
    e->next    = dlq->head;
    dlq->head  = e;
    dlq->count++;
    return 0;
}

int hl_dlq_list(hl_dlq_t *dlq, const char *tenant_id,
                hl_dlq_entry_t **out, int *count) {
    int n = 0;
    hl_dlq_entry_t *e = dlq->head;
    while (e) {
        if (!tenant_id || strcmp(e->tenant_id, tenant_id) == 0) n++;
        e = e->next;
    }
    if (n == 0) { *count = 0; *out = NULL; return 0; }

    hl_dlq_entry_t *arr = calloc((size_t)n, sizeof(*arr));
    if (!arr) return -1;

    int i = 0;
    e = dlq->head;
    while (e) {
        if (!tenant_id || strcmp(e->tenant_id, tenant_id) == 0)
            arr[i++] = *e;
        e = e->next;
    }
    *out   = arr;
    *count = n;
    return 0;
}

int hl_dlq_delete(hl_dlq_t *dlq, const char *job_id) {
    hl_dlq_entry_t **pp = &dlq->head;
    while (*pp) {
        if (strcmp((*pp)->job_id, job_id) == 0) {
            hl_dlq_entry_t *del = *pp;
            *pp = del->next;
            free(del);
            dlq->count--;
            return 0;
        }
        pp = &(*pp)->next;
    }
    return -1;
}

int hl_dlq_delete_tenant(hl_dlq_t *dlq, const char *job_id, const char *tenant_id) {
    hl_dlq_entry_t **pp = &dlq->head;
    while (*pp) {
        if (strcmp((*pp)->job_id, job_id) == 0 &&
            (!tenant_id || !tenant_id[0] || strcmp((*pp)->tenant_id, tenant_id) == 0)) {
            hl_dlq_entry_t *del = *pp;
            *pp = del->next;
            free(del);
            dlq->count--;
            return 0;
        }
        pp = &(*pp)->next;
    }
    return -1;
}
