#ifndef GP_INDEX_H
#define GP_INDEX_H

#include "store.h"
#include <stdint.h>

#define GP_INDEX_BUCKETS 65536

typedef struct gp_index_entry {
    char              key[128];
    gp_offset_t       offset;
    struct gp_index_entry *next;
} gp_index_entry_t;

typedef struct {
    gp_index_entry_t *buckets[GP_INDEX_BUCKETS];
} gp_index_t;

void gp_index_init(gp_index_t *idx);
void gp_index_free(gp_index_t *idx);

int  gp_index_put(gp_index_t *idx, const char *key, gp_offset_t offset);
int  gp_index_get(gp_index_t *idx, const char *key, gp_offset_t *offset);
int  gp_index_delete(gp_index_t *idx, const char *key);
void gp_index_purge_segment(gp_index_t *idx, uint32_t segment);

#endif /* GP_INDEX_H */
