#ifndef GP_STORE_H
#define GP_STORE_H

#include <stdint.h>
#include <stddef.h>

/* Record magic bytes */
#define GP_MAGIC        0x47505354u  /* "GPST" */
#define GP_VERSION      1

/* Record types */
#define GP_TYPE_EVENT        1
#define GP_TYPE_JOB          2
#define GP_TYPE_ATTEMPT      3
#define GP_TYPE_DLQ          4
#define GP_TYPE_ENDPOINT     5
#define GP_TYPE_SUBSCRIPTION 6
#define GP_TYPE_ACK          7   /* job ACK tombstone for crash recovery */
#define GP_TYPE_AUDIT     0x09
#define GP_TYPE_TOMBSTONE    8   /* delete tombstone for endpoints/subscriptions */

/* Segment size: 256MB */
#define GP_SEGMENT_SIZE  (256 * 1024 * 1024)

/* Record header layout:
 *   [magic:4][version:1][type:1][length:4][payload:N][crc32:4]
 */
#define GP_RECORD_HEADER_SIZE 10
#define GP_RECORD_FOOTER_SIZE 4
#define GP_RECORD_OVERHEAD    (GP_RECORD_HEADER_SIZE + GP_RECORD_FOOTER_SIZE)

typedef struct {
    char     *data_dir;
    int       current_seg;   /* current segment index */
    int       seg_fd;        /* current segment file descriptor */
    uint64_t  seg_offset;    /* write offset within current segment */
} gp_store_t;

typedef struct {
    uint32_t magic;
    uint8_t  version;
    uint8_t  type;
    uint32_t length;
    /* payload follows */
    /* crc32 follows payload */
} __attribute__((packed)) gp_record_hdr_t;

/* Store offset: segment + offset within segment */
typedef struct {
    uint32_t segment;
    uint64_t offset;
} gp_offset_t;

int  gp_store_init(gp_store_t *store, const char *data_dir);
void gp_store_close(gp_store_t *store);

/* Returns byte offset of the appended record */
int  gp_store_append(gp_store_t *store, uint8_t type,
                     const uint8_t *payload, uint32_t length,
                     gp_offset_t *out_offset);

/* Read payload at offset; caller frees *payload */
int  gp_store_read(gp_store_t *store, gp_offset_t offset,
                   uint8_t **payload, uint32_t *length, uint8_t *type);

uint32_t gp_crc32(const uint8_t *data, size_t length);

#endif /* GP_STORE_H */
