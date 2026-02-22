#include "store.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <sys/stat.h>
#include <arpa/inet.h>

/* --- CRC32 (ISO 3309 / IEEE 802.3) --- */
static uint32_t crc32_table[256];
static int      crc32_init_done = 0;

static void crc32_init_table(void) {
    for (uint32_t i = 0; i < 256; i++) {
        uint32_t c = i;
        for (int j = 0; j < 8; j++)
            c = (c & 1) ? (0xEDB88320u ^ (c >> 1)) : (c >> 1);
        crc32_table[i] = c;
    }
    crc32_init_done = 1;
}

uint32_t gp_crc32(const uint8_t *data, size_t length) {
    if (!crc32_init_done) crc32_init_table();
    uint32_t crc = 0xFFFFFFFFu;
    for (size_t i = 0; i < length; i++)
        crc = crc32_table[(crc ^ data[i]) & 0xFF] ^ (crc >> 8);
    return crc ^ 0xFFFFFFFFu;
}

/* --- Segment helpers --- */
static char *seg_path(const char *data_dir, int seg_idx) {
    char *path = malloc(512);
    if (!path) return NULL;
    snprintf(path, 512, "%s/seg-%06d.gps", data_dir, seg_idx);
    return path;
}

static int open_segment(gp_store_t *store, int seg_idx, int create) {
    char *path = seg_path(store->data_dir, seg_idx);
    if (!path) return -1;
    int flags = O_RDWR | (create ? O_CREAT : 0);
    int fd = open(path, flags, 0644);
    free(path);
    if (fd < 0) return -1;
    return fd;
}

/* --- Public API --- */
int gp_store_init(gp_store_t *store, const char *data_dir) {
    store->data_dir = strdup(data_dir);
    if (!store->data_dir) return -1;

    if (mkdir(data_dir, 0755) < 0 && errno != EEXIST) {
        free(store->data_dir);
        return -1;
    }

    /* Find highest existing segment */
    store->current_seg = 0;
    store->seg_offset  = 0;

    for (int i = 0; i < 100000; i++) {
        char *p = seg_path(data_dir, i);
        if (!p) break;
        struct stat st;
        int exists = (stat(p, &st) == 0);
        free(p);
        if (!exists) break;
        store->current_seg = i;
        store->seg_offset  = (uint64_t)st.st_size;
    }

    store->seg_fd = open_segment(store, store->current_seg, 1);
    if (store->seg_fd < 0) {
        free(store->data_dir);
        return -1;
    }

    /* Seek to end */
    if (lseek(store->seg_fd, (off_t)store->seg_offset, SEEK_SET) < 0) {
        close(store->seg_fd);
        free(store->data_dir);
        return -1;
    }

    return 0;
}

void gp_store_close(gp_store_t *store) {
    if (store->seg_fd >= 0) close(store->seg_fd);
    free(store->data_dir);
    store->seg_fd = -1;
}

int gp_store_append(gp_store_t *store, uint8_t type,
                    const uint8_t *payload, uint32_t length,
                    gp_offset_t *out_offset) {
    /* Roll to new segment if needed */
    if (store->seg_offset + GP_RECORD_OVERHEAD + length > GP_SEGMENT_SIZE) {
        close(store->seg_fd);
        store->current_seg++;
        store->seg_offset = 0;
        store->seg_fd = open_segment(store, store->current_seg, 1);
        if (store->seg_fd < 0) return -1;
    }

    if (out_offset) {
        out_offset->segment = (uint32_t)store->current_seg;
        out_offset->offset  = store->seg_offset;
    }

    /* Build header */
    uint8_t hdr[GP_RECORD_HEADER_SIZE];
    uint32_t magic_be = htonl(GP_MAGIC);
    uint32_t len_be   = htonl(length);
    memcpy(hdr, &magic_be, 4);
    hdr[4] = GP_VERSION;
    hdr[5] = type;
    memcpy(hdr + 6, &len_be, 4);

    /* CRC over header (sans crc field) + payload */
    uint32_t crc = gp_crc32(hdr, GP_RECORD_HEADER_SIZE);
    crc = 0xFFFFFFFFu;  /* re-compute properly */
    {
        uint8_t *full = malloc(GP_RECORD_HEADER_SIZE + length);
        if (!full) return -1;
        memcpy(full, hdr, GP_RECORD_HEADER_SIZE);
        memcpy(full + GP_RECORD_HEADER_SIZE, payload, length);
        crc = gp_crc32(full, GP_RECORD_HEADER_SIZE + length);
        free(full);
    }
    uint32_t crc_be = htonl(crc);

    /* Write header + payload + crc */
    if (write(store->seg_fd, hdr, GP_RECORD_HEADER_SIZE) != GP_RECORD_HEADER_SIZE) return -1;
    if (write(store->seg_fd, payload, length) != (ssize_t)length) return -1;
    if (write(store->seg_fd, &crc_be, 4) != 4) return -1;

    store->seg_offset += GP_RECORD_OVERHEAD + length;
    return 0;
}

int gp_store_read(gp_store_t *store, gp_offset_t offset,
                  uint8_t **payload, uint32_t *length, uint8_t *type) {
    char *path = seg_path(store->data_dir, (int)offset.segment);
    if (!path) return -1;

    int fd = open(path, O_RDONLY);
    free(path);
    if (fd < 0) return -1;

    if (lseek(fd, (off_t)offset.offset, SEEK_SET) < 0) {
        close(fd); return -1;
    }

    uint8_t hdr[GP_RECORD_HEADER_SIZE];
    if (read(fd, hdr, GP_RECORD_HEADER_SIZE) != GP_RECORD_HEADER_SIZE) {
        close(fd); return -1;
    }

    uint32_t magic;
    memcpy(&magic, hdr, 4);
    magic = ntohl(magic);
    if (magic != GP_MAGIC) { close(fd); return -1; }

    if (type) *type = hdr[5];

    uint32_t plen;
    memcpy(&plen, hdr + 6, 4);
    plen = ntohl(plen);

    uint8_t *buf = malloc(plen + 1);
    if (!buf) { close(fd); return -1; }

    if (read(fd, buf, plen) != (ssize_t)plen) {
        free(buf); close(fd); return -1;
    }
    buf[plen] = '\0';

    uint32_t stored_crc_be;
    if (read(fd, &stored_crc_be, 4) != 4) {
        free(buf); close(fd); return -1;
    }
    close(fd);

    uint32_t stored_crc = ntohl(stored_crc_be);

    /* Verify CRC */
    uint8_t *full = malloc(GP_RECORD_HEADER_SIZE + plen);
    if (!full) { free(buf); return -1; }
    memcpy(full, hdr, GP_RECORD_HEADER_SIZE);
    memcpy(full + GP_RECORD_HEADER_SIZE, buf, plen);
    uint32_t computed_crc = gp_crc32(full, GP_RECORD_HEADER_SIZE + plen);
    free(full);

    if (computed_crc != stored_crc) {
        free(buf); return -1;  /* CRC mismatch */
    }

    *payload = buf;
    if (length) *length = plen;
    return 0;
}
