#include "ipc.h"
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <arpa/inet.h>

static int read_exactly(int fd, uint8_t *buf, uint32_t n) {
    uint32_t done = 0;
    while (done < n) {
        ssize_t r = read(fd, buf + done, n - done);
        if (r <= 0) return -1;
        done += (uint32_t)r;
    }
    return 0;
}

static int write_exactly(int fd, const uint8_t *buf, uint32_t n) {
    uint32_t done = 0;
    while (done < n) {
        ssize_t w = write(fd, buf + done, n - done);
        if (w <= 0) return -1;
        done += (uint32_t)w;
    }
    return 0;
}

int gp_ipc_recv(int fd, uint8_t **buf, uint32_t *len) {
    uint8_t len_buf[4];
    if (read_exactly(fd, len_buf, 4) < 0) return -1;
    uint32_t msg_len = ntohl(*(uint32_t *)len_buf);
    if (msg_len == 0 || msg_len > GP_IPC_MAX_MSG) return -1;

    uint8_t *data = malloc(msg_len + 1);
    if (!data) return -1;
    if (read_exactly(fd, data, msg_len) < 0) {
        free(data); return -1;
    }
    data[msg_len] = '\0';
    *buf = data;
    *len = msg_len;
    return 0;
}

int gp_ipc_send(int fd, const uint8_t *buf, uint32_t len) {
    uint32_t len_be = htonl(len);
    if (write_exactly(fd, (uint8_t *)&len_be, 4) < 0) return -1;
    if (write_exactly(fd, buf, len) < 0) return -1;
    return 0;
}

int gp_ipc_send_str(int fd, const char *str) {
    return gp_ipc_send(fd, (const uint8_t *)str, (uint32_t)strlen(str));
}
