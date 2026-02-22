#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>
#include <sys/stat.h>

#include "../src/store.h"
#include "../src/index.h"
#include "../src/queue.h"
#include "../src/dlq.h"

static int tests_run  = 0;
static int tests_pass = 0;
static int tests_fail = 0;

#define TEST(name) do { \
    printf("  [TEST] %s ... ", #name); \
    tests_run++; \
    int _rc = test_##name(); \
    if (_rc == 0) { printf("PASS\n"); tests_pass++; } \
    else          { printf("FAIL\n"); tests_fail++; } \
} while(0)

#define ASSERT(expr) do { if (!(expr)) { \
    fprintf(stderr, "\n    Assertion failed: %s at %s:%d\n", #expr, __FILE__, __LINE__); \
    return -1; } } while(0)

/* --- CRC32 --- */
static int test_crc32_basic(void) {
    const uint8_t data[] = "hello world";
    uint32_t crc = gp_crc32(data, 11);
    /* known value for "hello world" */
    ASSERT(crc == 0x0D4A1185u);
    return 0;
}

static int test_crc32_empty(void) {
    uint32_t crc = gp_crc32(NULL, 0);
    ASSERT(crc == (0xFFFFFFFFu ^ 0xFFFFFFFFu));  /* 0 */
    return 0;
}

/* --- Store --- */
static const char *TEST_DIR = "/tmp/gp_test_store";

static void rmdir_r(const char *dir) {
    char cmd[512];
    snprintf(cmd, sizeof(cmd), "rm -rf %s", dir);
    (void)system(cmd);
}

static int test_store_append_read(void) {
    rmdir_r(TEST_DIR);
    gp_store_t store;
    ASSERT(gp_store_init(&store, TEST_DIR) == 0);

    const char *payload = "{\"id\":\"evt1\",\"topic\":\"orders.created\"}";
    gp_offset_t off;
    ASSERT(gp_store_append(&store, GP_TYPE_EVENT,
                           (uint8_t *)payload, (uint32_t)strlen(payload), &off) == 0);
    ASSERT(off.segment == 0);

    uint8_t *out = NULL; uint32_t olen; uint8_t type;
    ASSERT(gp_store_read(&store, off, &out, &olen, &type) == 0);
    ASSERT(type == GP_TYPE_EVENT);
    ASSERT(olen == (uint32_t)strlen(payload));
    ASSERT(memcmp(out, payload, olen) == 0);
    free(out);

    gp_store_close(&store);
    rmdir_r(TEST_DIR);
    return 0;
}

static int test_store_multiple_records(void) {
    rmdir_r(TEST_DIR);
    gp_store_t store;
    ASSERT(gp_store_init(&store, TEST_DIR) == 0);

    gp_offset_t offsets[10];
    for (int i = 0; i < 10; i++) {
        char payload[64];
        snprintf(payload, sizeof(payload), "{\"seq\":%d}", i);
        ASSERT(gp_store_append(&store, GP_TYPE_EVENT,
               (uint8_t *)payload, (uint32_t)strlen(payload), &offsets[i]) == 0);
    }

    for (int i = 0; i < 10; i++) {
        uint8_t *out = NULL; uint32_t olen; uint8_t type;
        ASSERT(gp_store_read(&store, offsets[i], &out, &olen, &type) == 0);
        char expected[64];
        snprintf(expected, sizeof(expected), "{\"seq\":%d}", i);
        ASSERT(strcmp((char *)out, expected) == 0);
        free(out);
    }

    gp_store_close(&store);
    rmdir_r(TEST_DIR);
    return 0;
}

static int test_store_crash_recovery(void) {
    rmdir_r(TEST_DIR);
    gp_store_t store;
    ASSERT(gp_store_init(&store, TEST_DIR) == 0);

    const char *payload = "{\"id\":\"evt-crash\"}";
    gp_offset_t off1;
    ASSERT(gp_store_append(&store, GP_TYPE_EVENT,
           (uint8_t *)payload, (uint32_t)strlen(payload), &off1) == 0);
    gp_store_close(&store);

    /* Re-open (simulates crash recovery) */
    ASSERT(gp_store_init(&store, TEST_DIR) == 0);
    /* Append another */
    const char *payload2 = "{\"id\":\"evt-after-recovery\"}";
    gp_offset_t off2;
    ASSERT(gp_store_append(&store, GP_TYPE_EVENT,
           (uint8_t *)payload2, (uint32_t)strlen(payload2), &off2) == 0);
    ASSERT(off2.offset > off1.offset);

    /* Read both */
    uint8_t *out; uint32_t olen; uint8_t type;
    ASSERT(gp_store_read(&store, off1, &out, &olen, &type) == 0);
    ASSERT(strcmp((char *)out, payload) == 0);
    free(out);

    ASSERT(gp_store_read(&store, off2, &out, &olen, &type) == 0);
    ASSERT(strcmp((char *)out, payload2) == 0);
    free(out);

    gp_store_close(&store);
    rmdir_r(TEST_DIR);
    return 0;
}

/* --- Index --- */
static int test_index_put_get(void) {
    gp_index_t idx;
    gp_index_init(&idx);

    gp_offset_t off = {0, 1234};
    ASSERT(gp_index_put(&idx, "evt-abc", off) == 0);

    gp_offset_t got;
    ASSERT(gp_index_get(&idx, "evt-abc", &got) == 0);
    ASSERT(got.segment == 0 && got.offset == 1234);

    ASSERT(gp_index_get(&idx, "nonexistent", &got) == -1);

    gp_index_free(&idx);
    return 0;
}

static int test_index_update(void) {
    gp_index_t idx;
    gp_index_init(&idx);

    gp_offset_t off1 = {0, 100};
    gp_offset_t off2 = {0, 200};
    gp_index_put(&idx, "key1", off1);
    gp_index_put(&idx, "key1", off2);  /* update */

    gp_offset_t got;
    ASSERT(gp_index_get(&idx, "key1", &got) == 0);
    ASSERT(got.offset == 200);

    gp_index_free(&idx);
    return 0;
}

static int test_index_delete(void) {
    gp_index_t idx;
    gp_index_init(&idx);

    gp_offset_t off = {0, 50};
    gp_index_put(&idx, "del-key", off);
    ASSERT(gp_index_delete(&idx, "del-key") == 0);
    ASSERT(gp_index_delete(&idx, "del-key") == -1);

    gp_offset_t got;
    ASSERT(gp_index_get(&idx, "del-key", &got) == -1);

    gp_index_free(&idx);
    return 0;
}

/* --- Queue --- */
static int test_queue_enqueue_claim(void) {
    gp_queue_t q;
    gp_queue_init(&q);

    gp_job_t job = {0};
    strncpy(job.job_id, "job-001", sizeof(job.job_id));
    strncpy(job.event_id, "evt-001", sizeof(job.event_id));
    job.max_attempts = 5;
    job.next_attempt_at = 0;

    ASSERT(gp_queue_enqueue(&q, &job) == 0);
    ASSERT(q.count == 1);

    gp_job_t *claimed[10];
    int n = gp_queue_claim(&q, claimed, 10, 30);
    ASSERT(n == 1);
    ASSERT(strcmp(claimed[0]->job_id, "job-001") == 0);
    ASSERT(claimed[0]->state == GP_JOB_STATE_CLAIMED);
    ASSERT(claimed[0]->attempt_count == 1);

    gp_queue_free(&q);
    return 0;
}

static int test_queue_ack(void) {
    gp_queue_t q;
    gp_queue_init(&q);

    gp_job_t job = {0};
    strncpy(job.job_id, "job-ack", sizeof(job.job_id));
    job.max_attempts = 3;
    gp_queue_enqueue(&q, &job);

    gp_job_t *claimed[1];
    gp_queue_claim(&q, claimed, 1, 30);
    ASSERT(gp_queue_ack(&q, "job-ack") == 0);

    /* queue count decremented */
    ASSERT(q.count == 0);

    gp_queue_free(&q);
    return 0;
}

static int test_queue_nack_retry(void) {
    gp_queue_t q;
    gp_queue_init(&q);

    gp_job_t job = {0};
    strncpy(job.job_id, "job-nack", sizeof(job.job_id));
    job.max_attempts = 3;
    gp_queue_enqueue(&q, &job);

    gp_job_t *claimed[1];
    gp_queue_claim(&q, claimed, 1, 30);
    ASSERT(gp_queue_nack(&q, "job-nack", 0) == 0);

    /* Still pending, attempt_count still 1 (nack doesn't decrement) */
    claimed[0]->next_attempt_at = 0;  /* make claimable again */
    int n = gp_queue_claim(&q, claimed, 1, 30);
    ASSERT(n == 1);
    ASSERT(claimed[0]->attempt_count == 2);

    gp_queue_free(&q);
    return 0;
}

static int test_queue_nack_dlq(void) {
    gp_queue_t q;
    gp_queue_init(&q);

    gp_job_t job = {0};
    strncpy(job.job_id, "job-dlq", sizeof(job.job_id));
    job.max_attempts = 1;
    gp_queue_enqueue(&q, &job);

    gp_job_t *claimed[1];
    gp_queue_claim(&q, claimed, 1, 30);
    ASSERT(gp_queue_nack(&q, "job-dlq", 0) == 0);

    /* Should be in DLQ state now */
    ASSERT(claimed[0]->state == GP_JOB_STATE_DLQ);

    gp_queue_free(&q);
    return 0;
}

static int test_queue_lease_reap(void) {
    gp_queue_t q;
    gp_queue_init(&q);

    gp_job_t job = {0};
    strncpy(job.job_id, "job-lease", sizeof(job.job_id));
    job.max_attempts = 5;
    gp_queue_enqueue(&q, &job);

    gp_job_t *claimed[1];
    gp_queue_claim(&q, claimed, 1, 30);
    /* Expire the lease */
    claimed[0]->lease_expires_at = 0;

    int reaped = gp_queue_reap_leases(&q);
    ASSERT(reaped == 1);
    ASSERT(claimed[0]->state == GP_JOB_STATE_PENDING);

    gp_queue_free(&q);
    return 0;
}

/* --- DLQ --- */
static int test_dlq_put_list_delete(void) {
    gp_dlq_t dlq;
    gp_dlq_init(&dlq);

    gp_dlq_entry_t e = {0};
    strncpy(e.job_id,      "job-dlq1",    sizeof(e.job_id));
    strncpy(e.event_id,    "evt-1",       sizeof(e.event_id));
    strncpy(e.endpoint_id, "ep-1",        sizeof(e.endpoint_id));
    strncpy(e.tenant_id,   "tenant-a",    sizeof(e.tenant_id));
    strncpy(e.reason,      "http 500",    sizeof(e.reason));
    e.attempt_count = 5;

    ASSERT(gp_dlq_put(&dlq, &e) == 0);
    ASSERT(dlq.count == 1);

    gp_dlq_entry_t *out = NULL; int count = 0;
    ASSERT(gp_dlq_list(&dlq, "tenant-a", &out, &count) == 0);
    ASSERT(count == 1);
    ASSERT(strcmp(out[0].reason, "http 500") == 0);
    free(out);

    ASSERT(gp_dlq_delete(&dlq, "job-dlq1") == 0);
    ASSERT(dlq.count == 0);

    gp_dlq_free(&dlq);
    return 0;
}

/* --- main --- */
int main(void) {
    printf("=== GatePulse C Store Unit Tests ===\n\n");

    printf("CRC32:\n");
    TEST(crc32_basic);
    TEST(crc32_empty);

    printf("\nStore:\n");
    TEST(store_append_read);
    TEST(store_multiple_records);
    TEST(store_crash_recovery);

    printf("\nIndex:\n");
    TEST(index_put_get);
    TEST(index_update);
    TEST(index_delete);

    printf("\nQueue:\n");
    TEST(queue_enqueue_claim);
    TEST(queue_ack);
    TEST(queue_nack_retry);
    TEST(queue_nack_dlq);
    TEST(queue_lease_reap);

    printf("\nDLQ:\n");
    TEST(dlq_put_list_delete);

    printf("\n=== Results: %d/%d passed", tests_pass, tests_run);
    if (tests_fail > 0) printf(" (%d FAILED)", tests_fail);
    printf(" ===\n");
    return tests_fail > 0 ? 1 : 0;
}
