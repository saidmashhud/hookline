CC      = gcc
CFLAGS  = -std=c11 -Wall -Wextra -Wpedantic -O2 -D_GNU_SOURCE \
           -I./src
LDFLAGS = -lm

SRC_DIR   = src
TEST_DIR  = tests
BUILD_DIR = build

SRCS = $(SRC_DIR)/main.c \
       $(SRC_DIR)/store.c \
       $(SRC_DIR)/index.c \
       $(SRC_DIR)/queue.c \
       $(SRC_DIR)/ipc.c \
       $(SRC_DIR)/dlq.c

TEST_SRCS = $(TEST_DIR)/test_store.c \
            $(SRC_DIR)/store.c \
            $(SRC_DIR)/index.c \
            $(SRC_DIR)/queue.c \
            $(SRC_DIR)/ipc.c \
            $(SRC_DIR)/dlq.c

DAEMON  = $(BUILD_DIR)/gp_store
TESTS   = $(BUILD_DIR)/gp_store_tests

.PHONY: all clean test daemon

all: $(BUILD_DIR) daemon

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

daemon: $(BUILD_DIR) $(SRCS)
	$(CC) $(CFLAGS) -o $(DAEMON) $(SRCS) $(LDFLAGS)

test: $(BUILD_DIR) $(TEST_SRCS)
	$(CC) $(CFLAGS) -DUNIT_TEST -o $(TESTS) $(TEST_SRCS) $(LDFLAGS)
	$(TESTS)

clean:
	rm -rf $(BUILD_DIR)

install: daemon
	install -m 755 $(DAEMON) /usr/local/bin/gp_store
