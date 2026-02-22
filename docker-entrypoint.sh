#!/bin/sh
set -e

SOCKET="${GP_STORE_SOCKET:-/run/gatepulse/gp_store.sock}"
DATA_DIR="${GP_DATA_DIR:-/var/lib/gatepulse}"

echo "Starting gp_store daemon..."
gp_store "${SOCKET}" "${DATA_DIR}" &
GP_STORE_PID=$!

# Wait for socket to appear
for i in $(seq 1 30); do
    if [ -S "${SOCKET}" ]; then
        echo "gp_store ready."
        break
    fi
    sleep 0.2
done

if [ ! -S "${SOCKET}" ]; then
    echo "ERROR: gp_store did not start in time."
    exit 1
fi

echo "Starting GatePulse Erlang node..."
exec /opt/gatepulse/bin/gatepulse foreground
