#!/usr/bin/env python3
"""Verify a HookLine webhook signature.

In your web framework's route handler, call verify_signature before processing
the event to ensure the request originated from HookLine.
"""

from hookline import HookLineClient

# Example values (replace with real ones from your environment)
raw_body = b'{"topic":"orders.created","data":{"order_id":"ord_123"}}'
timestamp_header = "1710000000000"  # value of x-gp-timestamp request header
signature_header = "v1=abc123..."  # value of x-gp-signature request header
endpoint_secret = "my-endpoint-secret"

is_valid = HookLineClient.verify_signature(
    raw_body, timestamp_header, signature_header, endpoint_secret
)
if is_valid:
    print("Signature is valid — process the event")
else:
    print("Invalid signature — reject the request (return 401)")
