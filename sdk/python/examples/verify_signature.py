#!/usr/bin/env python3
"""Verify a GatePulse webhook signature.

In your web framework's route handler, call verify_signature before processing
the event to ensure the request originated from GatePulse.
"""

from gatepulse import GatePulseClient

# Example values (replace with real ones from your environment)
raw_body = b'{"topic":"orders.created","data":{"order_id":"ord_123"}}'
signature_header = "sha256=abc123..."  # value of x-gp-signature request header
endpoint_secret = "my-endpoint-secret"

is_valid = GatePulseClient.verify_signature(raw_body, signature_header, endpoint_secret)
if is_valid:
    print("Signature is valid — process the event")
else:
    print("Invalid signature — reject the request (return 401)")
