#!/usr/bin/env python3
"""GatePulse Python SDK — basic example.

Run with:
    GATEPULSE_URL=http://localhost:8080 GATEPULSE_API_KEY=dev-secret python basic.py
"""

import os
import urllib.request
import json

from gatepulse import GatePulseClient

BASE_URL = os.environ.get("GATEPULSE_URL", "http://localhost:8080")
API_KEY = os.environ.get("GATEPULSE_API_KEY", "dev-secret")

client = GatePulseClient(BASE_URL, API_KEY)

# 1. Create a dev inbox
req = urllib.request.Request(
    f"{BASE_URL}/v1/dev/inbox",
    data=b"{}",
    method="POST",
    headers={"Authorization": f"Bearer {API_KEY}", "Content-Type": "application/json"},
)
with urllib.request.urlopen(req) as resp:
    inbox = json.loads(resp.read())
receive_url = BASE_URL + inbox["receive_url"]
print(f"Dev inbox URL: {receive_url}")

# 2. Create endpoint pointing at inbox
endpoint = client.create_endpoint(url=receive_url, name="SDK test endpoint")
print(f"Created endpoint: {endpoint['endpoint_id']}")

# 3. Subscribe to all events
sub = client.create_subscription(endpoint_id=endpoint["endpoint_id"], topic_pattern="#")
print(f"Created subscription: {sub['subscription_id']}")

# 4. Publish an event
event = client.publish_event(
    topic="orders.created",
    data={"order_id": "ord_123", "amount": 99.99, "currency": "USD"},
)
print(f"Published event: {event['id']}")

# 5. List events
events = client.list_events(limit=5)
print(f"Listed {events['count']} event(s)")
