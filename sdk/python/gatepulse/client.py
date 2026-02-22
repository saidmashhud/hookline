"""GatePulse Python SDK."""

from __future__ import annotations

import hashlib
import hmac
import json
from typing import Any

try:
    import urllib.request as _urllib_request
    import urllib.error as _urllib_error
except ImportError:
    raise


class GatePulseError(Exception):
    def __init__(self, status: int, body: Any):
        self.status = status
        self.body = body
        super().__init__(f"GatePulse API error {status}: {body}")


class GatePulseClient:
    def __init__(self, base_url: str, api_key: str) -> None:
        self.base_url = base_url.rstrip("/")
        self.api_key = api_key

    def _request(
        self, method: str, path: str, body: Any = None
    ) -> Any:
        url = self.base_url + path
        data = json.dumps(body).encode() if body is not None else None
        req = _urllib_request.Request(
            url,
            data=data,
            method=method,
            headers={
                "Authorization": f"Bearer {self.api_key}",
                "Content-Type": "application/json",
            },
        )
        try:
            with _urllib_request.urlopen(req) as resp:
                return json.loads(resp.read())
        except _urllib_error.HTTPError as e:
            raise GatePulseError(e.code, json.loads(e.read())) from e

    def publish_event(
        self,
        topic: str,
        data: dict,
        idempotency_key: str | None = None,
        occurred_at: int | None = None,
    ) -> dict:
        payload: dict = {"topic": topic, "data": data}
        if idempotency_key:
            payload["idempotency_key"] = idempotency_key
        if occurred_at:
            payload["occurred_at"] = occurred_at
        return self._request("POST", "/v1/events", payload)

    def list_events(
        self,
        limit: int = 50,
        cursor: str | None = None,
        from_ms: int | None = None,
        to_ms: int | None = None,
    ) -> dict:
        qs = f"?limit={limit}"
        if cursor:
            qs += f"&cursor={cursor}"
        if from_ms:
            qs += f"&from_ms={from_ms}"
        if to_ms:
            qs += f"&to_ms={to_ms}"
        return self._request("GET", f"/v1/events{qs}")

    def create_endpoint(
        self, url: str, name: str | None = None, secret: str | None = None
    ) -> dict:
        payload: dict = {"url": url}
        if name:
            payload["name"] = name
        if secret:
            payload["secret"] = secret
        return self._request("POST", "/v1/endpoints", payload)

    def list_endpoints(self) -> dict:
        return self._request("GET", "/v1/endpoints")

    def delete_endpoint(self, endpoint_id: str) -> None:
        self._request("DELETE", f"/v1/endpoints/{endpoint_id}")

    def create_subscription(
        self,
        endpoint_id: str,
        topic_pattern: str = "#",
        filter: dict | None = None,
        transform: list | None = None,
    ) -> dict:
        payload: dict = {
            "endpoint_id": endpoint_id,
            "topic_pattern": topic_pattern,
        }
        if filter:
            payload["filter"] = filter
        if transform:
            payload["transform"] = transform
        return self._request("POST", "/v1/subscriptions", payload)

    def list_subscriptions(self) -> dict:
        return self._request("GET", "/v1/subscriptions")

    def delete_subscription(self, subscription_id: str) -> None:
        self._request("DELETE", f"/v1/subscriptions/{subscription_id}")

    def list_dlq(self) -> dict:
        return self._request("GET", "/v1/dlq")

    def requeue_dlq_job(self, job_id: str) -> dict:
        return self._request("POST", f"/v1/dlq/{job_id}/requeue")

    @staticmethod
    def verify_signature(payload: bytes | str, signature: str, secret: str) -> bool:
        """Verify an incoming webhook signature (HMAC-SHA256)."""
        if isinstance(payload, str):
            payload = payload.encode()
        expected = hmac.new(
            secret.encode(), payload, hashlib.sha256
        ).hexdigest()
        sig = signature.removeprefix("sha256=")
        return hmac.compare_digest(expected, sig)
