/**
 * GatePulse JavaScript/TypeScript SDK
 */

export interface GatePulseConfig {
  baseUrl: string;
  apiKey: string;
}

export interface PublishEventOptions {
  topic: string;
  data: Record<string, unknown>;
  idempotencyKey?: string;
  occurredAt?: number;
}

export interface CreateEndpointOptions {
  url: string;
  name?: string;
  secret?: string;
}

export interface CreateSubscriptionOptions {
  endpointId: string;
  topicPattern?: string;
  filter?: Record<string, unknown>;
  transform?: Array<Record<string, unknown>>;
}

export class GatePulseClient {
  private baseUrl: string;
  private apiKey: string;

  constructor(config: GatePulseConfig) {
    this.baseUrl = config.baseUrl.replace(/\/$/, "");
    this.apiKey = config.apiKey;
  }

  private async request<T>(
    method: string,
    path: string,
    body?: unknown
  ): Promise<T> {
    const res = await fetch(`${this.baseUrl}${path}`, {
      method,
      headers: {
        Authorization: `Bearer ${this.apiKey}`,
        "Content-Type": "application/json",
      },
      body: body !== undefined ? JSON.stringify(body) : undefined,
    });
    if (!res.ok) {
      const err = await res.json().catch(() => ({ error: res.statusText }));
      throw new Error(
        `GatePulse API error ${res.status}: ${JSON.stringify(err)}`
      );
    }
    return res.json() as Promise<T>;
  }

  async publishEvent(opts: PublishEventOptions): Promise<Record<string, unknown>> {
    const payload: Record<string, unknown> = {
      topic: opts.topic,
      data: opts.data,
    };
    if (opts.idempotencyKey) payload.idempotency_key = opts.idempotencyKey;
    if (opts.occurredAt) payload.occurred_at = opts.occurredAt;
    return this.request("POST", "/v1/events", payload);
  }

  async listEvents(params?: {
    limit?: number;
    cursor?: string;
    fromMs?: number;
    toMs?: number;
  }): Promise<{ items: unknown[]; count: number; next_cursor?: string }> {
    const qs = new URLSearchParams();
    if (params?.limit) qs.set("limit", String(params.limit));
    if (params?.cursor) qs.set("cursor", params.cursor);
    if (params?.fromMs) qs.set("from_ms", String(params.fromMs));
    if (params?.toMs) qs.set("to_ms", String(params.toMs));
    return this.request("GET", `/v1/events?${qs}`);
  }

  async createEndpoint(
    opts: CreateEndpointOptions
  ): Promise<Record<string, unknown>> {
    return this.request("POST", "/v1/endpoints", opts);
  }

  async listEndpoints(): Promise<{ items: unknown[] }> {
    return this.request("GET", "/v1/endpoints");
  }

  async deleteEndpoint(endpointId: string): Promise<void> {
    await this.request("DELETE", `/v1/endpoints/${endpointId}`);
  }

  async createSubscription(
    opts: CreateSubscriptionOptions
  ): Promise<Record<string, unknown>> {
    return this.request("POST", "/v1/subscriptions", {
      endpoint_id: opts.endpointId,
      topic_pattern: opts.topicPattern ?? "#",
      filter: opts.filter,
      transform: opts.transform,
    });
  }

  async listSubscriptions(): Promise<{ items: unknown[] }> {
    return this.request("GET", "/v1/subscriptions");
  }

  async deleteSubscription(subscriptionId: string): Promise<void> {
    await this.request("DELETE", `/v1/subscriptions/${subscriptionId}`);
  }

  async listDlq(): Promise<{ items: unknown[] }> {
    return this.request("GET", "/v1/dlq");
  }

  async requeueDlqJob(jobId: string): Promise<void> {
    await this.request("POST", `/v1/dlq/${jobId}/requeue`);
  }

  /**
   * Verify a webhook signature from GatePulse.
   * @param payload - Raw request body bytes
   * @param signature - Value of x-gp-signature header
   * @param secret - Endpoint secret
   */
  static async verifySignature(
    payload: string | Uint8Array,
    signature: string,
    secret: string
  ): Promise<boolean> {
    const enc = new TextEncoder();
    const key = await crypto.subtle.importKey(
      "raw",
      enc.encode(secret),
      { name: "HMAC", hash: "SHA-256" },
      false,
      ["verify"]
    );
    const body = typeof payload === "string" ? enc.encode(payload) : payload;
    const sigHex = signature.replace(/^sha256=/, "");
    const sigBytes = Uint8Array.from(
      sigHex.match(/.{2}/g)!.map((b) => parseInt(b, 16))
    );
    return crypto.subtle.verify("HMAC", key, sigBytes, body);
  }

  /**
   * Subscribe to live events via SSE.
   * Returns an EventSource; caller is responsible for closing it.
   */
  subscribeStream(topic = "#"): EventSource {
    const url = `${this.baseUrl}/v1/stream?topic=${encodeURIComponent(topic)}`;
    return new EventSource(url);
  }
}
