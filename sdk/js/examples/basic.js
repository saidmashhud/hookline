// GatePulse JS SDK — basic example
// Run with: node basic.js
// Requires GatePulse running at http://localhost:8080

import { GatePulseClient } from "@gatepulse/sdk";

const client = new GatePulseClient({
  baseUrl: process.env.GATEPULSE_URL || "http://localhost:8080",
  apiKey: process.env.GATEPULSE_API_KEY || "dev-secret",
});

async function main() {
  // 1. Create an endpoint (use the dev inbox as the target)
  const inboxRes = await fetch(
    `${process.env.GATEPULSE_URL || "http://localhost:8080"}/v1/dev/inbox`,
    {
      method: "POST",
      headers: {
        Authorization: `Bearer ${process.env.GATEPULSE_API_KEY || "dev-secret"}`,
        "Content-Type": "application/json",
      },
      body: "{}",
    }
  );
  const inbox = await inboxRes.json();
  const receiveUrl = `${process.env.GATEPULSE_URL || "http://localhost:8080"}${inbox.receive_url}`;
  console.log("Dev inbox URL:", receiveUrl);

  const endpoint = await client.createEndpoint({
    url: receiveUrl,
    name: "SDK test endpoint",
  });
  console.log("Created endpoint:", endpoint.endpoint_id);

  // 2. Subscribe to all events
  const sub = await client.createSubscription({
    endpointId: endpoint.endpoint_id,
    topicPattern: "#",
  });
  console.log("Created subscription:", sub.subscription_id);

  // 3. Publish an event
  const event = await client.publishEvent({
    topic: "orders.created",
    data: { order_id: "ord_123", amount: 99.99, currency: "USD" },
  });
  console.log("Published event:", event.id);

  // 4. List events
  const events = await client.listEvents({ limit: 5 });
  console.log(`Listed ${events.count} events`);
}

main().catch(console.error);
