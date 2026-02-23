package io.hookline.bridge

import zio.json.*
import zio.json.ast.Json
import java.nio.charset.StandardCharsets

/** Maps a Kafka record carrying a Mashgate DomainEvent to a HooklinePayload.
 *
 *  Expects the Kafka value to be a JSON object with at least:
 *    - "event_id"   : UUID string  → used as HookLine event_id (idempotency key)
 *    - "event_type" : PascalCase   → converted to dot.notation topic
 *                     e.g. "PaymentCaptured" → "payment.captured"
 *
 *  If "event_id" is absent a random UUID is generated.
 *  Returns Left(reason) when the value is not a JSON object.
 */
object MashgateEventMapper:

  /** Converts a PascalCase type name to a dot-separated topic string.
   *  "PaymentCaptured"  → "payment.captured"
   *  "BookingConfirmed" → "booking.confirmed"
   *  "ACHTransfer"      → "a.c.h.transfer"  (each uppercase run split)
   */
  def toTopic(eventType: String): String =
    eventType
      .split("(?<=[a-z])(?=[A-Z])|(?<=[A-Z])(?=[A-Z][a-z])")
      .map(_.toLowerCase)
      .mkString(".")

  def map(
    record: org.apache.kafka.clients.consumer.ConsumerRecord[Array[Byte], Array[Byte]]
  ): Either[String, HooklinePayload] =
    val valueStr = Option(record.value())
      .map(new String(_, StandardCharsets.UTF_8))
      .getOrElse("")

    valueStr.fromJson[Json] match
      case Left(_) =>
        Left(s"invalid JSON in record offset=${record.offset()}")
      case Right(json) =>
        json match
          case obj: Json.Obj =>
            val fields = obj.fields.toMap
            val eventId = fields.get("event_id").collect { case Json.Str(v) => v }
              .getOrElse(java.util.UUID.randomUUID().toString)
            val eventType = fields.get("event_type").collect { case Json.Str(v) => v }
              .getOrElse("")
            val topic = toTopic(eventType)
            Right(HooklinePayload(topic = topic, eventId = eventId, payload = json))
          case _ =>
            Left(s"expected JSON object at offset=${record.offset()}")
