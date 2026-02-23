package io.hookline.bridge

import zio.*
import zio.kafka.consumer.*
import zio.kafka.serde.Serde
import zio.logging.*
import org.apache.kafka.clients.consumer.ConsumerConfig

/** Core bridge loop.
 *
 *  Flow per Kafka record:
 *    1. Map record → HooklinePayload (EventMapper)
 *    2. Publish to HookLine with exponential retry (max retryMax attempts)
 *    3. Commit Kafka offset only after successful publish
 *
 *  At-least-once guarantee:
 *    - Kafka offset committed only after HookLine ACK
 *    - HookLine deduplicates via event_id (= "kafka-{topic}-{partition}-{offset}")
 *    - Crash between publish and commit → record redelivered → HookLine deduplicates
 *
 *  Ordering:
 *    - mapZIO (sequential) preserves intra-batch record order.
 *    - This is critical for event streams where aggregate ordering matters
 *      (e.g. payment.created must arrive before payment.authorized).
 *    - ZIO Kafka provides partition-level ordering; mapZIO preserves it end-to-end.
 */
object BridgeApp:

  def start: ZIO[HooklineClient & BridgeConfig & Scope, Throwable, Unit] =
    for
      cfg    <- ZIO.service[BridgeConfig]
      client <- ZIO.service[HooklineClient]
      _      <- ZIO.logInfo(s"Starting HookLine bridge → ${cfg.hookline.url}")
      _      <- ZIO.logInfo(s"Kafka: ${cfg.kafka.bootstrapServers} | topics: ${cfg.kafka.topics}")
      _      <- consume(cfg, client)
    yield ()

  private def consume(cfg: BridgeConfig, client: HooklineClient): ZIO[Scope, Throwable, Unit] =
    val consumerSettings = ConsumerSettings(cfg.kafka.bootstrapServers.split(",").toList)
      .withGroupId(cfg.kafka.groupId)
      .withProperty(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, cfg.kafka.autoOffsetReset)
      .withProperty(ConsumerConfig.ENABLE_AUTO_COMMIT_CONFIG, "false")

    val retrySchedule: Schedule[Any, Throwable, ?] =
      Schedule.exponential(cfg.retryBaseMs.milliseconds) &&
      Schedule.recurs(cfg.retryMax.toLong)

    Consumer.make(consumerSettings).flatMap { consumer =>
      consumer
        .plainStream(
          cfg.kafka.topicList match {
            case h :: t => Subscription.topics(h, t*)
            case Nil    => Subscription.topics("events")
          },
          Serde.byteArray,
          Serde.byteArray
        )
        .mapZIO { record =>
          val mapped: Either[String, HooklinePayload] = cfg.bridgeMode match
            case "mashgate" => MashgateEventMapper.map(record.record)
            case _          => Right(EventMapper.map(record, cfg.kafka))
          ZIO.fromEither(mapped)
            .mapError(e => new RuntimeException(e))
            .flatMap { event =>
              client
                .publish(event)
                .retry(retrySchedule)
                .tapError(e => ZIO.logError(
                  s"Failed to publish event ${event.eventId} after ${cfg.retryMax} retries: ${e.getMessage}"
                ))
            }
            .as(record.offset)
        }
        .aggregateAsync(Consumer.offsetBatches)
        .mapZIO(batch => batch.commit *> ZIO.logDebug(s"Committed batch of ${batch.offsets.size} offsets"))
        .runDrain
    }
