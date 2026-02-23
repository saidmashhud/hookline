package io.hookline.bridge

import zio.*

final case class KafkaConfig(
  bootstrapServers: String,
  groupId: String,
  topics: String,       // comma-separated
  topicPrefix: String,
  autoOffsetReset: String
) {
  def topicList: List[String] = topics.split(",").map(_.trim).filter(_.nonEmpty).toList

  def stripPrefix(kafkaTopic: String): String =
    if topicPrefix.nonEmpty && kafkaTopic.startsWith(topicPrefix)
    then kafkaTopic.drop(topicPrefix.length)
    else kafkaTopic
}

final case class HooklineConfig(
  url: String,
  apiKey: String
)

final case class BridgeConfig(
  kafka: KafkaConfig,
  hookline: HooklineConfig,
  parallelism: Int,
  retryMax: Int,
  retryBaseMs: Int,
  bridgeMode: String  // "generic" | "mashgate"
)

object BridgeConfig:

  val layer: ZLayer[Any, Nothing, BridgeConfig] =
    ZLayer.fromZIO(load)

  private def env(key: String, default: String): UIO[String] =
    ZIO.succeed(sys.env.getOrElse(key, default))

  private def envInt(key: String, default: Int): UIO[Int] =
    ZIO.succeed(sys.env.get(key).flatMap(_.toIntOption).getOrElse(default))

  private val load: UIO[BridgeConfig] =
    for
      bootstrapServers <- env("KAFKA_BOOTSTRAP_SERVERS", "localhost:9092")
      groupId          <- env("KAFKA_GROUP_ID",          "hookline-bridge")
      topics           <- env("KAFKA_TOPICS",            "events")
      topicPrefix      <- env("KAFKA_TOPIC_PREFIX",      "")
      autoOffsetReset  <- env("KAFKA_AUTO_OFFSET_RESET", "latest")
      hooklineUrl      <- env("HOOKLINE_URL",            "http://localhost:8080")
      hooklineApiKey   <- env("HOOKLINE_API_KEY",        "dev-secret")
      parallelism      <- envInt("BRIDGE_PARALLELISM",   4)
      retryMax         <- envInt("BRIDGE_RETRY_MAX",     5)
      retryBaseMs      <- envInt("BRIDGE_RETRY_BASE_MS", 200)
      bridgeMode       <- env("BRIDGE_MODE",             "generic")
    yield BridgeConfig(
      kafka    = KafkaConfig(bootstrapServers, groupId, topics, topicPrefix, autoOffsetReset),
      hookline = HooklineConfig(hooklineUrl, hooklineApiKey),
      parallelism = parallelism,
      retryMax    = retryMax,
      retryBaseMs = retryBaseMs,
      bridgeMode  = bridgeMode
    )
