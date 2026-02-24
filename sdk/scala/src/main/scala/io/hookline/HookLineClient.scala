package io.hookline

import io.hookline.HookLineError.*
import io.hookline.models.*
import zio.*
import zio.http.*
import zio.http.Header.Authorization
import zio.json.*
import zio.json.ast.Json

/** HookLine API client.
 *
 *  All methods return `IO[HookLineError, A]`:
 *    - `NotFound`      → 404
 *    - `Unauthorized`  → 401 / 403
 *    - `ApiError`      → other non-2xx
 *    - `NetworkError`  → transport failure
 */
trait HookLineClient:
  // ─── Events ──────────────────────────────────────────────────────────────
  def publishEvent(topic: String, payload: Json): IO[HookLineError, Event]
  def getEvent(eventId: String): IO[HookLineError, Event]
  def listEvents(limit: Int = 50): IO[HookLineError, List[Event]]

  // ─── Endpoints ───────────────────────────────────────────────────────────
  def createEndpoint(req: CreateEndpointRequest): IO[HookLineError, Endpoint]
  def getEndpoint(endpointId: String): IO[HookLineError, Endpoint]
  def listEndpoints(): IO[HookLineError, List[Endpoint]]
  def updateEndpoint(endpointId: String, req: UpdateEndpointRequest): IO[HookLineError, Endpoint]
  def deleteEndpoint(endpointId: String): IO[HookLineError, Unit]

  // ─── Subscriptions ───────────────────────────────────────────────────────
  def createSubscription(req: CreateSubscriptionRequest): IO[HookLineError, Subscription]
  def getSubscription(subscriptionId: String): IO[HookLineError, Subscription]
  def listSubscriptions(): IO[HookLineError, List[Subscription]]
  def deleteSubscription(subscriptionId: String): IO[HookLineError, Unit]

  // ─── Deliveries ──────────────────────────────────────────────────────────
  def listDeliveries(limit: Int = 50): IO[HookLineError, List[Delivery]]
  def listDlq(limit: Int = 50): IO[HookLineError, List[DlqEntry]]
  def replayEvent(eventId: String): IO[HookLineError, Unit]

  // ─── API Keys ────────────────────────────────────────────────────────────
  def createApiKey(req: CreateApiKeyRequest): IO[HookLineError, ApiKeyCreated]
  def listApiKeys(): IO[HookLineError, List[ApiKey]]
  def deleteApiKey(keyId: String): IO[HookLineError, Unit]

  // ─── Health ──────────────────────────────────────────────────────────────
  def health(): IO[HookLineError, Json]

object HookLineClient:

  /** Build a live client layer.  Embeds its own HTTP client — no external dependencies needed.
   *
   *  @param baseUrl  e.g. "https://api.hookline.io"
   *  @param apiKey   bearer token
   */
  def live(baseUrl: String, apiKey: String): ZLayer[Any, Nothing, HookLineClient] =
    (Scope.default >>> Client.default).orDie >>>
      ZLayer.fromZIO(
        for
          client <- ZIO.service[Client]
          url    <- ZIO.fromEither(URL.decode(baseUrl))
                      .mapError(e => new Exception(s"Invalid HookLine base URL '$baseUrl': $e"))
                      .orDie
        yield Live(client, url, apiKey)
      )

  // ─── Internal helper types ────────────────────────────────────────────────

  private final case class ListResponse[A](items: List[A])
  private object ListResponse:
    given [A: JsonDecoder]: JsonDecoder[ListResponse[A]] = DeriveJsonDecoder.gen[ListResponse[A]]

  private final case class PublishRequest(topic: String, payload: Json)
  private object PublishRequest:
    given JsonEncoder[PublishRequest] = DeriveJsonEncoder.gen[PublishRequest]

  private final case class ReplayRequest(event_id: String)
  private object ReplayRequest:
    given JsonEncoder[ReplayRequest] = DeriveJsonEncoder.gen[ReplayRequest]

  // ─── Live implementation ──────────────────────────────────────────────────

  private[hookline] final class Live(client: Client, baseUrl: URL, apiKey: String) extends HookLineClient:

    private val auth        = Authorization.Bearer(apiKey)
    private val contentJson = Header.ContentType(MediaType.application.json)
    private val accept      = Header.Accept(MediaType.application.json)
    private val baseHeaders = Headers(auth, contentJson, accept)

    // ── helpers ──

    private def urlFor(path: String): URL =
      baseUrl.path(Path.decode(path))

    private def get[A: JsonDecoder](path: String): IO[HookLineError, A] =
      val req = Request(url = urlFor(path), method = Method.GET, headers = baseHeaders)
      execute(req).flatMap(decodeBody[A])

    private def post[B: JsonEncoder, A: JsonDecoder](path: String, body: B): IO[HookLineError, A] =
      val req = Request(
        url     = urlFor(path),
        method  = Method.POST,
        headers = baseHeaders,
        body    = Body.fromString(body.toJson)
      )
      execute(req).flatMap(decodeBody[A])

    private def patch[B: JsonEncoder, A: JsonDecoder](path: String, body: B): IO[HookLineError, A] =
      val req = Request(
        url     = urlFor(path),
        method  = Method.PATCH,
        headers = baseHeaders,
        body    = Body.fromString(body.toJson)
      )
      execute(req).flatMap(decodeBody[A])

    private def delete(path: String): IO[HookLineError, Unit] =
      val req = Request(url = urlFor(path), method = Method.DELETE, headers = baseHeaders)
      execute(req).unit

    private def execute(req: Request): IO[HookLineError, Response] =
      ZIO.scoped {
        client.request(req).mapError(e => NetworkError(e)).flatMap { resp =>
          resp.status.code match
            case c if c >= 200 && c < 300 => ZIO.succeed(resp)
            case 401 | 403 =>
              resp.body.asString.orDie.flatMap(b => ZIO.fail(Unauthorized(b)))
            case 404 =>
              resp.body.asString.orDie.flatMap(b => ZIO.fail(NotFound(b)))
            case c =>
              resp.body.asString.orDie.flatMap(b => ZIO.fail(ApiError(c, b)))
        }
      }

    private def decodeBody[A: JsonDecoder](resp: Response): IO[HookLineError, A] =
      resp.body.asString.orDie.flatMap { s =>
        ZIO.fromEither(s.fromJson[A])
          .mapError(e => ApiError(200, s"Failed to decode response: $e"))
      }

    private def getList[A: JsonDecoder](path: String): IO[HookLineError, List[A]] =
      get[ListResponse[A]](path).map(_.items)

    // ── Events ──

    def publishEvent(topic: String, payload: Json): IO[HookLineError, Event] =
      post[PublishRequest, Event]("/v1/events", PublishRequest(topic, payload))

    def getEvent(eventId: String): IO[HookLineError, Event] =
      get[Event](s"/v1/events/$eventId")

    def listEvents(limit: Int = 50): IO[HookLineError, List[Event]] =
      getList[Event](s"/v1/events?limit=$limit")

    // ── Endpoints ──

    def createEndpoint(req: CreateEndpointRequest): IO[HookLineError, Endpoint] =
      post[CreateEndpointRequest, Endpoint]("/v1/endpoints", req)

    def getEndpoint(endpointId: String): IO[HookLineError, Endpoint] =
      get[Endpoint](s"/v1/endpoints/$endpointId")

    def listEndpoints(): IO[HookLineError, List[Endpoint]] =
      getList[Endpoint]("/v1/endpoints")

    def updateEndpoint(endpointId: String, req: UpdateEndpointRequest): IO[HookLineError, Endpoint] =
      patch[UpdateEndpointRequest, Endpoint](s"/v1/endpoints/$endpointId", req)

    def deleteEndpoint(endpointId: String): IO[HookLineError, Unit] =
      delete(s"/v1/endpoints/$endpointId")

    // ── Subscriptions ──

    def createSubscription(req: CreateSubscriptionRequest): IO[HookLineError, Subscription] =
      post[CreateSubscriptionRequest, Subscription]("/v1/subscriptions", req)

    def getSubscription(subscriptionId: String): IO[HookLineError, Subscription] =
      get[Subscription](s"/v1/subscriptions/$subscriptionId")

    def listSubscriptions(): IO[HookLineError, List[Subscription]] =
      getList[Subscription]("/v1/subscriptions")

    def deleteSubscription(subscriptionId: String): IO[HookLineError, Unit] =
      delete(s"/v1/subscriptions/$subscriptionId")

    // ── Deliveries ──

    def listDeliveries(limit: Int = 50): IO[HookLineError, List[Delivery]] =
      getList[Delivery](s"/v1/deliveries?limit=$limit")

    def listDlq(limit: Int = 50): IO[HookLineError, List[DlqEntry]] =
      getList[DlqEntry](s"/v1/dlq?limit=$limit")

    def replayEvent(eventId: String): IO[HookLineError, Unit] =
      post[ReplayRequest, Json]("/v1/replay", ReplayRequest(eventId)).unit

    // ── API Keys ──

    def createApiKey(req: CreateApiKeyRequest): IO[HookLineError, ApiKeyCreated] =
      post[CreateApiKeyRequest, ApiKeyCreated]("/v1/apikeys", req)

    def listApiKeys(): IO[HookLineError, List[ApiKey]] =
      getList[ApiKey]("/v1/apikeys")

    def deleteApiKey(keyId: String): IO[HookLineError, Unit] =
      delete(s"/v1/apikeys/$keyId")

    // ── Health ──

    def health(): IO[HookLineError, Json] =
      get[Json]("/healthz")
