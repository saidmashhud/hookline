package io.hookline

import io.hookline.models.*
import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect

/** Integration-style tests against an in-process HTTP stub server. */
object HookLineClientSpec extends ZIOSpecDefault:

  // ── Stub response fixtures ────────────────────────────────────────────────

  private val stubEventJson =
    """|{
       |  "event_id":   "evt_001",
       |  "tenant_id":  "ten_001",
       |  "topic":      "orders.created",
       |  "payload":    {"amount": 99},
       |  "created_at": "2024-01-01T00:00:00Z"
       |}""".stripMargin

  private val stubListJson = s"""{"items": [$stubEventJson]}"""

  private def jsonResp(status: Status, body: String): Response =
    Response(
      status  = status,
      headers = Headers(Header.ContentType(MediaType.application.json)),
      body    = Body.fromString(body)
    )

  // ── Test server routes ────────────────────────────────────────────────────

  private val testRoutes = Routes(

    Method.POST / "v1" / "events" ->
      handler((_: Request) => jsonResp(Status.Created, stubEventJson)),

    Method.GET / "v1" / "events" / string("id") ->
      handler((_: String, _: Request) => jsonResp(Status.Ok, stubEventJson)),

    Method.GET / "v1" / "events" ->
      handler((_: Request) => jsonResp(Status.Ok, stubListJson)),

    Method.GET / "healthz" ->
      handler((_: Request) => jsonResp(Status.Ok, """{"status":"ok"}""")),

    Method.DELETE / "v1" / "endpoints" / string("id") ->
      handler((_: String, _: Request) => Response(status = Status.NoContent)),

    Method.GET / "v1" / "endpoints" / string("id") ->
      handler { (_: String, _: Request) =>
        jsonResp(Status.NotFound, """{"error":"not found"}""")
      }
  )

  // ── Helper: run a test with a real client pointing at the stub server ─────

  private def withClient[A](
    f: HookLineClient => IO[HookLineError, A]
  ): ZIO[Client & Server, Throwable, A] =
    for
      port   <- Server.install(testRoutes)
      url     = URL.decode(s"http://localhost:$port").toOption.get
      client <- ZIO.service[Client]
      hlc     = new HookLineClient.Live(client, url, "test-key")
      result <- f(hlc)
    yield result

  // ── Tests ─────────────────────────────────────────────────────────────────

  def spec = suite("HookLineClient")(

    test("publishEvent returns Event on 201") {
      withClient { c =>
        c.publishEvent("orders.created", Json.Obj(Chunk("amount" -> Json.Num(99))))
      }.map { ev =>
        assertTrue(ev.event_id == "evt_001") &&
        assertTrue(ev.topic    == "orders.created")
      }
    },

    test("getEvent returns Event on 200") {
      withClient(_.getEvent("evt_001")).map { ev =>
        assertTrue(ev.event_id == "evt_001")
      }
    },

    test("listEvents returns list") {
      withClient(_.listEvents()).map { evs =>
        assertTrue(evs.length == 1) &&
        assertTrue(evs.head.event_id == "evt_001")
      }
    },

    test("getEndpoint 404 lifts to NotFound") {
      withClient(_.getEndpoint("missing")).either.map { result =>
        assertTrue(result.isLeft) &&
        assertTrue(result.left.exists(_.isInstanceOf[HookLineError.NotFound]))
      }
    },

    test("deleteEndpoint succeeds on 204") {
      withClient(_.deleteEndpoint("ep_001")).as(assertTrue(true))
    },

    test("health returns Json.Obj") {
      withClient(_.health()).map {
        case Json.Obj(_) => assertTrue(true)
        case _           => assertTrue(false)
      }
    }

  ) @@ TestAspect.sequential provide(
    Client.default,
    Scope.default,
    Server.defaultWith(_.port(0))
  )
