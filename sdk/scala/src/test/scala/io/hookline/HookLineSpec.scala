package io.hookline

import zio.*
import zio.test.*
import zio.test.Assertion.*
import java.lang.{System => JSystem}

object HookLineSpec extends ZIOSpecDefault:

  // A fixed secret used across tests
  private val secret    = "test-secret-key"

  // Pre-computed valid signature for body `{"ok":true}` with the above secret/timestamp
  private def computeSignature(ts: String, body: String): String =
    import javax.crypto.Mac
    import javax.crypto.spec.SecretKeySpec
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(new SecretKeySpec(secret.getBytes("UTF-8"), "HmacSHA256"))
    mac.update(ts.getBytes("UTF-8"))
    mac.update('.'.toByte)
    mac.update(body.getBytes("UTF-8"))
    val bytes = mac.doFinal()
    "v1=" + bytes.map("%02x".format(_)).mkString

  def spec = suite("HookLine.verifySignature")(

    test("valid signature returns true") {
      val body = """{"ok":true}"""
      val now  = JSystem.currentTimeMillis().toString
      val sig  = computeSignature(now, body)
      assertTrue(HookLine.verifySignature(secret, now, sig, body.getBytes("UTF-8")))
    },

    test("wrong secret returns false") {
      val body = """{"ok":true}"""
      val now  = JSystem.currentTimeMillis().toString
      val sig  = computeSignature(now, body)
      assertTrue(!HookLine.verifySignature("wrong-secret", now, sig, body.getBytes("UTF-8")))
    },

    test("tampered body returns false") {
      val body    = """{"ok":true}"""
      val tampered = """{"ok":false}"""
      val now     = JSystem.currentTimeMillis().toString
      val sig     = computeSignature(now, body)
      assertTrue(!HookLine.verifySignature(secret, now, sig, tampered.getBytes("UTF-8")))
    },

    test("expired timestamp (>5 min) returns false") {
      val body = """{"ok":true}"""
      val old  = (JSystem.currentTimeMillis() - 600000L).toString // 10 min ago
      val sig  = computeSignature(old, body)
      assertTrue(!HookLine.verifySignature(secret, old, sig, body.getBytes("UTF-8")))
    },

    test("future timestamp (>5 min) returns false") {
      val body   = """{"ok":true}"""
      val future = (JSystem.currentTimeMillis() + 600000L).toString
      val sig    = computeSignature(future, body)
      assertTrue(!HookLine.verifySignature(secret, future, sig, body.getBytes("UTF-8")))
    },

    test("missing v1= prefix returns false") {
      val body  = """{"ok":true}"""
      val now   = JSystem.currentTimeMillis().toString
      val sig   = computeSignature(now, body).drop("v1=".length) // strip prefix
      assertTrue(!HookLine.verifySignature(secret, now, sig, body.getBytes("UTF-8")))
    },

    test("invalid hex in signature returns false") {
      val now = JSystem.currentTimeMillis().toString
      assertTrue(!HookLine.verifySignature(secret, now, "v1=ZZZZ", "body".getBytes("UTF-8")))
    },

    test("non-numeric timestamp returns false") {
      assertTrue(!HookLine.verifySignature(secret, "not-a-number", "v1=abc123", "body".getBytes("UTF-8")))
    }
  )
