package io.hookline

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import java.security.MessageDigest
import java.util.HexFormat

/** Utility functions for the HookLine webhook platform. */
object HookLine:

  private val HmacAlgo      = "HmacSHA256"
  private val SignaturePrefix = "v1="
  private val MaxAgeMillis    = 300000L // 5 minutes

  /** Verify the HMAC-SHA256 signature on an incoming webhook.
   *
   *  Header format:   `x-gp-signature: v1=<hex>`
   *  Header timestamp: `x-gp-timestamp: <unix-milliseconds>`
   *  Signed payload:  `{timestamp}.{rawBody}`
   *
   *  @param secret    Endpoint secret (UTF-8 string)
   *  @param timestamp Value of the `x-gp-timestamp` header
   *  @param signature Value of the `x-gp-signature` header
   *  @param body      Raw request body bytes
   *  @return          `true` if signature is valid and not expired
   */
  def verifySignature(
    secret:    String,
    timestamp: String,
    signature: String,
    body:      Array[Byte]
  ): Boolean =
    try
      // Reject expired timestamps (replay protection)
      val ts = timestamp.toLong
      val now = System.currentTimeMillis()
      if math.abs(now - ts) > MaxAgeMillis then return false

      // Strip the "v1=" prefix
      if !signature.startsWith(SignaturePrefix) then return false
      val expectedHex = signature.drop(SignaturePrefix.length)

      // Compute HMAC-SHA256("timestamp.body", secret)
      val mac = Mac.getInstance(HmacAlgo)
      mac.init(new SecretKeySpec(secret.getBytes("UTF-8"), HmacAlgo))
      mac.update(timestamp.getBytes("UTF-8"))
      mac.update('.'.toByte)
      mac.update(body)
      val computed = mac.doFinal()

      val expectedBytes =
        try HexFormat.of().parseHex(expectedHex)
        catch case _: Exception => return false

      // Constant-time comparison to prevent timing attacks
      MessageDigest.isEqual(computed, expectedBytes)

    catch
      case _: Exception => false
