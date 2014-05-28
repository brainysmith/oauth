package com.identityblitz.utils

import java.security._
import java.security.spec._
import java.math.BigInteger

object HashUtils {

  def calcSha1(data: Array[Byte]): Array[Byte] = {
    val md = MessageDigest.getInstance("SHA-1")
    md.digest(data)
  }

  def genECPublicKey(curv: String, x: Array[Byte], y: Array[Byte]): PublicKey = {
    val ap = AlgorithmParameters.getInstance("EC")
    ap.init(new ECGenParameterSpec(curv))
    val paramSpec = ap.getParameterSpec(classOf[ECParameterSpec])
    val keySpec = new ECPublicKeySpec(new ECPoint(new BigInteger(1, x), new BigInteger(1, y)), paramSpec)
    val kf = KeyFactory.getInstance("EC")
    kf.generatePublic(keySpec)
  }

  def genECPrivateKey(curv: String, d: Array[Byte]): PrivateKey = {
    val ap = AlgorithmParameters.getInstance("EC")
    ap.init(new ECGenParameterSpec(curv))
    val paramSpec = ap.getParameterSpec(classOf[ECParameterSpec])
    val keySpec = new ECPrivateKeySpec(new BigInteger(1, d), paramSpec)
    val kf = KeyFactory.getInstance("EC")
    kf.generatePrivate(keySpec)
  }

}
