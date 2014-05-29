package com.identityblitz.utils

import java.security._
import java.math.BigInteger
import org.bouncycastle.jce.ECNamedCurveTable
import org.bouncycastle.jce.spec.{ECPrivateKeySpec, ECPublicKeySpec}
import org.bouncycastle.asn1._

object CryptoUtils {

  def calcSha1(data: Array[Byte]): Array[Byte] = {
    val md = MessageDigest.getInstance("SHA-1")
    md.digest(data)
  }

  def genECPublicKey(curv: String, x: Array[Byte], y: Array[Byte]): PublicKey = {
    val spec = ECNamedCurveTable.getParameterSpec(curv)
    val point = spec.getCurve.createPoint(new BigInteger(1, x), new BigInteger(1, y), false)
    val keySpec = new ECPublicKeySpec(point, spec)
    val kf = KeyFactory.getInstance("EC")
    kf.generatePublic(keySpec)
  }

  def genECPrivateKey(curv: String, d: Array[Byte]): PrivateKey = {
    val spec = ECNamedCurveTable.getParameterSpec(curv)
    val keySpec = new ECPrivateKeySpec(new BigInteger(1, d), spec)
    val kf = KeyFactory.getInstance("EC")
    kf.generatePrivate(keySpec)
  }

  def encodeECSignature(join: Array[Byte]) = {
    val r = new BigInteger(1, join.take(32))
    val s = new BigInteger(1, join.takeRight(32))
    val v: ASN1EncodableVector = new ASN1EncodableVector
    v.add(new DERInteger(r))
    v.add(new DERInteger(s))
    new DERSequence(v).getEncoded(ASN1Encoding.DER)
  }

  def decodeECSignature(encoded: Array[Byte]) = {
    val seq = ASN1Primitive.fromByteArray(encoded).asInstanceOf[ASN1Sequence]
    val r = seq.getObjectAt(0).asInstanceOf[DERInteger].getValue.toByteArray.dropWhile(_ == 0)
      .reverse.padTo(32, 0x00.toByte).reverse
    val s = seq.getObjectAt(1).asInstanceOf[DERInteger].getValue.toByteArray.dropWhile(_ == 0)
      .reverse.padTo(32, 0x00.toByte).reverse
    r ++ s
  }

}
