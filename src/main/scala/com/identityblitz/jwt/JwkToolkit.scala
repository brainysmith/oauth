package com.identityblitz.jwt

import java.net.URI
import javax.security.cert.X509Certificate
import com.identityblitz.utils.json._

/**
 *
 */
trait JwkToolkit {

  implicit object JwkReader extends JReader[JWK] {
    def read(v: JVal): JResult[JWK] = v match {
      case _ => JError("not realised yet")
    }
  }

  implicit object JwkWriter extends JWriter[JWK] {
    def write(o: JWK): JVal = throw new UnsupportedOperationException("not realised yet")
  }

  implicit object JX509Reader extends JReader[X509Certificate] {
    def read(v: JVal): JResult[X509Certificate] = v match {
      case _ => JError("not realised yet")
    }
  }

  implicit object JX509Writer extends JWriter[X509Certificate] {
    def write(o: X509Certificate): JVal = throw new UnsupportedOperationException("not realised yet")
  }

  implicit object JBytesReader extends JReader[Array[Byte]] {
    def read(v: JVal): JResult[Array[Byte]] = v match {
      case _ => JError("not realised yet")
    }
  }

  implicit object JBytesWriter extends JWriter[Array[Byte]] {
    def write(o: Array[Byte]): JVal = throw new UnsupportedOperationException("not realised yet")
  }

  trait Kty

  object Use extends Enumeration {
    type Use = Value
    val sig = Value("sig")
    val enc = Value("enc")
  }
  import Use._

  object KeyOps extends Enumeration {
    type KeyOps = Value
    val sign = Value("sign")
    val verify = Value("verify")
    val encrypt = Value("encrypt")
    val decrypt = Value("decrypt")
    val wrapKey = Value("wrapKey")
    val unwrapKey = Value("unwrapKey")
    val deriveKey = Value("deriveKey")
    val deriveBits = Value("deriveBits")
  }
  import KeyOps._

  trait JWK {

    val kty: Kty

    val use: Option[Use]

    val key_ops: Option[KeyOps]

    val alg: Option[String]

    val kid: Option[String]

    val x5u: Option[URI]

    val x5c: Option[Array[X509Certificate]]

    val x5t: Option[Array[Byte]]

  }

}
