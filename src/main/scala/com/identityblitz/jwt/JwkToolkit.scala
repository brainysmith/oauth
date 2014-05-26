package com.identityblitz.jwt

import java.net.URI
import javax.security.cert.X509Certificate
import com.identityblitz.utils.json._
import scala.annotation.implicitNotFound
import org.apache.commons.codec.binary.Base64
import scala.util.{Failure, Success, Try}
import scala.util.Success
import scala.util.Failure
import com.identityblitz.utils.json.JSuccess
import java.math.BigInteger

/**
 *
 */
trait JwkToolkit {

  val cryptoService: CryptoService

  implicit object JwkReader extends JReader[JWK] {
    def read(v: JVal): JResult[JWK] = v match {
      case o: JObj => o match {
        case EcPublicKey(e) => JSuccess(e)
        case EcPrivateKey(e) => JSuccess(e)
        case RsaPublicKey(e) => JSuccess(e)
        case RsaPrivateKey(e) => JSuccess(e)
        case SymmetricKey(e) => JSuccess(e)
        case _ => JError("json.error.unknownJwk")
      }
      case _ => JError("json.error.expected.object")
    }
  }

  implicit object JwkWriter extends JWriter[JWK] {
    def write(o: JWK): JVal = throw new UnsupportedOperationException("not realised yet")
  }

  implicit object JX509Reader extends JReader[X509Certificate] {
    def read(v: JVal): JResult[X509Certificate] = v match {
      case JStr(s) => Try(cryptoService.createX509Cert(Base64.decodeBase64(s))) match {
        case Success(cert) => JSuccess(cert)
        case Failure(err) => JError(err.getMessage)
      }
      case _ => JError("json.error.expected.string")
    }
  }

  implicit object JX509Writer extends JWriter[X509Certificate] {
    def write(o: X509Certificate): JVal = throw new UnsupportedOperationException("not realised yet")
  }

  implicit object JBytesReader extends JReader[Array[Byte]] {
    def read(v: JVal): JResult[Array[Byte]] = v match {
      case JStr(s) => JSuccess(Base64.decodeBase64(s))
      case _ => JError("json.error.expected.string")
    }
  }

  implicit object JBytesWriter extends JWriter[Array[Byte]] {
    def write(o: Array[Byte]): JVal = throw new UnsupportedOperationException("not realised yet")
  }

  implicit object JUriReader extends JReader[URI] {
    def read(v: JVal): JResult[URI] = v match {
      case o: JStr => Try(new URI(o)) match {
        case Success(r) => JSuccess(r)
        case Failure(e) => JError(e.getMessage)
      }
      case _ => JError("json.error.expected.string")
    }
  }

  implicit object JUriWriter extends JWriter[URI] {
    def write(o: URI): JVal = JStr(o.toString)
  }

  object Use extends Enumeration {
    type Use = Value
    val sig = Value("sig")
    val enc = Value("enc")


    implicit object JUseReader extends JReader[Use] {
      def read(v: JVal): JResult[Use] = v match {
        case JStr(s) => Use.values.find(_.toString == s).map(JSuccess(_))
          .orElse(Option(JError("json.error.worngValueOfUse"))).get
        case _ => JError("json.error.expected.string")
      }
    }

    implicit object JUseWriter extends JWriter[Use] {
      def write(o: Use): JVal = JStr(o.toString)
    }
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

    implicit object JKeyOpsReader extends JReader[KeyOps]{
      def read(v: JVal): JResult[KeyOps] = v match {
        case JStr(s) => KeyOps.values.find(_.toString == s).map(JSuccess(_))
          .orElse(Option(JError("json.error.worngValueOfKeyOps"))).get
        case _ => JError("json.error.expected.string")
      }
    }

    implicit object JKeyOpsWriter extends JWriter[KeyOps] {
      def write(o: KeyOps): JVal = JStr(o.toString)
    }
  }
  import KeyOps._

  trait JWK {

    val kty: String

    val use: Option[Use]

    val key_ops: Option[Set[KeyOps]]

    val alg: Option[String]

    val kid: Option[String]

    val x5u: Option[URI]

    val x5c: Option[Array[X509Certificate]]

    val x5t: Option[Array[Byte]]

  }

  class EcPublicKey(val crv: String,
                    val x: Array[Byte],
                    val y: Array[Byte],
                    val use: Option[Use],
                    val key_ops: Option[Set[KeyOps]],
                    val alg: Option[String],
                    val kid: Option[String],
                    val x5u: Option[URI],
                    val x5c: Option[Array[X509Certificate]],
                    val x5t: Option[Array[Byte]]) extends JWK {

    //add some checks

    val kty: String = "EC"

  }

  object EcPublicKey {

    implicit object JEcPublicKeyReader extends JReader[EcPublicKey] {
      def read(v: JVal): JResult[EcPublicKey] = {
        JSuccess(new EcPublicKey((v \ "crv").as[String],
          Base64.decodeBase64((v \ "x").as[String]),
          Base64.decodeBase64((v \ "y").as[String]),
          (v \ "use").asOpt[Use],
          (v \ "key_ops").asOpt[Array[KeyOps]].map(_.toSet),
          (v \ "alg").asOpt[String],
          (v \ "kid").asOpt[String],
          (v \ "x5u").asOpt[URI],
          (v \ "x5c").asOpt[Array[X509Certificate]],
          (v \ "x5t").asOpt[Array[Byte]]))
      }
    }

    def unapply(jwk: JObj): Option[EcPublicKey] =
      if(jwk("kty").as[String] == "EC" &&
        jwk.fields.contains("crv") &&
        jwk.fields.contains("x") &&
        jwk.fields.contains("y") &&
        !jwk.fields.contains("d")) jwk.asOpt[EcPublicKey] else None

  }

  class EcPrivateKey(val crv: String,
                     val x: Array[Byte],
                     val y: Array[Byte],
                     val d: Array[Byte],
                     val use: Option[Use],
                     val key_ops: Option[Set[KeyOps]],
                     val alg: Option[String],
                     val kid: Option[String],
                     val x5u: Option[URI],
                     val x5c: Option[Array[X509Certificate]],
                     val x5t: Option[Array[Byte]]) extends JWK {

    //add some checks

    val kty: String = "EC"
  }

  object EcPrivateKey {

    implicit object JEcPrivateKeyReader extends JReader[EcPrivateKey] {
      def read(v: JVal): JResult[EcPrivateKey] = {
        JSuccess(new EcPrivateKey((v \ "crv").as[String],
          Base64.decodeBase64((v \ "x").as[String]),
          Base64.decodeBase64((v \ "y").as[String]),
          Base64.decodeBase64((v \ "d").as[String]),
          (v \ "use").asOpt[Use],
          (v \ "key_ops").asOpt[Array[KeyOps]].map(_.toSet),
          (v \ "alg").asOpt[String],
          (v \ "kid").asOpt[String],
          (v \ "x5u").asOpt[URI],
          (v \ "x5c").asOpt[Array[X509Certificate]],
          (v \ "x5t").asOpt[Array[Byte]]
        ))
      }
    }

    def unapply(jwk: JObj): Option[EcPrivateKey] =
      if(jwk.fields.contains("crv") &&
        jwk.fields.contains("x") &&
        jwk.fields.contains("y") &&
        jwk.fields.contains("d")) jwk.asOpt[EcPrivateKey] else None

  }

  class RsaPublicKey(val n: BigInteger,
                     val e: BigInteger,
                     val use: Option[Use],
                     val key_ops: Option[Set[KeyOps]],
                     val alg: Option[String],
                     val kid: Option[String],
                     val x5u: Option[URI],
                     val x5c: Option[Array[X509Certificate]],
                     val x5t: Option[Array[Byte]]) extends JWK {

    //add some checks

    val kty: String = "RSA"

  }

  object RsaPublicKey {

    implicit object JRsaPublicKeyReader extends JReader[RsaPublicKey] {
      def read(v: JVal): JResult[RsaPublicKey] = {
        JSuccess(new RsaPublicKey(new BigInteger(1, Base64.decodeBase64((v \ "n").as[String])),
          new BigInteger(1, Base64.decodeBase64((v \ "e").as[String])),
          (v \ "use").asOpt[Use],
          (v \ "key_ops").asOpt[Array[KeyOps]].map(_.toSet),
          (v \ "alg").asOpt[String],
          (v \ "kid").asOpt[String],
          (v \ "x5u").asOpt[URI],
          (v \ "x5c").asOpt[Array[X509Certificate]],
          (v \ "x5t").asOpt[Array[Byte]]))
      }
    }

    def unapply(jwk: JObj): Option[RsaPublicKey] =
      if(jwk("kty").as[String] == "RSA" &&
        jwk.fields.contains("n") &&
        jwk.fields.contains("e") &&
        !jwk.fields.contains("d")) jwk.asOpt[RsaPublicKey] else None

  }

  class PrimeInfo(val r: BigInteger,
                  val d: BigInteger,
                  val t: BigInteger)

  class RsaPrivateKey(val n: BigInteger,
                      val e: BigInteger,
                      val d: BigInteger,
                      val p: Option[BigInteger],
                      val q: Option[BigInteger],
                      val dp: Option[BigInteger],
                      val dq: Option[BigInteger],
                      val qi: Option[BigInteger],
                      val oth: Option[Array[PrimeInfo]],
                      val use: Option[Use],
                      val key_ops: Option[Set[KeyOps]],
                      val alg: Option[String],
                      val kid: Option[String],
                      val x5u: Option[URI],
                      val x5c: Option[Array[X509Certificate]],
                      val x5t: Option[Array[Byte]]) extends JWK {


    //add some checks

    val kty: String = "RSA"

  }

  object RsaPrivateKey {

    implicit object JPrimeInfo extends JReader[PrimeInfo]{
      def read(v: JVal): JResult[PrimeInfo] = JSuccess(
        new PrimeInfo(new BigInteger(1, Base64.decodeBase64((v \ "r").as[String])),
          new BigInteger(1, Base64.decodeBase64((v \ "d").as[String])),
          new BigInteger(1, Base64.decodeBase64((v \ "t").as[String]))))
    }

    implicit object JRsaPrivateKeyReader extends JReader[RsaPrivateKey] {
      def read(v: JVal): JResult[RsaPrivateKey] = {
        JSuccess(new RsaPrivateKey(new BigInteger(1, Base64.decodeBase64((v \ "n").as[String])),
          new BigInteger(1, Base64.decodeBase64((v \ "e").as[String])),
          new BigInteger(1, Base64.decodeBase64((v \ "d").as[String])),
          (v \ "p").asOpt[String].map(a => new BigInteger(1, Base64.decodeBase64(a))),
          (v \ "q").asOpt[String].map(a => new BigInteger(1, Base64.decodeBase64(a))),
          (v \ "dp").asOpt[String].map(a => new BigInteger(1, Base64.decodeBase64(a))),
          (v \ "dq").asOpt[String].map(a => new BigInteger(1, Base64.decodeBase64(a))),
          (v \ "qi").asOpt[String].map(a => new BigInteger(1, Base64.decodeBase64(a))),
          (v \ "oth").asOpt[Array[PrimeInfo]],
          (v \ "use").asOpt[Use],
          (v \ "key_ops").asOpt[Array[KeyOps]].map(_.toSet),
          (v \ "alg").asOpt[String],
          (v \ "kid").asOpt[String],
          (v \ "x5u").asOpt[URI],
          (v \ "x5c").asOpt[Array[X509Certificate]],
          (v \ "x5t").asOpt[Array[Byte]]))
      }
    }

    def unapply(jwk: JObj): Option[RsaPrivateKey] =
      if(jwk("kty").as[String] == "RSA" &&
        jwk.fields.contains("n") &&
        jwk.fields.contains("e") &&
        jwk.fields.contains("d")) jwk.asOpt[RsaPrivateKey] else None

  }

  class SymmetricKey(val k: Array[Byte],
                     val use: Option[Use],
                     val key_ops: Option[Set[KeyOps]],
                     val alg: Option[String],
                     val kid: Option[String],
                     val x5u: Option[URI],
                     val x5c: Option[Array[X509Certificate]],
                     val x5t: Option[Array[Byte]]) extends JWK {


    //add some checks

    val kty: String = "oct"

  }

  object SymmetricKey {

    implicit object JSymmetricKeyReader extends JReader[SymmetricKey] {
      def read(v: JVal): JResult[SymmetricKey] = {
        JSuccess(new SymmetricKey(Base64.decodeBase64((v \ "k").as[String]),
          (v \ "use").asOpt[Use],
          (v \ "key_ops").asOpt[Array[KeyOps]].map(_.toSet),
          (v \ "alg").asOpt[String],
          (v \ "kid").asOpt[String],
          (v \ "x5u").asOpt[URI],
          (v \ "x5c").asOpt[Array[X509Certificate]],
          (v \ "x5t").asOpt[Array[Byte]]))
      }
    }

    def unapply(jwk: JObj): Option[SymmetricKey] =
      if(jwk("kty").as[String] == "oct" &&
        jwk.fields.contains("k")) jwk.asOpt[SymmetricKey] else None

  }

  @implicitNotFound("No Kids register found. Try to implement an implicit KidsRegister.")
  trait KidsRegister {

    def apply(kid: String): JWK
    def get(kid: String): Option[JWK]

    def curKid(alg: String): JWK
    def optCurKid(alg: String): Option[JWK]

    def curKid(alg: String, ctx: Map[String, String]): JWK
    def optCurKid(alg: String, ctx: Map[String, String]): Option[JWK]

  }

  @implicitNotFound("No cryptographic service found. Try to implement an implicit CryptoService.")
  trait CryptoService {

    def sign(alg: String, jwk: JWK, plainText: Array[Byte]): Array[Byte]

    def verify(alg: String, jwk: JWK, plainText: Array[Byte], signature: Array[Byte]): Boolean

    def encrypt(alg: String, jwk: JWK, plainText: Array[Byte]): Array[Byte]

    def decrypt(alg: String, jwk: JWK, cipherText: Array[Byte]): Array[Byte]

    def createX509Cert(der: Array[Byte]): X509Certificate

  }

}
