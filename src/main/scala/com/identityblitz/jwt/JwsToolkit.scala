package com.identityblitz.jwt

import com.identityblitz.utils.json._
import java.net.URI
import com.identityblitz.utils.json.JSuccess
import scala.util.{Failure, Success, Try}
import javax.security.cert.X509Certificate
import org.apache.commons.codec.binary.Base64
import org.apache.commons.codec.binary

trait JwsToolkit extends AlgorithmsKit with JwtToolkit with JwkToolkit {

  val cryptoService: CryptoService

  trait JWSNameKit extends BaseNameKit {

    val jku = Name[URI]("jku", (v) => {})
    val jwk = Name[JWK]("jwk", (v) => {})
    val kid = Name[String]("kid", checkIfBlank("kid"))
    val x5u = Name[URI]("x5u", (v) => {})
    val x5c = Name[Array[X509Certificate]]("x5c", checkIfEmptyArray("x5c"))
    val x5t = Name[Array[Byte]]("x5t", (v) => {})
    val crit = Name[Array[String]]("crit", checkIfEmptyArray("crit"))

  }

  object JWSNameKit extends JWSNameKit

  /**
   * This traits is represents JWS header.
   */
  class JWS(val alg: Algorithm[JWS, _], values: JObj)(implicit val crypto: CryptoService) extends Header[JWS] {
    val typ: Option[String] = values(BaseNameKit.typ.name).asOpt[String]
    val cty: Option[String] = values(BaseNameKit.cty.name).asOpt[String]

    val jku: Option[URI] = values(JWSNameKit.jku.name).asOpt[URI]
    val jwk: Option[JWK] = values(JWSNameKit.jwk.name).asOpt[JWK]
    val kid: Option[String] = values(JWSNameKit.kid.name).asOpt[String]
    val x5u: Option[URI] = values(JWSNameKit.x5u.name).asOpt[URI]
    val x5c: Option[Array[X509Certificate]] = values(JWSNameKit.x5c.name).asOpt[Array[X509Certificate]]
    val x5t: Option[Array[Byte]] = values(JWSNameKit.x5t.name).asOpt[Array[Byte]]
    val crit: Option[Array[String]] = values(JWSNameKit.crit.name).asOpt[Array[String]]



    def apply(name: String): JVal = values(name)
    def get(name: String): Option[JVal] = values(name).asOpt[JVal]

    def names:Set[String] = values.fields

    def asBase64: String = utf8StringToBase64((values + ("alg" -> alg.toString)).toJson)

    override def toString: String = (values + ("alg" -> alg.toString)).toString
  }

  val jwtPattern = """^([A-Za-z0-9_-]+\.([A-Za-z0-9_-]+))\.([A-Za-z0-9_-]*)$""".r

  val none = Algorithm[JWS, JWSNoneBuilder]("none",
    (alg, hdr, tkn) => {
      jwtPattern findFirstIn tkn match {
        case Some(jwtPattern(_, pl, _)) => (new JWSNone(hdr), Base64.decodeBase64(pl))
        case None => throw new IllegalStateException("token has wrong format")
      }},
    serializeNoneJws,
    new JWSNoneBuilder(_)
  )

  val HS256 = Algorithm[JWS, JWSBuilder]("HS256",
    deserializeJws(cryptoService),
    serializeJws,
    new JWSBuilder(_)
  )

  def serializeNoneJws(hdr: JWS, pl: Array[Byte]) = hdr.asBase64 + "." + Base64.encodeBase64URLSafeString(pl) + "."

  def serializeJws(hdr: JWS, pl: Array[Byte]) = {
    val hpl = hdr.asBase64 + "." + Base64.encodeBase64URLSafeString(pl)
    val signature = Base64.encodeBase64URLSafeString(hdr.crypto.sign(hdr.alg.name, null, hpl.getBytes("US-ASCII")))
    hpl + "." + signature
  }

  def deserializeJws(cs: CryptoService)(alg: Algorithm[JWS, _], hdr: JObj, tkn: String) = {
    jwtPattern findFirstIn tkn match {
      case Some(jwtPattern(hpl, pl, s)) =>
        val jws = new JWS(alg, hdr)(cs)
        if(!cs.verify(alg.name, null, hpl.getBytes("US-ASCII"),Base64.decodeBase64(s)))
          throw new IllegalStateException("Signature is wrong.")
        (jws, Base64.decodeBase64(pl))
      case None => throw new IllegalStateException("token has wrong format")
    }
  }



  private sealed class JWSNone(private val values: JObj) extends JWS(none, values)(null)

  /**
   * JWS header builder.
   * @param _alg - signature algorithm used to protect token
   */
  sealed class JWSNoneBuilder(_alg: Algorithm[JWS, JWSNoneBuilder]) {
    def header(hs: (String, JVal)*) = new PayloadBuilder[JWS] {
      val header: JWS = new JWSNone(JObj(hs))
    }
  }

  sealed class JWSBuilder(alg: Algorithm[JWS, JWSBuilder]) {
    def header(hs: (String, JVal)*)(implicit crypto: CryptoService) = new PayloadBuilder[JWS] {
      val header: JWS = new JWS(alg, JObj(hs))
    }
  }
}
