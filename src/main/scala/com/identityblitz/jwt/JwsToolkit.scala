package com.identityblitz.jwt

import com.identityblitz.utils.json._
import java.net.URI
import com.identityblitz.utils.json.JSuccess
import scala.util.{Failure, Success, Try}
import javax.security.cert.X509Certificate

trait JwsToolkit extends AlgorithmsKit with JwtToolkit with JwkToolkit {

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
  class JWS(val alg: Algorithm[JWS, _], values: JObj) extends Header[JWS] {
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

  val none = Algorithm[JWS, JWSNoneBuilder]("none",
    (hdr, tkn) => {
      if(!tkn.endsWith("."))
        throw new IllegalStateException("token has wrong format")
      tkn.split("\\.").toList match {
        case _ :: cs :: Nil =>
          new JWTBase[JWS](new JWSNone(hdr), JVal.parseStr(base64ToUtf8String(cs)).as[JObj])
        case _ => throw new IllegalStateException("token has wrong format")
      }},
    j => j.header.asBase64 + "." + j.claimSet.asBase64 + ".",
    new JWSNone(_),
    new JWSNoneBuilder(_)
  )

  val HS256 = Algorithm[JWS, JWSBuilder]("HS256",
    (hdr, tkn) => null,
    j => null,
    hdr => null,
    new JWSBuilder(_)
  )

  private sealed class JWSNone(private val values: JObj) extends JWS(none, values)

  sealed class JWSNoneBuilder(alg: Algorithm[JWS, JWSNoneBuilder]) {
    def header(hs: (String, JVal)*) = ???
  }

  sealed class JWSBuilder(alg: Algorithm[JWS, JWSBuilder]) {
    def header(hs: (String, JVal)*)(implicit crypto: CryptoService) = ???
  }
}
