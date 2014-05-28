package com.identityblitz.jwt

import com.identityblitz.utils.json._
import java.net.URI
import scala.util.{Failure, Success, Try}
import java.security.cert.X509Certificate
import org.apache.commons.codec.binary.Base64
import scala.annotation.tailrec

trait JwsToolkit extends AlgorithmsKit with JwtToolkit with JwkToolkit {

  val cryptoService: CryptoService
  val kidsRegister: KidsRegister

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

  val none = Algorithm[JWS, JWSNoneBuilder]("none","none", Use.sig,
    (alg, hdr, tkn) => {
      jwtPattern findFirstIn tkn match {
        case Some(jwtPattern(_, pl, _)) => (new JWSNone(hdr), Base64.decodeBase64(pl))
        case None => throw new IllegalStateException("token has wrong format")
      }},
    serializeNoneJws,
    new JWSNoneBuilder(_)
  )

  val HS256 = Algorithm[JWS, JWSBuilder]("HS256", "oct", Use.sig,
    deserializeJws(cryptoService, kidsRegister),
    serializeJws(kidsRegister),
    new JWSBuilder(_)
  )

  val RS256 = Algorithm[JWS, JWSBuilder]("RS256", "RSA", Use.sig,
    deserializeJws(cryptoService, kidsRegister),
    serializeJws(kidsRegister),
    new JWSBuilder(_)
  )

  def serializeNoneJws(hdr: JWS, pl: Array[Byte]) = hdr.asBase64 + "." + Base64.encodeBase64URLSafeString(pl) + "."

  def serializeJws(kr: KidsRegister)(hdr: JWS, pl: Array[Byte]) = {
    implicit val ics = hdr.crypto
    implicit val ikr = kr
    val hpl = hdr.asBase64 + "." + Base64.encodeBase64URLSafeString(pl)
    val signature = Base64.encodeBase64URLSafeString(makeSignature(hdr, hpl.getBytes("US-ASCII")))
    hpl + "." + signature
  }

  def deserializeJws(cs: CryptoService, kr: KidsRegister)(alg: Algorithm[JWS, _], hdr: JObj, tkn: String) = {
    implicit val ics = cs
    implicit val ikr = kr
    jwtPattern findFirstIn tkn match {
      case Some(jwtPattern(hpl, pl, s)) =>
        val jws = new JWS(alg, hdr)
        if(!checkSignature(alg.name, collectTryingJwk(jws), hpl.getBytes("US-ASCII"),Base64.decodeBase64(s)))
          throw new IllegalStateException("Signature is wrong.")
        (jws, Base64.decodeBase64(pl))
      case None => throw new IllegalStateException("token has wrong format")
    }
  }

  def checkSignature(alg: String, keys: Seq[JWK], data: Array[Byte], signature: Array[Byte])(implicit cs: CryptoService): Boolean = {
    @tailrec
    def core(keys: Seq[JWK]): Boolean = {
      if(keys.isEmpty) false
      else if(cs.verify(alg, keys.head, data, signature)) true else core(keys.tail)
    }
    core(keys)
  }

  /**
   * Methods to collect and filter out trying keys.
   */

  def collectTryingJwk(jws: JWS)(implicit kids: KidsRegister, crypto: CryptoService): Seq[JWK] = {
    val registered = (for (kid <- jws.kid; key <- kids.get(kid)) yield key).toSeq
    val fromJku = (for (jku <- jws.jku) yield downloadJwk(jku)).toSeq.flatten
    //JWK has been skipped because it is not clear how to check its authenticity
    val fromX5u = (for (x5u <- jws.x5u) yield downloadX5c(x5u)).toSeq.flatten
    val fromx5c = (for (x5c <- jws.x5c; cert <- crypto.verifyX509CertChain(x5c)) yield JWK(cert)).toSeq
    val default = kids.defaultKids(jws.alg.name)
    val all = registered ++ fromJku ++ fromX5u ++ fromx5c ++ default

    val filteredByKid = jws.kid.map(id => all.filter(k => k.kid.isEmpty || k.kid.exists(_ == id)))
      .orElse(Option(all)).get
    val filteredByX5t = jws.x5t.map(hash => filteredByKid.filter(k => k.x5t.isEmpty || k.x5t.exists(_.deep == hash.deep)))
      .orElse(Option(filteredByKid)).get
    filteredByX5t.filter(_.kty == jws.alg.kty).filter(k => k.use.isEmpty || k.use.exists(_ == jws.alg.use)).distinct
  }

  def downloadJwk(url: URI): Seq[JWK] = Seq.empty
  def downloadX5c(url: URI): Seq[JWK] = Seq.empty

  def makeSignature(jws: JWS, data: Array[Byte])(implicit kids: KidsRegister, crypto: CryptoService): Array[Byte] =
    findJwkToSign(jws).map(crypto.sign(jws.alg.name, _, data))
      .orElse(throw new IllegalStateException("appropriate JWK not found")).get

  def findJwkToSign(jws: JWS)(implicit kids: KidsRegister, crypto: CryptoService): Option[JWK] =
    jws.kid.flatMap(k => kids.get(k)).orElse(kids.defaultKids(jws.alg.name)
      .filter(_.kty == jws.alg.kty)
      .filter(k => k.use.isEmpty || k.use.exists(_ == jws.alg.use))
      .filter{
      case _: RsaPrivateKey => true
      case _: EcPrivateKey => true
      case _: SymmetricKey => true
      case _ => false}
      .find(k => k.key_ops.isEmpty || k.key_ops.exists(_.contains(KeyOps.sign))))

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
