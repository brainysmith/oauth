package com.identityblitz.jwt

import org.joda.time.DateTime
import java.net.URI
import scala.collection.mutable
import com.identityblitz.utils.json.{JVal, Json, JObj}
import org.apache.commons.codec.binary.Base64

/**
 * The JSON Web Token (JWT) toolkit is to work with JWT token. The current implementation is based on
 * the JSON Web Token (JWT) draft of version 20 (http://tools.ietf.org/html/draft-ietf-oauth-json-web-token-20).
 */

trait JwtTools {
  object AlgorithmType extends Enumeration {
    type AlgorithmType = Value
    val SIGNING, ENCRYPTION = Value
  }

  /**
   * This trait represents algorithm being used to encrypt or sign an JWT.
   */

  trait Algorithm {

    def apply(header: JObj, token: String): JWT[JWS, JWE]

  }

  /**
   * This trait represents header of a JWT token.
   */
  trait Header {

    val alg: Algorithm

    /**
     * JWT type
     */
    val typ: Option[String]

    /**
     * Content type
     */
    val cty: Option[String]

    def serialize(cs: ClaimsSet): String

  }

  trait JWS extends Header

  trait JWE extends Header

  trait ClaimsSet

  trait JWT[+S <: JWS, +E <: JWE] {

    val asJWS: Option[S]

    val asJWE: Option[E]

    val claimSet: ClaimsSet

    def serialize: String

  }
}

abstract class AlgorithmsKit extends JwtTools {
  thisKit =>

  import AlgorithmType._

  private val aMap: mutable.Map[String, Algorithm] = new mutable.HashMap

  def byName(name: String) = aMap(name)

  protected def Algorithm[S <: JWS, E <: JWE](name: String,
                                              algType: AlgorithmType,
                                              ds: (JObj, String) => JWT[S, E]): Algorithm =
    new AlgorithmItem(name, algType, ds)

  protected class AlgorithmItem[+S <: JWS, +E <: JWE](val name: String,
                                                    val algType: AlgorithmType,
                                                    val ds: (JObj, String) => JWT[S, E]) extends Algorithm {
    thisKit.aMap(name) = this

    def apply(header: JObj, token: String): JWT[JWS, JWE] = ds(header, token)
  }
}

trait JwtToolkit extends AlgorithmsKit with JwtTools {
  self =>

  val none = Algorithm("none", AlgorithmType.SIGNING, (hdr, tkn) => {
    new JWTImpl(new JWSNone(hdr))
  })

  private sealed class JWSNone(private val values: JObj) extends JWS {
    val alg: Algorithm = none
    val typ: Option[String] = values("typ").asOpt[String]
    val cty: Option[String] = values("cty").asOpt[String]

    def serialize(cs: ClaimsSet): String = ???
  }

  protected sealed class JWTImpl(private val header: JWS) extends JWT[JWS, JWE]{

    val asJWS: Option[JWS] = Option(header)

    val asJWE: Option[JWE] = None

    val claimSet: ClaimsSet = new ClaimsSet {}

    def serialize: String = header.serialize(claimSet)

  }

  object JWT {

    def apply(strJWT: String): JWT[JWS, JWE] = {
      val header = JVal.parseStr(new String(Base64.decodeBase64(strJWT.takeWhile(_ != '.')), "UTF-8")).as[JObj]
      self.byName(header("alg").as[String])(header, strJWT)
    }

  }







  /**
   * This class represents IntDate type of JSON Web Token. The type contain the number of seconds from 1970-01-01T0:0:OZ UTC
   * until the specified UTC date/time.
   */
  sealed case class IntDate(value: Int) {

    if (value < 0) throw new IllegalArgumentException("The number of second from epoch must be non negative.")

    def before(d: IntDate): Boolean = d.value < value

    def after(d: IntDate): Boolean = d.value > value

  }

  object IntDate {

    def now: IntDate = IntDate((new DateTime().getMillis / 1000).toInt)

  }



  /**
   * This class represents StringOrUri type of JSON Web token
   */
  sealed trait StringOrUri {

    def string: Option[String]

    def uri: Option[URI]

  }

  object StringOrUri {

    def apply(str: String) = if(str.indexOf(':') == -1) new StringVersion(str) else new UriVersion(new URI(str))

  }

  private sealed case class StringVersion(private val content: String) extends StringOrUri {
    def string: Option[String] = Option(content)

    def uri: Option[URI] = None
  }

  private sealed case class UriVersion(private val content: URI) extends StringOrUri{
    def string: Option[String] = None

    def uri: Option[URI] = Option(content)
  }

}
