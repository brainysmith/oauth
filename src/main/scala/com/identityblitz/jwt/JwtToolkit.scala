package com.identityblitz.jwt

import org.joda.time.DateTime
import java.net.URI

/**
 * The JSON Web Token (JWT) toolkit is to work with JWT token. The current implementation is based on
 * the JSON Web Token (JWT) draft of version 20 (http://tools.ietf.org/html/draft-ietf-oauth-json-web-token-20).
 */
trait JwtToolkit {

  object AlgorithmType extends Enumeration {
    type AlgorithmType = Value
    val SIGNING, ENCRYPTION = Value
  }
  import AlgorithmType._

  /**
   * This trait represents algorithm being used to encrypt or sign an JWT.
   */
  sealed class Algorithm(val name: String, val algType: AlgorithmType, ds: (String) => JWT[_, _])

  trait AlgorithmsKit

  implicit object AlgorithmsKit extends AlgorithmsKit {
    case object none extends Algorithm("none", SIGNING, s => {
      new PlainJWT(new PlainJWS())
    })
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
    
    def deserialize(strJWT: String): JWT[_, _]

  }

  trait JWS extends Header

  trait JWE extends Header

  trait ClaimsSet

  trait JWT[S <: JWS, E <: JWE] {

    val asJWS: Option[S]

    val asJWE: Option[E]

    val claimSet: ClaimsSet

    def serialize: String

  }

  sealed class PlainJWS() extends JWS {
    val alg: Algorithm = AlgorithmsKit.none
    val typ: Option[String] = None
    val cty: Option[String] = None

    def serialize(cs: ClaimsSet): String = ???

    def deserialize(strJWT: String): JWT[_, _] = ???
  }

  sealed class PlainJWT(private val header: JWS) extends JWT[JWS, JWE]{

    val asJWS: Option[JWS] = Option(header)

    val asJWE: Option[JWE] = None

    val claimSet: ClaimsSet = new ClaimsSet {}

    def serialize: String = header.serialize(claimSet)

  }

  object JWT {

    def apply(strJWT: String)(implicit algKit: AlgorithmsKit): JWT[JWS, JWE] = ???

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
