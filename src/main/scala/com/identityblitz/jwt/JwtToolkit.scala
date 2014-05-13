package com.identityblitz.jwt

import org.joda.time.DateTime
import java.net.URI
import scala.collection.mutable
import com.identityblitz.utils.json._
import org.apache.commons.codec.binary.Base64
import com.identityblitz.utils.json.JSuccess

/**
 * The JSON Web Token (JWT) toolkit is to work with JWT token. The current implementation is based on
 * the JSON Web Token (JWT) draft of version 20 (http://tools.ietf.org/html/draft-ietf-oauth-json-web-token-20).
 */

/**
 * This class represents IntDate type of JSON Web Token. The type contain the number of seconds from 1970-01-01T0:0:OZ UTC
 * until the specified UTC date/time.
 */
sealed case class IntDate(value: Int) {

  if (value < 0) throw new IllegalArgumentException("The number of second from epoch must be non negative.")

  def before(d: IntDate): Boolean = d.value < value

  def after(d: IntDate): Boolean = d.value > value

  override def toString: String = value.toString
}

object IntDate {

  implicit object JStrReader extends JReader[IntDate] {
    def read(v: JVal): JResult[IntDate] = v match {
      case o: JNum => JSuccess(IntDate(o.as[Int]))
      case _ => JError("json.error.expected.number")
    }
  }

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

  implicit object JStrReader extends JReader[StringOrUri] {
    def read(v: JVal): JResult[StringOrUri] = v match {
      case o: JStr => JSuccess(StringOrUri(o))
      case _ => JError("json.error.expected.string")
    }
  }

}

private sealed case class StringVersion(private val content: String) extends StringOrUri {
  def string: Option[String] = Option(content)

  def uri: Option[URI] = None

  override def toString: String = content.toString
}

private sealed case class UriVersion(private val content: URI) extends StringOrUri{
  def string: Option[String] = None

  def uri: Option[URI] = Option(content)

  override def toString: String = content.toString
}

trait JwtTools {

  /**
   * Enumeration of algorithm types
   */
  object AlgorithmType extends Enumeration {
    type AlgorithmType = Value
    val SIGNING, ENCRYPTION = Value
  }

  /**
   * This trait represents algorithm being used to encrypt or sign an JWT.
   */
  trait Algorithm {

    val typ: AlgorithmType.Value

    /**
     * Deserializes a string representation of the JWT token.
     * @param header - header of a JWT token
     * @param token - string representation of the token
     * @return - JWT instance
     */
    def apply(header: JObj, token: String): JWT[JWS, JWE]

    /**
     * Serializes an jwt instance into its string representation.
     * @param jwt - JWT instance
     * @return - string representation of the token
     */
    def apply(jwt :JWT[JWS, JWE]): String

    def apply(header: JObj, cs: JObj): JWT[JWS, JWE]

  }

  /**
   * This trait represents general header of a JWT token.
   */
  trait Header {

    val alg: Algorithm
    val typ: Option[String]
    val cty: Option[String]

    def apply(name: String): JVal
    def get(name: String): Option[JVal]

    def names:Set[String]

    def asJson: JObj

  }

  /**
   * This traits is represents JWS header.
   */
  class JWS(val alg: Algorithm, values: JObj) extends Header {
    val typ: Option[String] = values("typ").asOpt[String]
    val cty: Option[String] = values("cty").asOpt[String]

    def apply(name: String): JVal = values(name)
    def get(name: String): Option[JVal] = values(name).asOpt[JVal]

    def names:Set[String] = values.fields

    def asJson: JObj = ???
  }

  /**
   * This traits is represents JWS header.
   */
  trait JWE extends Header

  class ClaimsSet(val values: JObj) {

    val iss: Option[StringOrUri] = values("iss").asOpt[StringOrUri]
    val sub: Option[StringOrUri] = values("sub").asOpt[StringOrUri]
    val aud: Option[Seq[StringOrUri]] = values("aud").asOpt[Seq[StringOrUri]]
    val exp: Option[IntDate] = values("exp").asOpt[IntDate]
    val iat: Option[IntDate] = values("iat").asOpt[IntDate]
    val jti: Option[String] = values("jti").asOpt[String]

    def apply(name: String): JVal = values.apply(name)
    def get(name: String): Option[JVal] = values.asOpt[JVal]

    def names:Set[String] = values.fields

  }

  /**
   * Represents JSON Web token instance.
   * @tparam S - type of JWS header
   * @tparam E - type of JWE header
   */
  trait JWT[+S <: JWS, +E <: JWE] {

    val asJWS: Option[S]

    val asJWE: Option[E]

    val claimSet: ClaimsSet

    def asJson: JObj
  }

  def base64ToUtf8String(base64: String) = new String(Base64.decodeBase64(base64), "UTF-8")

  /**
   * Builder types.
   */

  sealed class HdrParam(val name: String, val value: JVal)

  class StringHdrParamMapper(name: String) {
    def -> (value: String): HdrParam = new HdrParam(name, JStr(value))
    def -> (value: StringOrUri): HdrParam = new HdrParam(name, JStr(value.toString))
    def -> (value: Int): HdrParam = new HdrParam(name, JNum(value))
    def -> (value: IntDate): HdrParam = new HdrParam(name, JNum(value.value))
  }

  sealed class Claim(val name: String, val value: JVal)

  class StringClaimMapper(name: String) {
    def -> (value: String): Claim = new Claim(name, JStr(value))
    def -> (value: StringOrUri): Claim = new Claim(name, JStr(value.toString))
    def -> (value: Int): Claim = new Claim(name, JNum(value))
    def -> (value: IntDate): Claim = new Claim(name, JNum(value.value))
  }

  def builder = new {
    def alg(a: Algorithm) = new {
      val _alg = a
      def header(hs: HdrParam*) = new {
        val header = JObj(hs.map(h => (h.name, h.value)))
        def cs(s: Claim*) = new {
          def build:JWT[JWS, JWE] = a.apply(header, JObj(s.map(c => (c.name, c.value))))
        }
      }
    }
  }

}

/**
 * Base class to construct a kit of algorithms to sign and encrypt JWT tokens.
 */
abstract class AlgorithmsKit extends JwtTools {
  thisKit =>

  import AlgorithmType._

  private val aMap: mutable.Map[String, Algorithm] = new mutable.HashMap

  /**
   * Returns an algorithm by its name. If there is no algorithm associated with the key null is returned.
   * @param name - algorithm's name
   * @return - algorithm instance or null
   */
  def byName(name: String) = aMap(name)

  /**
   * Returns an Option of algorithm by its name. If there is no algorithm associated with the key None is returned.
   * @param name - algorithm's name
   * @return - Some of algorithm instance or None
   */
  def optByName(name: String) = aMap.get(name)

  /**
   * The method to describe an algorithm being used to sign or encrypt JWT token.
   * @param name - algorithm name. It is a name used in 'alg' header parameter to describe cryptographic transformations
   *             that had been applied to the token.
   * @param algType - algorithm type: SIGNING or ENCRYPTING
   * @param ds - function to deserialize an JWT token from its string representation. Also it must do all necessary
   *           validations such as MAC validation, structure validation and so on.
   * @param sz - function to serialize an JWT token to its string representation.
   * @tparam S - type of JWS header.
   * @tparam E - type of JWE header.
   * @return - newly created algorithm.
   */
  protected def Algorithm[S <: JWS, E <: JWE](name: String,
                                              algType: AlgorithmType,
                                              ds: (JObj, String) => JWT[S, E],
                                              sz: (JWT[JWS, JWE]) => String): Algorithm =
    new AlgorithmItem(name, algType, ds, sz)

  private class AlgorithmItem[+S <: JWS, +E <: JWE](val name: String,
                                                    val typ: AlgorithmType,
                                                    val ds: (JObj, String) => JWT[S, E],
                                                    val sz: (JWT[JWS, JWE]) => String) extends Algorithm {
    thisKit.aMap(name) = this

    def apply(header: JObj, token: String): JWT[JWS, JWE] = ds(header, token)

    def apply(jwt: JWT[JWS, JWE]): String = sz(jwt)

    def apply(header: JObj, cs: JObj): JWT[JWS, JWE] = ???
  }

}

trait JwtToolkit extends AlgorithmsKit with JwtTools {
  self =>

  import AlgorithmType._

  val none = Algorithm("none", SIGNING,
    (hdr, tkn) => {
      if(!tkn.endsWith("."))
        throw new IllegalStateException("token has wrong format")

      tkn.split("\\.").toList match {
        case _ :: cs :: Nil =>
          new JWTImpl(new JWSNone(hdr), JVal.parseStr(base64ToUtf8String(cs)).as[JObj])
        case _ => throw new IllegalStateException("token has wrong format")
      }},
    j => null)

  private sealed class JWSNone(private val values: JObj) extends JWS(none, values)

  protected sealed class JWTImpl(private val header: JWS, cs: JObj) extends JWT[JWS, JWE]{

    val asJWS: Option[JWS] = Option(header)

    val asJWE: Option[JWE] = None

    val claimSet: ClaimsSet = new ClaimsSet(cs)

    def asJson: JObj = ???
  }

  object JWT {

    def apply(strJWT: String): JWT[JWS, JWE] = {
      val header = JVal.parseStr(base64ToUtf8String(strJWT.takeWhile(_ != '.'))).as[JObj]
      header("alg").asOpt[String].map(n => self.optByName(n).map(_(header, strJWT))
        .orElse(throw new IllegalStateException("unknown algorithm ["+ n +"]")))
        .orElse(throw new IllegalStateException("not found mandatory header parameter [alg]")).get.get
    }

  }

}
