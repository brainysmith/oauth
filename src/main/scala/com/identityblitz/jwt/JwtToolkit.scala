package com.identityblitz.jwt

import org.joda.time.DateTime
import java.net.URI
import scala.collection.mutable
import com.identityblitz.utils.json._
import org.apache.commons.codec.binary.Base64
import com.identityblitz.utils.json.JSuccess
import org.apache.commons.lang.StringUtils

/**
 * The JSON Web Token (JWT) toolkit is to work with JWT token. The current implementation is based on
 * the JSON Web Token (JWT) draft of version 20 (http://tools.ietf.org/html/draft-ietf-oauth-json-web-token-20).
 */

/**
 * This class represents IntDate type of JSON Web Token. The type contain the number of seconds from 1970-01-01T0:0:OZ UTC
 * until the specified UTC date/time.
 */
sealed case class IntDate(value: Int) {

  if (value <= 0) throw new IllegalArgumentException("The number of second from epoch must be non negative.")

  def before(d: IntDate): Boolean = d.value < value

  def after(d: IntDate): Boolean = d.value > value

  override def toString: String = value.toString
}

object IntDate {

  implicit object JIntDateReader extends JReader[IntDate] {
    def read(v: JVal): JResult[IntDate] = v match {
      case o: JNum => JSuccess(IntDate(o.as[Int]))
      case _ => JError("json.error.expected.number")
    }
  }

  implicit object JIntDateWriter extends JWriter[IntDate] {
    def write(o: IntDate): JVal = JNum(o.value)
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

  def apply(str: String) = {
    if(StringUtils.isBlank(str))
      throw new IllegalArgumentException("StringOrUri can not be blank.")
    if(str.indexOf(':') == -1) new StringVersion(str) else new UriVersion(new URI(str))
  }

  implicit object JStringOrUriReader extends JReader[StringOrUri] {
    def read(v: JVal): JResult[StringOrUri] = v match {
      case o: JStr => JSuccess(StringOrUri(o))
      case _ => JError("json.error.expected.string")
    }
  }

  implicit object JStringOrUriDateWriter extends JWriter[StringOrUri] {
    def write(o: StringOrUri): JVal = JStr(o.toString)
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
  trait Algorithm[H <: Header[H]] {

    val name: String

    val typ: AlgorithmType.Value

    /**
     * Deserializes a string representation of the JWT token.
     * @param header - header of a JWT token
     * @param token - string representation of the token
     * @return - JWT instance
     */
    def apply(header: JObj, token: String): JWT[H]

    /**
     * Serializes an jwt instance into its string representation.
     * @param jwt - JWT instance
     * @return - string representation of the token
     */
    def apply(jwt :JWT[H]): String

    def apply(header: JObj, cs: JObj): JWT[H]

  }

  /**
   * Description of a registered or predefined header parameter or claim.
   */
  trait Name[A] {
    val name: String
    val validator: (A) => Unit
    def % (a: A): (String, JVal)
  }

  abstract class NameKit {

    def Name[A](name: String, validator: (A) => Unit)(implicit writer: JWriter[A]): Name[A] = new NameImpl[A](name, validator)

    private class NameImpl[A](val name: String, val validator: (A) => Unit)(implicit writer: JWriter[A]) extends Name[A] {
      def % (a: A): (String, JVal) = {
        validator(a)
        (name, Json.toJson(a))
      }
    }

  }

  trait BaseNameKit extends NameKit {

    val checkIfBlank = (s: String) => {
      if(StringUtils.isBlank(s))
        throw new IllegalArgumentException("typ parameter is blank")
    }

    /**
     * Common header parameters
     */
    val typ = Name[String]("typ", checkIfBlank)
    val cty = Name[String]("cty", checkIfBlank)

    /**
     * Registered claims
     */
    val iss = Name[StringOrUri]("iss", (v) => {})
    val sub = Name[StringOrUri]("sub", (v) => {})
    val aud = Name[Array[StringOrUri]]("aud", (v) => {
      if(v.isEmpty)
        throw new IllegalArgumentException("Audience list is empty")
    })
    val exp = Name[IntDate]("exp", (v) => {})
    val nbf = Name[IntDate]("nbf", (v) => {})
    val iat = Name[IntDate]("iat", (v) => {})
    val jti = Name[String]("typ", checkIfBlank)

  }

  object BaseNameKit extends BaseNameKit

  implicit def JsonPairConverter(name: String): JsonPairConverter = new JsonPairConverter(name)

  class JsonPairConverter(name: String) {
    def % (value: String): (String, JVal) = (name, JStr(value))
    def % (value: StringOrUri): (String, JVal) = (name, JStr(value.toString))
    def % (value: Int): (String, JVal) = (name, JNum(value))
    def % (value: IntDate): (String, JVal) = (name, JNum(value.value))
    def % (value: Boolean): (String, JVal) = (name, JBool(value))
  }

  /**
   * This trait represents general header of a JWT token.
   */
  trait Header[H <: Header[H]] {

    val alg: Algorithm[H]
    val typ: Option[String]
    val cty: Option[String]

    def apply(name: String): JVal
    def get(name: String): Option[JVal]

    def names:Set[String]

    def asBase64: String

  }

  /**
   * This traits is represents JWS header.
   */
  class JWS(val alg: Algorithm[JWS], values: JObj) extends Header[JWS] {
    val typ: Option[String] = values(BaseNameKit.typ.name).asOpt[String]
    val cty: Option[String] = values(BaseNameKit.cty.name).asOpt[String]

    def apply(name: String): JVal = values(name)
    def get(name: String): Option[JVal] = values(name).asOpt[JVal]

    def names:Set[String] = values.fields

    def asBase64: String = utf8StringToBase64((values + ("alg" -> alg.toString)).toJson)

    override def toString: String = (values + ("alg" -> alg.toString)).toString
  }

  /**
   * This traits is represents JWS header.
   */
  trait JWE extends Header[JWE]

  class ClaimsSet(val values: JObj) {

    val iss: Option[StringOrUri] = values(BaseNameKit.iss.name).asOpt[StringOrUri]
    val sub: Option[StringOrUri] = values(BaseNameKit.sub.name).asOpt[StringOrUri]
    val aud: Option[Seq[StringOrUri]] = values(BaseNameKit.aud.name).asOpt[Seq[StringOrUri]]
    val exp: Option[IntDate] = values(BaseNameKit.exp.name).asOpt[IntDate]
    val nbf: Option[IntDate] = values(BaseNameKit.nbf.name).asOpt[IntDate]
    val iat: Option[IntDate] = values(BaseNameKit.iat.name).asOpt[IntDate]
    val jti: Option[String] = values(BaseNameKit.jti.name).asOpt[String]

    def apply(name: String): JVal = values.apply(name)
    def get(name: String): Option[JVal] = values.asOpt[JVal]

    def names:Set[String] = values.fields

    def asBase64: String = utf8StringToBase64(values.toJson)

    override def toString: String = values.toString
  }

  /**
   * Represents JSON Web token instance.
   * @tparam H - type of the header
   */
  trait JWT[H <: Header[H]] {

    val header: H

    val claimSet: ClaimsSet

    def asBase64: String
  }

  protected sealed class JWTBase[H <: Header[H]](val header: H, cs: JObj) extends JWT[H] {

    val claimSet: ClaimsSet = new ClaimsSet(cs)

    def asBase64: String = header.alg.apply(this)

    override def toString: String = header.toString + "." + cs.toString

  }

  /**
   * Builder types.
   */

  implicit val impl = scala.language.implicitConversions
  implicit val reflective = scala.language.reflectiveCalls
  implicit val postfix = scala.language.postfixOps

  def builder = new {
    def alg[H <: Header[H]](a: Algorithm[H]) = new {
      val _alg = a
      def header(hs: (String, JVal)*) = new {
        val header = JObj(hs)
        def cs(s: (String, JVal)*) = new {
          def build = a.apply(header, JObj(s))
        }
      }
    }
  }

  /**
   * Arbitrary tools
   */

  def base64ToUtf8String(base64: String) = new String(Base64.decodeBase64(base64), "UTF-8")

  def utf8StringToBase64(str: String) = Base64.encodeBase64URLSafeString(str.getBytes("UTF-8"))

}

/**
 * Base class to construct a kit of algorithms to sign and encrypt JWT tokens.
 */
abstract class AlgorithmsKit extends JwtTools {
  thisKit =>

  import AlgorithmType._

  private val aMap: mutable.Map[String, Algorithm[_ <: Header[_]]] = new mutable.HashMap

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
   * @tparam H - type of the header.
   * @return - newly created algorithm.
   */
  protected def Algorithm[H <: Header[H]](name: String,
                                          algType: AlgorithmType,
                                          ds: (JObj, String) => JWT[H],
                                          sz: (JWT[H]) => String,
                                          mkh: (JObj) => H): Algorithm[H] =
    new AlgorithmItem[H](name, algType, ds, sz, mkh)

  private class AlgorithmItem[H <: Header[H]](val name: String,
                                              val typ: AlgorithmType,
                                              val ds: (JObj, String) => JWT[H],
                                              val sz: (JWT[H]) => String,
                                              val mkh: (JObj) => H) extends Algorithm[H] {
    thisKit.aMap(name) = this

    def apply(header: JObj, token: String): JWT[H] = ds(header, token)

    def apply(jwt: JWT[H]): String = sz(jwt)

    def apply(header: JObj, cs: JObj): JWT[H] = new JWTBase[H](mkh(header), cs)

    override def toString: String = name
  }

}

trait JwtToolkit extends AlgorithmsKit with JwtTools {
  self =>

  import AlgorithmType._

  val none = Algorithm[JWS]("none", SIGNING,
    (hdr, tkn) => {
      if(!tkn.endsWith("."))
        throw new IllegalStateException("token has wrong format")
      tkn.split("\\.").toList match {
        case _ :: cs :: Nil =>
          new JWTBase[JWS](new JWSNone(hdr), JVal.parseStr(base64ToUtf8String(cs)).as[JObj])
        case _ => throw new IllegalStateException("token has wrong format")
      }},
    j => {
      j.header.asBase64 + "." + j.claimSet.asBase64 + "."
    },
    new JWSNone(_)
  )

  private sealed class JWSNone(private val values: JObj) extends JWS(none, values)

  object JWT {

    def apply(strJWT: String): JWT[_ <: Header[_]] = {
      val header = JVal.parseStr(base64ToUtf8String(strJWT.takeWhile(_ != '.'))).as[JObj]
      header("alg").asOpt[String].map(n => self.optByName(n).map(a => a.apply(header, strJWT))
        .orElse(throw new IllegalStateException("unknown algorithm ["+ n +"]")))
        .orElse(throw new IllegalStateException("not found mandatory header parameter [alg]")).get.get
    }

  }

}
