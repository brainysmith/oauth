package com.identityblitz.jwt

import scala.collection.mutable
import com.identityblitz.utils.json._
import org.apache.commons.codec.binary.Base64
import org.apache.commons.lang.StringUtils

/**
 * The JSON Web Token (JWT) toolkit is to work with JWT token. The current implementation is based on
 * the JSON Web Token (JWT) draft of version 20 (http://tools.ietf.org/html/draft-ietf-oauth-json-web-token-20).
 */
trait JwtToolkit {
  self: AlgorithmsKit =>

  /**
   * This trait represents algorithm being used to encrypt or sign an JWT.
   */
  trait Algorithm[H <: Header[H]] {

    val name: String

    /**
     * Deserializes a string representation of the JWT token and do validating of signature or MAC for JWS tokens and
     * decryption of JWT payload for JWE tokens.
     * @param header - header of a JWT token
     * @param token - string representation of the token
     * @return - JWT instance
     */
    def apply(header: JObj, token: String): JWT[H]

    /**
     * Serializes an jwt instance into its string representation and do signing or MACing for JWS tokens and
     * encryption of JWT payload for JWE token.
     * @param jwt - JWT instance
     * @return - string representation of the token
     */
    def apply(jwt :JWT[H]): String

    /**
     * Builds JWT header corresponding to the current algorithm.
     * @param header - JSON representation of header without 'alg' header parameter
     * @param cs - claims set (TODO: think of necessity of passing in a claim set)
     * @return - header
     */
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

  /**
   * The name kit of registered header parameters or claims names from JWT specification.
   */
  trait BaseNameKit extends NameKit {

    def checkIfBlank(name: String)(value: String) = {
      if(StringUtils.isBlank(value))
        throw new IllegalArgumentException(name + " parameter is blank")
    }

    def checkIfEmptyArray(name: String)(value: Array[_]) = {
      if(value.isEmpty)
        throw new IllegalArgumentException(value + " list is empty")
    }

    /**
     * Common header parameters
     */
    val typ = Name[String]("typ", checkIfBlank("typ"))
    val cty = Name[String]("cty", checkIfBlank("cty"))

    /**
     * Registered claims
     */
    val iss = Name[StringOrUri]("iss", (v) => {})
    val sub = Name[StringOrUri]("sub", (v) => {})
    val aud = Name[Array[StringOrUri]]("aud", checkIfEmptyArray("aud"))
    val exp = Name[IntDate]("exp", (v) => {})
    val nbf = Name[IntDate]("nbf", (v) => {})
    val iat = Name[IntDate]("iat", (v) => {})
    val jti = Name[String]("jti", checkIfBlank("jit"))

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
   * The Claims set representation.
   * @param values
   */
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

  /**
   * The base simple implementation of JWT token.
   * @param header - header
   * @param cs - claims set
   * @tparam H - type of the header
   */
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

  /**
   * Returns a builder of JWT token. The example of using:
   * {{{
   * import BaseNameKit.
   *
   * val myJwt = builder
   * .alg(none)
   * .header (typ % "JWT",
   *         "http://mydomain.com/type" % "idToken")
   * .cs (iss % StringOrUri("john"),
   *     "http://mydomain.com/trusted" % true)
   * .build
   * }}}
   * The header parameters and claims names can be passed in as string or as predefined name objects.
   * If predefined name objects used then type checking and validating is performed.
   * In the [[com.identityblitz.jwt.JwtToolkit]] the only base names from JWT specification are defined.
   * To use them import [[com.identityblitz.jwt.JwtToolkit.BaseNameKit]] object.
   * @return - JWT token builder
   */
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

  object JWT {

    /**
     * Deserializes a string representation of the JWT token.
     * @param strJWT - string representation of the JWT token
     * @return - JWT instance
     */
    def apply(strJWT: String): JWT[_ <: Header[_]] = {
      val header = JVal.parseStr(base64ToUtf8String(strJWT.takeWhile(_ != '.'))).as[JObj]
      header("alg").asOpt[String].map(n => self.optByName(n).map(_(header, strJWT))
        .orElse(throw new IllegalStateException("unknown algorithm ["+ n +"]")))
        .orElse(throw new IllegalStateException("not found mandatory header parameter [alg]")).get.get
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
abstract class AlgorithmsKit extends JwtToolkit {
  thisKit =>

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
   * @param ds - function to deserialize an JWT token from its string representation. Also it must do all necessary
   *           validations such as MAC validation, structure validation and so on.
   * @param sz - function to serialize an JWT token to its string representation.
   * @tparam H - type of the header.
   * @return - newly created algorithm.
   */
  protected def Algorithm[H <: Header[H]](name: String,
                                          ds: (JObj, String) => JWT[H],
                                          sz: (JWT[H]) => String,
                                          mkh: (JObj) => H): Algorithm[H] =
    new AlgorithmItem[H](name, ds, sz, mkh)

  private class AlgorithmItem[H <: Header[H]](val name: String,
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

