package com.identityblitz.oauth

import java.net.{URISyntaxException, URI}
import com.identityblitz.json.{JVal, Json, JObj}
import org.apache.commons.codec.binary.Base64
import org.apache.commons.codec.net.URLCodec

import scala.annotation.implicitNotFound
import scala.util.Try

trait ORequests {
  self: ClientStore =>

  trait OReq {

    def param(name: String): Option[String]

    def names: Set[String]

  }

  /**
   * A trait of the OAuth 2.0 authorization request. It has all attributes described in OAuth 2.0 specification and
   * their corresponding extraction and validation logic. Runs authentication of the client associated with the request.
   * By default it supports two responses, code and token. The list of supported response types can be extended
   * by overriding the method <i>AuthzReq.extResponseTypes</i> returning the additional supported response types.
   */
  trait AuthzReq extends OReq {

    /*
    An opaque value used by the client to maintain
    state between the request and callback.
    */
    val state: Option[String] = param("state")

    val responseType: Set[String] = param("response_type").map(_.split(" ").toSet)
      .getOrElse(throw new OAuthException("invalid_request", "Response type not found", state))

    private val supResponseTypes: Set[String] = Set("code", "token") ++ extResponseTypes

    if(!responseType.subsetOf(supResponseTypes))
      throw new OAuthException("invalid_request", "Found unsupported response type", state)

    /*
    The unique string representing the registration
    information provided by the client.
    */
    val clientId: Client = param("client_id")
      .map(id => byClientId(id).getOrElse(throw new OAuthException("invalid_client", "Unknown client " + id + ".", state)))
      .getOrElse(throw new OAuthException("invalid_client", "Undefined client.", state))

    def authenticate = clientId.authenticate(this) match {
      case Right(_) =>
      case Left(e) => throw e
    }

    /*
    The redirection endpoint URI MUST be an absolute URI as defined by
    [RFC3986] Section 4.3.  The endpoint URI MAY include an
    "application/x-www-form-urlencoded" formatted (per Appendix B) query
    component ([RFC3986] Section 3.4), which MUST be retained when adding
    additional query parameters.  The endpoint URI MUST NOT include a
    fragment component.
    */
    val redirectUri: Option[URI] = param("redirect_uri").flatMap(u => Try(new URI(u)).recoverWith {
      case e: URISyntaxException => throw new OAuthException("invalid_request", "Wrong redirect uri", state)
      case o => throw o
    }.toOption)

    /*
    The value of the scope parameter is expressed as a list of space-
    delimited, case-sensitive strings.  The strings are defined by the
    authorization server.  If the value contains multiple space-delimited
    strings, their order does not matter, and each string adds an
    additional access range to the requested scope.
    */
    val scope: Option[Set[String]] = param("scope").map(s => s.split(" ").toSet)

    /*
    Returns extended supported response types. This method is intended to be
    overridden in descendants if additional response types required.
    */
    protected def extResponseTypes: Set[String] = Set()

    final def serialize: String = Base64.encodeBase64URLSafeString(clientId.buildClientSecret(this)
      .fold(iSerialize)(cs => iSerialize + ("client_secret" -> cs)).toJson.getBytes("UTF-8"))

    final def asQueryString = "?" +
      names.map(n => n + "=" + new URLCodec("US-ASCII").encode(param(n).getOrElse(""))).mkString("&") +
      clientId.buildClientSecret(this).fold("")("client_secret=" + new URLCodec("US-ASCII").encode(_))

    /**
     * Serializes this request to [[JObj]]. Intended to be overridden by the implementation.
     * @return
     */
    def iSerialize: JObj

  }

  object AuthzReq {
    def apply[Req](serialized: String)(implicit rc: ZReqConverter[Req]): AuthzReq = rc.convert(serialized)
  }

  trait AcsTknReq extends OReq {

    val grantType: String = param("grant_type")
      .getOrElse(throw new OAuthException("invalid_grant", "Undefined grant type"))

    val clientId: Client = param("client_id").flatMap(byClientId)
      .getOrElse(throw new OAuthException("invalid_client", "Unknown client"))

    clientId.authenticate(this) match {
      case Right(_) =>
      case Left(e) => throw e
    }

  }

  /**
   * The access token request used in authorization code grant.
   */
  trait CodeAcsTknReq extends AcsTknReq {

    if(grantType != "authorization_code")
      throw new OAuthException("invalid_grant", "authorization grant must be \"authorization_code\"")

    /*
    The authorization code received from the authorization server.
    */
    val code: String = param("code").getOrElse(throw new OAuthException("invalid_request", "Undefined code"))

    /*
    The redirect URI, if the "redirect_uri" parameter was included in the
    authorization request as described in Section 4.1.1, and their values MUST be identical.
    */
    val redirectUri: Option[URI] = param("redirect_uri").flatMap(u => Try(new URI(u)).recoverWith {
      case e: URISyntaxException => throw new OAuthException("invalid_request", "Wrong redirect uri")
      case o => throw o
    }.toOption)

  }

  /**
   * The access token request used in resource owner password credential grant
   */
  trait ResOwnerPaswdCredAcsTknReq extends AcsTknReq {

    if(grantType != "password")
      throw new OAuthException("invalid_grant", "authorization grant must be \"password\"")

    /*
    The resource owner username.
    */
    val username: String = param("username").getOrElse(throw new OAuthException("invalid_request", "Undefined username"))

    /*
    The resource owner password.
    */
    val password: String = param("password").getOrElse(throw new OAuthException("invalid_request", "Undefined password"))

    /*
    The scope of the access request as described by Section 3.3.
    */
    val scope: Option[Set[String]] = param("scope").map(s => s.split(" ").toSet)

  }

  /**
   * The access token request used in client credential grant
   */
  trait ClientCredAcsTknReq extends AcsTknReq {

    if(grantType != "client_credentials")
      throw new OAuthException("invalid_grant", "authorization grant must be \"client_credentials\"")

    /*
    The scope of the access request as described by Section 3.3.
    */
    val scope: Option[Set[String]] = param("scope").map(s => s.split(" ").toSet)

  }

  /**
   * Interaction request to go on to fulfill the authorization request, processing of which requires user interaction.
   */

  case class InteractionReq(authzReq: AuthzReq,
                            succeeded: Boolean,
                            exception: OAuthException) extends OReq {

    def param(name: String): Option[String] = name match {
      case "authz_req" => Some(authzReq.serialize)
      case "succeeded" => Some(succeeded.toString)
      case "exception" => Some(exception.toString) //hack
    }

    def names = Set("authz_req", "succeeded", "exception")

  }

  /**
   * Converters into authorization or access requests.
   */

  @implicitNotFound("No authorization request converter found for type ${Req}. Try to implement an implicit ZReqConverter.")
  trait ZReqConverter[Req] {
    def convert(in: Req): AuthzReq
    def convert(in: JObj): AuthzReq
    def convert(in: AuthzReqBuilder): AuthzReq

    /**
     * Deserializes an authorization request from string representation obtained from call to
     * the serialization method, <i>AuthzReq.serialization</i>.
     * @param in
     * @return
     */
    final def convert(in: String): AuthzReq = convert(JVal.parse(new String(Base64.decodeBase64(in), "UTF-8")).as[JObj])
  }

  @implicitNotFound("No access request converter found for type ${Req}. Try to implement an implicit AReqConverter.")
  trait AReqConverter[Req] {
    def convert(in: Req): AcsTknReq
  }

  /**
   * Request builders
   */

  def request(rt: responseType): AuthzReqBuilder = AuthzReqBuilder.empty(rt)

  /**
   * Response types
   */

  sealed class responseType(val name: String)

  object code extends responseType("code")

  object token extends responseType("token")

  trait Sender[RQ, RS] {
    def send(a: AuthzReq)
  }



  /**
   * Authorization request builder
   */

  trait AuthzReqBuilder {

    val responseTypes: Set[responseType]

    if(responseTypes == null || responseTypes.isEmpty)
      throw new IllegalArgumentException("Response types must be defined")

    val clientId: Option[String]
    val redirectUri: Option[URI]
    val scopes: Set[String]
    val extParams: Map[String, String]

    def and(rt: responseType): AuthzReqBuilder = AuthzReqBuilder.apply(responseTypes + rt)(this)

    def from(clt: String): AuthzReqBuilder = AuthzReqBuilderImpl(responseTypes, Some(clt), redirectUri, scopes, extParams)

    def redirectTo(redirect: URI): AuthzReqBuilder = AuthzReqBuilderImpl(responseTypes, clientId, Some(redirect), scopes, extParams)

    def withScope(scp: String): AuthzReqBuilder = AuthzReqBuilderImpl(responseTypes, clientId, redirectUri, scopes + scp, extParams)

    def withScopes(scp: Set[String]): AuthzReqBuilder = AuthzReqBuilderImpl(responseTypes, clientId, redirectUri, scopes ++ scp, extParams)

    def withExtParam(name: String, value: String): AuthzReqBuilder = AuthzReqBuilderImpl(responseTypes, clientId, redirectUri, scopes, extParams + (name -> value))

    def build[Req](implicit c: ZReqConverter[Req]): AuthzReq = c.convert(this)

    def send[Req, Resp](implicit c: ZReqConverter[Req], s: Sender[Req, Resp]) = s.send(c.convert(this))

  }

  private case class AuthzReqBuilderImpl(responseTypes: Set[responseType],
                                         clientId: Option[String] = None,
                                         redirectUri: Option[URI] = None,
                                         scopes: Set[String] = Set(),
                                         extParams: Map[String, String] = Map()) extends AuthzReqBuilder

  object AuthzReqBuilder {
    def apply(rts: Set[responseType])(rb: AuthzReqBuilder): AuthzReqBuilder = AuthzReqBuilderImpl(rts, rb.clientId, rb.redirectUri, rb.scopes)
    def empty(rt: responseType): AuthzReqBuilder = new AuthzReqBuilderImpl(Set(rt))
  }



}