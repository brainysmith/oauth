package com.identityblitz.oauth

import java.net.{URISyntaxException, URI}
import scala.util.Try

trait ORequests {

  val clientStore: ClientStore

  val authnService: AuthenticationService

  trait OReq {

    def apply(name: String): String

    def get(name: String): Option[String]

  }

  /**
   * An external request to be used in an external module which is used to interact to an end-user.
   * For example to ask an end-user to log in or to consent to the requested permissions.
   */
  trait ExtReq extends OReq

  trait AuthzReq extends OReq {

    /*
    An opaque value used by the client to maintain
    state between the request and callback.
    */
    val state: Option[String] = get("state")

    val responseType: Set[String] = apply("response_type").split(" ").toSet

    if(!responseType.subsetOf(supResponseTypes))
      throw new OAuthException("invalid_request", "Found unsupported response type", state)

    /*
    The unique string representing the registration
    information provided by the client.
    */
    val clientId: Client = clientStore byId apply("client_id") getOrElse {
      throw new OAuthException("invalid_client", "Unknown client", state)
    }

    clientId.authenticate(this) match {
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
    val redirectUri: Option[URI] = get("redirect_uri").flatMap(u => Try(new URI(u)).recoverWith {
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
    val scope: Option[Set[String]] = get("scope").map(s => s.split(" ").toSet)

    private val supResponseTypes: Set[String] = Set("code", "token") ++ extResponseTypes

    /*
    Returns extended supported response types. This method is intended to be
    overridden in descendants.
    */
    protected def extResponseTypes: Set[String] = Set()

  }

  trait AcsTknReq extends OReq {

    val grantType: String = apply("grant_type")

    val clientId: Client = get("client_id")
      .fold(authnService.authenticate(this))(clientStore.byId(_)
      .toRight(new OAuthException("invalid_client", "Unknown client")).right.map(_.authenticate(this)).joinRight
      ) match {
      case Right(c) => c
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
    val code: String = apply("code")

    /*
    The redirect URI, if the "redirect_uri" parameter was included in the
    authorization request as described in Section 4.1.1, and their values MUST be identical.
    */
    val redirectUri: Option[URI] = get("redirect_uri").flatMap(u => Try(new URI(u)).recoverWith {
      case e: URISyntaxException => throw new OAuthException("invalid_request", "Wrong redirect uri")
      case o => throw o
    }.toOption)

  }

  /**
   * The access token request used in resource owner password credential grant
   */
  trait ResOwnerPaswdCredAcsTknReq extends AcsTknReq {

    /*
    The resource owner username.
    */
    val username: String = apply("username")

    /*
    The resource owner password.
    */
    val password: String = apply("password")

    /*
    The scope of the access request as described by Section 3.3.
    */
    val scope: Option[Set[String]] = get("scope").map(s => s.split(" ").toSet)

  }

  /**
   * The access token request used in client credential grant
   */
  trait ClientCredAcsTkn extends AcsTknReq {

    /*
    The scope of the access request as described by Section 3.3.
    */
    val scope: Option[Set[String]] = get("scope").map(s => s.split(" ").toSet)

  }

}