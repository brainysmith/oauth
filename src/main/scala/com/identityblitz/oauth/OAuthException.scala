package com.identityblitz.oauth

import java.net.URI

case class OAuthException(error: String,
                          errorDescription: Option[String],
                          errorUri: Option[URI],
                          state: Option[String]) extends Exception(errorDescription.getOrElse("")) {

  def this(error: String,
           errorDescription: String,
           state: Option[String] = None) = this(error, Some(errorDescription), None, state)

}

object OAuthException {
  def apply(error: String, errorDescription: String, state: Option[String] = None) = new OAuthException(error, errorDescription, state)
}

trait OAuthErrors extends OResponses {

  val invalid_request = error("invalid_request", """The request is missing a required parameter, includes
                                                  an invalid parameter value, includes a parameter more than
                                                  once, or is otherwise malformed.""")

  val unauthorized_client = error("unauthorized_client", """The client is not authorized to request an authorization
                                                          code using this method.""")

  val access_denied = error("access_denied", "The resource owner or authorization server denied the request.")

  val unsupported_response_type = error("unsupported_response_type",
    "The authorization server does not support obtaining an access token using this method.")

  val invalid_scope = error("invalid_scope", "The requested scope is invalid, unknown, or malformed.")

  val server_error = error("server_error", """The authorization server encountered an unexpected
                                            condition that prevented it from fulfilling the request.
                                            (This error code is needed because a 500 Internal Server
                                            Error HTTP status code cannot be returned to the client
                                            via an HTTP redirect.)""")

  val temporarily_unavailable = error("temporarily_unavailable", """The authorization server is currently unable
                                                                  to handle the request due to a temporary overloading
                                                                  or maintenance of the server.  (This error code is
                                                                  needed because a 503 Service Unavailable HTTP status
                                                                  to the client via an HTTP redirect.)""")

  val invalid_client = error("invalid_client", """Client authentication failed (e.g., unknown client, no client
                                                authentication included, or unsupported authentication method).
                                                The authorization server MAY return an HTTP 401 (Unauthorized) status
                                                code to indicate which HTTP authentication schemes are supported.
                                                If the client attempted to authenticate via the "Authorization" request
                                                header field, the authorization server MUST respond with an HTTP 401
                                                (Unauthorized) status code and include the "WWW-Authenticate" response
                                                header field matching the authentication scheme used by the client.""")

  val invalid_grant = error("invalid_grant", """The provided authorization grant (e.g., authorization
                                              code, resource owner credentials) or refresh token is
                                              invalid, expired, revoked, does not match the redirection
                                              URI used in the authorization request, or was issued to
                                              another client.""")

  val unsupported_grant_type = error("unsupported_grant_type", """The authorization grant type is not supported by the
                                                                authorization server.""")

  private def error(err: String, desc: String) = (r: String) => (req: OReq) => {
    writeToLog("[" + req + "]: " + r)
    ErrorResp(err, desc, req.param("state"))
  }

  protected def writeToLog(msg: String) = System.out.println(msg)
}
