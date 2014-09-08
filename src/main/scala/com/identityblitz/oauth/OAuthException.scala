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

trait OAuthErrors extends OResponses {

  val unsupported_response_type = error("unsupported_response_type",
    "The authorization server does not support obtaining an access token using this method.")

  val server_error = error("server_error", """The authorization server encountered an unexpected
                                            condition that prevented it from fulfilling the request.
                                            (This error code is needed because a 500 Internal Server
                                            Error HTTP status code cannot be returned to the client
                                            via an HTTP redirect.)""")

  val invalid_grant = error("invalid_grant", """The provided authorization grant (e.g., authorization
                                              code, resource owner credentials) or refresh token is
                                              invalid, expired, revoked, does not match the redirection
                                              URI used in the authorization request, or was issued to
                                              another client.""")

  private def error(err: String, desc: String) = (r: String) => (req: OReq) => {
    writeToLog("[" + req + "]: " + r)
    ErrorResp(err, desc, req.param("state"))
  }

  protected def writeToLog(msg: String) = System.out.println(msg)
}
