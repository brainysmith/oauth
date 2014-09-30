package com.identityblitz.oauth

trait ClientStore extends ORequests {

  trait Client {
    val id: String

    val name: String

    /**
     * Authenticates the request got from the client. The method is intended for a generic case
    * when request type is unknown.
    * @param req - OAuth request
    * @return - either an authenticated client or exception
    */
    def authenticate(req: OReq): Either[OAuthException, Client]

  }

  def byClientId(id: String): Option[Client]

}
