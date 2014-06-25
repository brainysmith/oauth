package com.identityblitz.oauth

trait Client {

  val id: String

  /**
   * Authenticates the request got from the client. The method is intended for a generic case
   * when request type is unknown.
   * @param req - OAuth request
   * @return - either an authenticated client or exception
   */
  def authenticate(req: ORequests#OReq): Either[OAuthException, Client]

}

trait ClientStore {

  def byId(id: String): Option[Client] = Map.empty.get(id)

}
