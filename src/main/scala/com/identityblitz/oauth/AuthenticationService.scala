package com.identityblitz.oauth

trait AuthenticationService {

  /**
   * Authenticates the request got from the client. The method is intended for a generic case
   * when request type is unknown.
   * @param req - OAuth request
   * @return - client corresponding to the passed credentials or error.
   */
  def authenticate(req: ORequests#OReq): Either[OAuthException, Client]

}
