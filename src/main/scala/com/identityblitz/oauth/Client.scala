package com.identityblitz.oauth

trait Client {

  val id: String

  /*
  Authenticates the request got from the client
  */
  def authenticate(req: OReq): Unit

}

object Client {

  def byId(id: String): Option[Client] = Map.empty.get(id)

}
