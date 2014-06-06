package com.identityblitz.oauth

trait Client {

  val id: String

}

object Client {

  def byId(id: String): Option[Client] = Map.empty.get(id)

}
