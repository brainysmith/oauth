package com.identityblitz.oauth

trait DefaultHandlers extends Handlers {

  val responseTypeHandlers: Map[Set[String], Handler] = Map()

  val grantTypeHandlers: Map[String, Handler] = Map()

}
