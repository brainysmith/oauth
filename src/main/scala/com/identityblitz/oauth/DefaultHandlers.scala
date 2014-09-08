package com.identityblitz.oauth

trait DefaultHandlers extends Handlers {

  val responseTypeHandlers: Map[Set[String], PartialFunction[OReq, OResp]] = Map()

  val grantTypeHandlers: Map[String, PartialFunction[OReq, OResp]] = Map()

}
