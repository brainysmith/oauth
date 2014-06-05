package com.identityblitz.oauth

import java.net.URI

trait OReq {

  def apply(name: String): String

  def get(name: String): Option[String]

}

trait AuthzReq extends OReq {

  val responseType: String = apply("response_type")

  val clientId: String = apply("client_id")

  val redirectUri: Option[URI] = get("redirect_uri").map(new URI(_))

  val scope: Option[Set[String]] = get("scope").map(s => s.split(" ").toSet)

  val state: Option[String] = get("state")

}