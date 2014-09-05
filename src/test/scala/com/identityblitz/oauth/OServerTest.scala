package com.identityblitz.oauth

import com.identityblitz.utils.json.{Json, JObj}
import org.apache.commons.codec.net.URLCodec
import org.scalatest.{Matchers, FlatSpec}

class OServerTest extends FlatSpec with Matchers {

  case class StringWrapper(value: String)

  object TestOServer extends OServer[StringWrapper, String] with DefaultHandlers {

    override def goToUserInteraction(resp: InteractionResp): String = ???

    override val clientStore: ClientStore = new ClientStore {}

    case class SimpleAuthzReq(private val store: Map[String, String]) extends AuthzReq {
      override def param(name: String): Option[String] = store.get(name)
      override def iSerialize: JObj = Json.toJson(store).as[JObj]
    }

    implicit object StringToOReq extends ZReqConverter[StringWrapper] {

      override def convert(in: StringWrapper): AuthzReq = SimpleAuthzReq(in.value.substring(1).split('&')
        .map(_.split('=')).map(a => (a(0), new URLCodec("US-ASCII").decode(a(1)))).toMap)
      override def convert(in: JObj): AuthzReq = ???
    }

    implicit object ORespToString extends RespConverter[String] {
      override def convert(res: OResp): String = res.asQueryString
    }

  }

  behavior of "OServer"

  it should "process authorization code request" in {
    val request = "?response_type=code&client_id=s6BhdRkqt3&state=xyz&redirect_uri=https%3A%2F%2Fclient%2Eexample%2Ecom%2Fcb"

    System.out.println(TestOServer.ea(StringWrapper(request)))

  }

}
