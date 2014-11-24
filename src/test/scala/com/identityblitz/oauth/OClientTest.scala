package com.identityblitz.oauth

import java.net.URI
import java.util.Date
import com.identityblitz.json.{Json, JObj}
import org.apache.commons.codec.net.URLCodec
import org.scalatest.{Matchers, FlatSpec}

class OClientTest extends FlatSpec with Matchers {

  case class StringWrapper(value: String)

  object TestOClient extends OClient[StringWrapper, String] with DefaultHandlers with DefaultClientStore {

    case class SimpleAuthzReq(private val store: Map[String, String]) extends AuthzReq {
      override def param(name: String): Option[String] = store.get(name)
      override def names: Set[String] = store.keys.toSet
      override def iSerialize: JObj = Json.toJson(store).as[JObj]
    }

    implicit object StringToOReq extends ZReqConverter[StringWrapper] {

      override def convert(in: StringWrapper): AuthzReq = SimpleAuthzReq(in.value.substring(1).split('&')
        .map(_.split('=')).map(a => (a(0), new URLCodec("US-ASCII").decode(a(1)))).toMap)

      override def convert(in: JObj): AuthzReq = ???

      override def convert(in: AuthzReqBuilder): AuthzReq = {
        SimpleAuthzReq(Map(
          "response_type" -> in.responseTypes.map(_.name).mkString(" "),
          "client_id" -> in.clientId.get,
          "redirect_uri" -> in.redirectUri.get.toString(),
          "scope" -> in.scopes.mkString(" ")
        ) ++ in.extParams)
      }
    }

    case class SimpleAuthzRes(private val store: Map[String, String]) extends AuthzResp {
      override def param(name: String): Option[String] = store.get(name)
    }

    implicit val zConverter = new ZRespConverter[StringWrapper] {
      override def convert(res: StringWrapper): OResp = {
        val parsed = res.value.substring(1).split('&')
          .map(_.split('='))
          .map(a => (a(0), new URLCodec("US-ASCII").decode(a(1)))).toMap

        if(parsed.contains("error")) ErrorResp(parsed)
        else SimpleAuthzRes(parsed)
      }
    }

    override protected val store: Map[String, Client] = Map(
      "esia" -> DefaultClient("esia", "super secret", "ESIA")
    )

    override implicit val sender: Sender[StringWrapper, String] = new Sender[StringWrapper, String] {
      override def send(a: AuthzReq): Unit = {
        System.out.println(a.asQueryString)
      }
    }

  }

  behavior of "OClient"

  it should "request building" in {
    import TestOClient._
    import scala.language.postfixOps

    request(code) from "esia" redirectTo new URI("home") withScope "usr_inf" withExtParam("time", new Date().toString) send


    System.out.println(callback(StringWrapper("?code=SplxlOBeZQQYbYS6WxSbIA&state=xyz")))

  }

}
