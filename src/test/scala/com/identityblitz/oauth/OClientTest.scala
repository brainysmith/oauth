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

    case class SimpleAccessReq(private val store: Map[String, String]) extends CodeAcsTknReq {
      override def param(name: String): Option[String] = store.get(name)
      override def names: Set[String] = store.keys.toSet
    }

    implicit val aConverter = new AReqConverter[StringWrapper] {
      override def convert(in: StringWrapper): AcsTknReq = ???
      override def convert(in: AccessReqBuilder[_]): AcsTknReq = in match {
        case b: CodeReqBuilder => SimpleAccessReq(Map(
          "grant_type" -> b.grant.name,
          "client_id" -> b.clientId.get,
          "code" -> b.code,
          "redirect_uri" -> b.redirectUri.get.toString
        ) ++ in.extParams)
        case _ => throw new IllegalArgumentException("Unsupported access request builder")
      }
    }

    override protected val store: Map[String, Client] = Map(
      "esia" -> DefaultClient("esia", "super secret", "ESIA")
    )

    override implicit val sender: Sender[StringWrapper, String] = new Sender[StringWrapper, String] {
      override def send(a: AuthzReq): String = a.asQueryString
      override def send(a: AcsTknReq): String = a.asJson.toJson
    }

  }

  behavior of "OClient"

  it should "request building" in {
    import TestOClient._
    import scala.language.postfixOps
    import scala.language.reflectiveCalls



    System.out.println(oRequest(code) from "esia" redirectTo new URI("home") withScope "usr_inf" withExtParam("time", new Date().toString) send)

    System.out.println(callback(StringWrapper("?code=SplxlOBeZQQYbYS6WxSbIA&state=xyz")))

    System.out.println(aRequest(authorization_code)
      .withCode("some_code")
      .from("esia")
      .redirectTo(new URI("home"))
      .send)

  }

}
