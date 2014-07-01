package com.identityblitz.oauth

import com.identityblitz.utils.json.{JStr, JObj}
import java.net.URI
import org.apache.commons.codec.net.URLCodec

trait OResponses {

  trait OResp {

    def apply(name: String): String

    def get(name: String): Option[String]


    /**
     * Serialization methods.
     */

    def asQueryString: String

    def asJson: JObj

  }

  case class ExtResp(private val store: Map[String, String]) extends OResp {

    def apply(name: String): String = ???

    def get(name: String): Option[String] = ???

    /**
     * Serialization methods.
     */
    def asQueryString: String = ???

    def asJson: JObj = ???
  }

  trait ErrorResp extends OResp {

    val error: String = apply("error")

    val error_description: Option[String] = get("error_description")

    val error_uri: Option[URI] = get("error_uri").map(new URI(_))

    val state: Option[String] = get("state")

    private def mp(n: String)(a: String, b: String): String = a + "&" + n + "=" + b

    override def asQueryString = new URLCodec("US-ASCII").encode(state.foldLeft(
      error_uri.map(_.toString).foldLeft(
        error_description.foldLeft("?error=" + error)(mp("error_description")(_, _))
      )(mp("&error_uri=")(_, _))
    )(mp("&state=")(_, _)))

    private def jmp(n: String)(a: JObj, b: String): JObj = a + (n, b)

    override def asJson = state.foldLeft(
      error_uri.map(_.toString).foldLeft(
        error_description.foldLeft(JObj("error" -> JStr(error)))(jmp("error_description")(_, _))
      )(jmp("error_uri")(_, _))
    )(jmp("state")(_, _))

  }

  object ErrorResp {
    def apply(e: OAuthException): ErrorResp = new ErrorResp {
      def get(name: String): Option[String] = name match {
        case "error" => Option(e.error)
        case "error_description" => e.errorDescription
        case "error_uri" => e.errorUri.map(_.toString)
        case "state" => e.state
        case _ => None
      }
      def apply(name: String): String = get(name).get
    }

    def apply(err: String, err_desc: String): ErrorResp = new ErrorResp {
      def get(name: String): Option[String] = Map("error" -> err, "error_description" -> err_desc).get(name)
      def apply(name: String): String = get(name).get
    }
  }
}
