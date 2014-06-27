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

  trait ErrorResp extends OResp {

    val error: String = apply("error")

    val error_description: Option[String] = get("error_description")

    val error_uri: Option[URI] = get("error_description").map(new URI(_))

    val state: Option[String] = get("state")

    def mp(n: String)(a: String, b: String): String = a + "&" + n + "=" + b

    override def asQueryString = new URLCodec("US-ASCII").encode(state.foldLeft(
      error_uri.map(_.toString).foldLeft(
        error_description.foldLeft("?error=" + error)(mp("error_description")(_, _))
      )(mp("&error_uri=")(_, _))
    )(mp("&state=")(_, _)))

    //override def asJson = error_description.foldLeft(JObj("error" -> JStr(error)))(_ + ("" -> _)

  }
}
