package com.identityblitz.oauth

import com.identityblitz.utils.json.{JWriter, JStr, JObj}
import java.net.URI
import org.apache.commons.codec.net.URLCodec

trait OResponses extends ORequests {

  trait OResp {

    def param(name: String): Option[String]

    /**
     * Serialization methods.
     */

    def asQueryString: String

    def asJson: JObj

  }

  /**
   * Authorization response
   */

  trait AuthzResp extends OResp {

    val code: String = param("code")
      .getOrElse(throw new OAuthException("invalid_response", "Undefined code"))

    val state: Option[String] = param("state")

    override def asQueryString = new URLCodec("US-ASCII").encode(state.foldLeft("?code=" + code)(mp("&state=")(_, _)))

    override def asJson = state.foldLeft(JObj("code" -> JStr(code)))(jmp("state")(_, _))

  }

  /**
   * Access token response
   */

  trait AcsTknResp extends OResp {

    val accessToken: String = param("access_token")
      .getOrElse(throw new OAuthException("invalid_response", "Undefined access token"))

    val tokenType: String = param("token_type")
      .getOrElse(throw new OAuthException("invalid_response", "Undefined token type"))

    val expiresIn: Option[Long] = param("expires_in").map(_.toLong)

    val scope: Option[String] = param("scope")


  }

  /**
   * Access token responses used in the authorization code grant
   */

  trait CodeAcsTknResp extends AcsTknResp {

    val refreshToken: Option[String] = param("refresh_token")

    def asQueryString: String = throw new UnsupportedOperationException

    override def asJson = scope.foldLeft(
      refreshToken.foldLeft(
        expiresIn.map(_.toString).foldLeft(
          JObj("access_token" -> JStr(accessToken)) +
            ("token_type" -> JStr(tokenType))
        )(jmp("expires_in")(_, _))
      )(jmp("refresh_token")(_, _))
    )(jmp("scope")(_, _))

  }

  /**
   * Access token response used in the implicit grant
   */

  trait ImplicitAcsTknResp extends AcsTknResp {

    val state: Option[String] = param("state")

    override def asQueryString = new URLCodec("US-ASCII").encode(scope.foldLeft(
      state.foldLeft(
        expiresIn.map(_.toString).foldLeft(
          "?access_token=" + accessToken + "&token_type=" + tokenType
        )(mp("expires_in")(_, _))
      )(mp("state")(_, _))
    )(mp("scope")(_, _)))

    override def asJson = scope.foldLeft(
      state.foldLeft(
        expiresIn.map(_.toString).foldLeft(
          JObj("access_token" -> JStr(accessToken)) +
            ("token_type" -> JStr(tokenType))
        )(jmp("expires_in")(_, _))
      )(jmp("state")(_, _))
    )(jmp("scope")(_, _))

  }

  /**
   * The access token response used in resource owner password credential grant
   */

  trait ResOwnerPaswdCredAcsTknResp extends CodeAcsTknResp

  /**
   * The access token response used in client credential grant
   */
  trait ClientCredAcsTknResp extends CodeAcsTknResp

  /**
   * Error response
   */

  trait ErrorResp extends OResp {

    val error: String = param("error")
      .getOrElse(throw new OAuthException("invalid_response", "Undefined error"))

    val error_description: Option[String] = param("error_description")

    val error_uri: Option[URI] = param("error_uri").map(new URI(_))

    val state: Option[String] = param("state")

    override def asQueryString = new URLCodec("US-ASCII").encode(state.foldLeft(
      error_uri.map(_.toString).foldLeft(
        error_description.foldLeft("?error=" + error)(mp("error_description")(_, _))
      )(mp("&error_uri=")(_, _))
    )(mp("&state=")(_, _)))

    override def asJson = state.foldLeft(
      error_uri.map(_.toString).foldLeft(
        error_description.foldLeft(JObj("error" -> JStr(error)))(jmp("error_description")(_, _))
      )(jmp("error_uri")(_, _))
    )(jmp("state")(_, _))

  }

  object ErrorResp {
    def apply(e: OAuthException): ErrorResp = new ErrorResp {
      def param(name: String): Option[String] = name match {
        case "error" => Option(e.error)
        case "error_description" => e.errorDescription
        case "error_uri" => e.errorUri.map(_.toString)
        case "state" => e.state
        case _ => None
      }
    }

    def apply(err: String, err_desc: String): ErrorResp = new ErrorResp {
      def param(name: String): Option[String] = Map("error" -> err, "error_description" -> err_desc).get(name)
    }
  }

  private def mp(n: String)(a: String, b: String): String = a + "&" + n + "=" + b

  private def jmp[T](n: String)(a: JObj, b: T)(implicit jw: JWriter[T]): JObj = a + (n, b)

  /**
   * Interaction response
   */

  trait InteractionResp extends OResp {

    val authzReq: AuthzReq

    def param(name: String): Option[String] = ???

    /**
     * Serialization methods.
     */
    def asQueryString: String = ???

    def asJson: JObj = ???
  }

}
