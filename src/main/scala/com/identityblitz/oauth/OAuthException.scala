package com.identityblitz.oauth

import java.net.URI

case class OAuthException(error: String,
                          errorDescription: Option[String],
                          errorUri: Option[URI],
                          state: Option[String]) extends Exception(errorDescription.getOrElse("")) {

  def this(error: String,
           errorDescription: String,
           state: Option[String] = None) = this(error, Some(errorDescription), None, state)

}
