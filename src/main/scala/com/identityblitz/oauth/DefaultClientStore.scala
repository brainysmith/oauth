package com.identityblitz.oauth

import com.identityblitz.json.{JResult, JVal, JReader}

import scala.collection.Map

trait DefaultClientStore extends ClientStore with OAuthErrors {

  protected val store: Map[String, Client]

  override def byClientId(id: String): Option[Client] = store.get(id)

  case class DefaultClient(id: String,
                           clientSecret: String,
                           name: String) extends Client {

    override def authenticate(req: OReq): Either[OAuthException, Client] = req match {
      case azr: AuthzReq => Right(this)
      case acr: CodeAcsTknReq => acr.param("client_secret")
        .toRight(new OAuthException("unauthorized_client", "client_id or client_secret is incorrect", None))
        .right.flatMap{cs =>
        if(cs == clientSecret) Right(this)
        else Left(new OAuthException("unauthorized_client", "client_id or client_secret is incorrect", None))}
    }

    /**
     * Builds client secret for the specified request.
     * @param req - request to build client secret for
     * @return - built client secret
     */
    override def buildClientSecret(req: OReq): Option[String] = Option(clientSecret)

  }

}

