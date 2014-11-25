package com.identityblitz.oauth

import scala.util.{Failure, Success, Try}

trait OClient[Req, Resp] extends OAuthErrors {

  implicit val zSender: ZSender[Resp]

  final def callback(res: Req)(implicit c: ZRespConverter[Req]): Either[OAuthException, AuthzResp] = {
    Try(c.convert(res)) match {
      case Success(res: AuthzResp) => Right(res)
      case Success(err: ErrorResp) => Left(err.asException)
      case Failure(e: OAuthException) => Left(e)
      case Failure(e) => Left(new OAuthException("server_error", e.getMessage))
      case _ @ r => Left(new OAuthException("server_error", r.toString))
    }
  }

}
