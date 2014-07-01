package com.identityblitz.oauth

import scala.util.{Failure, Success, Try}

trait OServer[Req, Resp] extends OResponses with ORequests {

  val responseTypeHandlers: Map[Set[String], Handler]

  def ea(req: Req)(implicit reqConverter: AReqConverter,
                   respConverter: RespConverter): Resp = {
    respConverter.convert(
      Try{
        val oreq = reqConverter.convert(req)
        responseTypeHandlers.get(oreq.responseType).map(_.handle(oreq)).orElse(
          Option(ErrorResp("unsupported_response_type",
            """The authorization server does not support obtaining an
              |access token using this method."""))
        ).get
      } match {
        case Success(r) => r
        case Failure(e: OAuthException) => ErrorResp(e)
        case Failure(e) => ErrorResp("server_error", e.getMessage)
      })
  }

  trait AReqConverter {
    def convert(in: Req): AuthzReq
  }

  trait RespConverter {
    def convert(res: OResp): Resp
  }

  trait Handler {
    def handle(req: OReq): OResp
  }

}
