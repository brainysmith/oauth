package com.identityblitz.oauth

import scala.util.{Failure, Success, Try}

trait OServer[Req, Resp] {

  val responseTypeHandlers: Map[Set[String], Handler]

  def authorizationEndpoint(req: Req)(implicit reqConverter: AReqConverter[Req],
                                      respConverter: RespConverter[Resp]): Resp = {
    respConverter.convert(
      Try{
        val oreq = reqConverter.convert(req)
        responseTypeHandlers.get(oreq.responseType).map(_.handle(oreq)).orElse(
          //TODO handle case with unsupported multivalued response types
          null
        ).get
      } match {
        case Success(r) => r
        case Failure(e: OAuthException) => null
        case Failure(e) => null
      })
  }

}

trait AReqConverter[Req] {
  def convert(in: Req): ORequests#AuthzReq
}

trait RespConverter[Resp] {
  def convert(res: OResponses#OResp): Resp
}

trait Handler {
  def handle(req: ORequests#OReq): OResponses#OResp
}
