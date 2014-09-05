package com.identityblitz.oauth

import scala.util.{Failure, Success, Try}

trait OServer[Req, Resp] extends Handlers {

  val responseTypeHandlers: Map[Set[String], Handler]

  val grantTypeHandlers: Map[String, Handler]

  def ea(req: Req)(implicit reqConverter: ZReqConverter[Req],
                   respConverter: RespConverter[Resp]): Resp = Try(_ea(reqConverter.convert(req))) match {
    case Success(r) => r
    case Failure(e: OAuthException) => respConverter.convert(ErrorResp(e))
    case Failure(e) => respConverter.convert(ErrorResp("server_error", e.getMessage))
  }

  /**
   * The function is intended to be implemented in final server object to give the possibility of switching
   * the control to the user interaction interface.
   * @param resp - response from the main OAuth flow asking for some actions to be fulfilled
   *             by the user interaction interface.
   * @return - user interaction interface.
   */
  def goToUserInteraction(resp: InteractionResp): Resp

  //TODO rewrite completely
  def returnFromUserInteraction(req: InteractionReq)(implicit respConverter: RespConverter[Resp]): Resp = _ea(req.authzReq)

  private def _ea(req: AuthzReq)(implicit respConverter: RespConverter[Resp]): Resp = {
    Try{
      responseTypeHandlers.get(req.responseType).map(_.handle(req)).orElse(
        Option(ErrorResp("unsupported_response_type",
          """The authorization server does not support obtaining an
            |access token using this method."""))
      ).get
    } match {
      case Success(ir: InteractionResp) => goToUserInteraction(ir)
      case Success(r) => respConverter.convert(r)
      case Failure(e: OAuthException) => respConverter.convert(ErrorResp(e))
      case Failure(e) => respConverter.convert(ErrorResp("server_error", e.getMessage))
    }
  }

  def te(req: Req)(implicit reqConverter: AReqConverter[Req],
                   respConverter: RespConverter[Resp]): Resp = {
    val oreq = reqConverter.convert(req)
    respConverter.convert(Try{
      grantTypeHandlers.get(oreq.grantType).map(_.handle(oreq)).orElse(
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

}
