package com.identityblitz.oauth

import scala.util.{Failure, Success, Try}

trait OServer[Req, Resp] extends OResponses with ORequests {

  val responseTypeHandlers: Map[Set[String], Handler]

  def ea(req: Req)(implicit reqConverter: AReqConverter,
                   respConverter: RespConverter): Resp = _ea(reqConverter.convert(req))

  /**
   * The function is intended to be implemented in final server object to give the possibility of switching
   * the control to the user interaction interface.
   * @param resp - response from the main OAuth flow asking for some actions to be fulfilled
   *             by the user interaction interface.
   * @return - user interaction interface.
   */
  def goToUserInteraction(resp: ExtResp): Resp

  def returnFromUserInteraction(req: ExtReq)(implicit respConverter: RespConverter): Resp = _ea(req)

  private def _ea(req: OReq)(implicit respConverter: RespConverter): Resp = {
    Try{
      responseTypeHandlers.get(req.responseType).map(_.handle(req)).orElse(
        Option(ErrorResp("unsupported_response_type",
          """The authorization server does not support obtaining an
            |access token using this method."""))
      ).get
    } match {
      case Success(r: ExtResp) => goToUserInteraction(r)
      case Success(r) => respConverter.convert(r)
      case Failure(e: OAuthException) => respConverter.convert(ErrorResp(e))
      case Failure(e) => respConverter.convert(ErrorResp("server_error", e.getMessage))
    }
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
