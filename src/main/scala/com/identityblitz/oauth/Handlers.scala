package com.identityblitz.oauth

trait Handlers extends ORequests with OResponses {

  trait Handler {
    def handle(req: OReq): OResp
  }

}
