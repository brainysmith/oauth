package com.identityblitz.oauth

trait Handlers extends ORequests with OResponses with ClientStore{

  trait Handler {
    def handle(req: OReq): OResp
  }

}
