package com.identityblitz.jwt

import org.apache.commons.codec.binary.Base64

object Runner {

  def main(args: Array[String]) {
    import JwsToolkit._

    val jwt = JWT(Base64.encodeBase64URLSafeString("{\"alg\":\"none\"}".getBytes("UTF-8")))

    //System.out.println(JwsToolkit.HMAC)
    //System.out.println(JwsToolkit.AES)

  }

}
