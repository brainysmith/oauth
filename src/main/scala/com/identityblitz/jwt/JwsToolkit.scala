package com.identityblitz.jwt

trait Jwstk extends AlgorithmsKit {
  val HMAC = Algorithm("HMAC", AlgorithmType.SIGNING, (h, a) => null)
}

trait Jwetk extends AlgorithmsKit {
  val AES = Algorithm("AES", AlgorithmType.ENCRYPTION, (h, a) => null)
}

object JwsToolkit extends JwtToolkit with Jwetk {

}
