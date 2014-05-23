package com.identityblitz.jwt

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import org.apache.commons.codec.binary.Base64
import javax.security.cert.X509Certificate
import java.security.cert.CertificateFactory
import java.io.ByteArrayInputStream

trait DefaultCryptoServiceContainer extends JwkToolkit {

  val sk256 = new SecretKeySpec(Base64.decodeBase64(
    "AyM1SysPpbyDfgZld3umj1qzKObwVMkoqQ-EstJQLr_T-1qS0gZH75aKtMN3Yj0iPS4hcgUuTwjAzZr1Z9CAow"), "HmacSHA256")

  object SimpleCrypto extends CryptoService {
    def sign(alg: String, jwk: JWK, plainText: Array[Byte]): Array[Byte] = {
      alg match {
        case "HS256" =>
          val hs256 = Mac.getInstance("HmacSHA256")
          hs256.init(sk256)
          hs256.doFinal(plainText)
      }
    }

    def verify(alg: String, jwk: JWK, plainText: Array[Byte], signature: Array[Byte]): Boolean = {
      alg match {
        case "HS256" =>
          val hs256 = Mac.getInstance("HmacSHA256")
          hs256.init(sk256)
          val calculated = hs256.doFinal(plainText)
          calculated.deep == signature.deep
      }
    }

    def encrypt(alg: String, jwk: JWK, plainText: Array[Byte]): Array[Byte] = ???

    def decrypt(alg: String, jwk: JWK, cipherText: Array[Byte]): Array[Byte] = ???

    def createX509Cert(der: Array[Byte]): X509Certificate = {
      val factory = CertificateFactory.getInstance("X.509")
      factory.generateCertificate(new ByteArrayInputStream(der)).asInstanceOf[X509Certificate]
    }
  }

  implicit val cryptoService = SimpleCrypto

}
