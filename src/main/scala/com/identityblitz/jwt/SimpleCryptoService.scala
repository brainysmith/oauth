package com.identityblitz.jwt

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import java.security.cert.X509Certificate
import java.security.cert.CertificateFactory
import java.io.ByteArrayInputStream
import java.security.{KeyFactory, Signature}
import java.security.spec.{RSAPrivateKeySpec, RSAPublicKeySpec}
import java.security.interfaces.RSAPrivateKey

trait SimpleCryptoService extends JwkToolkit {

  object SimpleCrypto extends CryptoService {
    def sign(alg: String, jwk: JWK, plainText: Array[Byte]): Array[Byte] = {
      alg match {
        case "HS256" =>
          val hs256 = Mac.getInstance("HmacSHA256")
          hs256.init(new SecretKeySpec(jwk.asInstanceOf[SymmetricKey].k, "HmacSHA256"))
          hs256.doFinal(plainText)
        case "RS256" =>
          val sig = Signature.getInstance("SHA256withRSA")
          val key = jwk.asInstanceOf[RsaPrivateKey]
          sig.initSign(KeyFactory.getInstance("RSA").generatePrivate(new RSAPrivateKeySpec(key.n, key.d)))
          sig.update(plainText)
          sig.sign()
      }
    }

    def verify(alg: String, jwk: JWK, plainText: Array[Byte], signature: Array[Byte]): Boolean = {
      alg match {
        case "HS256" =>
          val hs256 = Mac.getInstance("HmacSHA256")
          hs256.init(new SecretKeySpec(jwk.asInstanceOf[SymmetricKey].k, "HmacSHA256"))
          val calculated = hs256.doFinal(plainText)
          calculated.deep == signature.deep
        case "RS256" =>
          val sig = Signature.getInstance("SHA256withRSA")
          val key = jwk.asInstanceOf[RsaPrivateKey]
          sig.initVerify(KeyFactory.getInstance("RSA").generatePublic(new RSAPublicKeySpec(key.n, key.e)))
          sig.update(plainText)
          sig.verify(signature)
      }
    }

    def encrypt(alg: String, jwk: JWK, plainText: Array[Byte]): Array[Byte] = ???

    def decrypt(alg: String, jwk: JWK, cipherText: Array[Byte]): Array[Byte] = ???

    def createX509Cert(der: Array[Byte]): X509Certificate = {
      val factory = CertificateFactory.getInstance("X.509")
      factory.generateCertificate(new ByteArrayInputStream(der)).asInstanceOf[X509Certificate]
    }

    def verifyX509CertChain(chain: Array[X509Certificate]): Option[X509Certificate] = ???
  }

  implicit val cryptoService = SimpleCrypto

}
