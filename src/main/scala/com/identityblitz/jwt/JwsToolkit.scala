package com.identityblitz.jwt

import com.identityblitz.utils.json.{JObj, JVal}

trait JwsToolkit extends AlgorithmsKit with JwtToolkit {

  /**
   * This traits is represents JWS header.
   */
  class JWS(val alg: Algorithm[JWS], values: JObj) extends Header[JWS] {
    val typ: Option[String] = values(BaseNameKit.typ.name).asOpt[String]
    val cty: Option[String] = values(BaseNameKit.cty.name).asOpt[String]

    def apply(name: String): JVal = values(name)
    def get(name: String): Option[JVal] = values(name).asOpt[JVal]

    def names:Set[String] = values.fields

    def asBase64: String = utf8StringToBase64((values + ("alg" -> alg.toString)).toJson)

    override def toString: String = (values + ("alg" -> alg.toString)).toString
  }

  val none = Algorithm[JWS]("none",
    (hdr, tkn) => {
      if(!tkn.endsWith("."))
        throw new IllegalStateException("token has wrong format")
      tkn.split("\\.").toList match {
        case _ :: cs :: Nil =>
          new JWTBase[JWS](new JWSNone(hdr), JVal.parseStr(base64ToUtf8String(cs)).as[JObj])
        case _ => throw new IllegalStateException("token has wrong format")
      }},
    j => j.header.asBase64 + "." + j.claimSet.asBase64 + ".",
    new JWSNone(_)
  )

  private sealed class JWSNone(private val values: JObj) extends JWS(none, values)

}
