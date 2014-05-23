package com.identityblitz.jwt

import java.lang.System
import com.identityblitz.utils.json.JVal

object BaseJwtToolkit extends AlgorithmsKit with JwsToolkit with DefaultCryptoServiceContainer

object Runner {

  def main(args: Array[String]) {
    import BaseJwtToolkit._

    val plainJwt = "eyJhbGciOiJub25lIn0.eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ."

    val jwt = JWT[ClaimsSet](plainJwt)

    System.out.println(jwt.header.alg.name)
    System.out.println(jwt.header.typ)

    System.out.println(jwt.payload.iss.get)
    System.out.println(jwt.payload.exp.get)
    System.out.println(jwt.payload.names)

    import BaseNameKit._

    val jwt2 =  builder.alg(none)
      .header(typ % "JWT")
      .payload(cs(
      iss % StringOrUri("joe"),
      aud % Array(StringOrUri("aud1"), StringOrUri("aud2"), StringOrUri("aud3")),
      iss % StringOrUri("joe")))
      .build

    System.out.println(jwt2)
    System.out.println(jwt2 asBase64)



    val jwt3 = builder.alg(HS256).
      header(typ % "JWT")
      .payload(cs(
      iss % StringOrUri("joe"),
      aud % Array(StringOrUri("aud1"), StringOrUri("aud2"), StringOrUri("aud3")),
      iss % StringOrUri("joe")))
      .build

    System.out.println(jwt3)
    System.out.println(jwt3 asBase64)

    val strJwtHS256 = "eyJ0eXAiOiJKV1QiLA0KICJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
    val jwtHS256 = JWT[ClaimsSet](strJwtHS256)

    System.out.println(jwtHS256)

    val publEc = JVal.parseStr("""{"kty":"EC",
                      "crv":"P-256",
                      "x":"MKBCTNIcKUSDii11ySs3526iDZ8AiTo7Tu6KPAqv7D4",
                      "y":"4Etl6SRW2YiLUrN5vfvVHuhp7x8PxltmWWlbbM4IFyM",
                      "use":"enc",
                      "kid":"1"}""").as[JWK]

    System.out.println("")




  }

}
