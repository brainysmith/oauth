package com.identityblitz.jwt

import java.lang.System

object BaseJwtToolkit extends AlgorithmsKit with JwsToolkit with SimpleCryptoService with SimpleKidsRegisterService

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

    val strJwtRS256 = "eyJhbGciOiJSUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.cC4hiUPoj9Eetdgtv3hF80EGrhuB__dzERat0XF9g2VtQgr9PJbu3XOiZj5RZmh7AAuHIm4Bh-0Qc_lF5YKt_O8W2Fp5jujGbds9uJdbF9CUAr7t1dnZcAcQjbKBYNX4BAynRFdiuB--f_nZLgrnbyTyWzO75vRK5h6xBArLIARNPvkSjtQBMHlb1L07Qe7K0GarZRmB_eSN9383LcOLn6_dO--xi12jzDwusC-eOkHWEsqtFZESc6BfI7noOPqvhJ1phCnvWh6IeYI2w9QOYEUipUTI8np6LbgGY9Fs98rqVt5AXLIhWkWywlVmtVrBp0igcN_IoypGlUPQGe77Rw"
    val jwtRS256 = JWT[ClaimsSet](strJwtRS256)

    System.out.println(jwtRS256)

    val jwt4 = builder.alg(RS256).
      header()
      .payload(cs(
      iss % StringOrUri("joe"),
      exp % IntDate(1300819380),
      "http://example.com/is_root" % true))
      .build

    System.out.println(jwt4)
    System.out.println(jwt4 asBase64)

    val strJwtES256 = "eyJhbGciOiJFUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.DtEhU3ljbEg8L38VWAfUAqOyKAM6-Xx-F4GawxaepmXFCgfTjDxw5djxLa8ISlSApmWQxfKTUJqPP3-Kg6NU1Q"
    val jwtES256 = JWT[ClaimsSet](strJwtES256)

    System.out.println(jwtES256)

    val jwt5 = builder.alg(ES256).
      header()
      .payload(cs(
      iss % StringOrUri("joe"),
      exp % IntDate(1300819380),
      "http://example.com/is_root" % true))
      .build

    System.out.println(jwt5)
    System.out.println(jwt5 asBase64)

    val jwtES256Check = JWT[ClaimsSet](jwt5 asBase64)
    System.out.println("Check passed!")

  }

}
