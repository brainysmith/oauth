package com.identityblitz.jwt

import java.lang.System

object Runner {

  def main(args: Array[String]) {
    import JwsToolkit._

    val plainJwt = "eyJhbGciOiJub25lIn0.eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ."

    val jwt = JWT(plainJwt)

    System.out.println(jwt.claimSet.iss.get)
    System.out.println(jwt.claimSet.exp.get)
    System.out.println(jwt.claimSet.names)

    import BaseNameKit._

    val jwt2 = builder alg none header (
      typ % "JWT"
      ) cs (
      iss % StringOrUri("joe"),
      aud % Array(StringOrUri("aud1"), StringOrUri("aud2"), StringOrUri("aud3"))
      ) build

    System.out.println(jwt2)
    System.out.println(jwt2 asBase64)

  }

}
