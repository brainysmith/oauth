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

    /*class JwtBuilder(set: Seq[Claim])

    def cs(s: Claim*): JwtBuilder = new JwtBuilder(s)

    trait Claim

    class StringMapper(name: String) {
      def -> (value: String): Claim = ???
      def -> (value: StringOrUri): Claim = ???
      def -> (value: Int): Claim = ???
      def -> (value: IntDate): Claim = ???
    }

    class JwtBuilder2[A] {
      def header():A = ???
    }

    implicit def alg2Header[A <: Algorithm](a: A) = new JwtBuilder2[A]

    implicit def string2Claim(x: String): StringMapper = new StringMapper(x)

    none header () cs ("" -> "",
      "" -> 1,
      "" -> StringOrUri(""),
      "" -> new IntDate(7))*/

  }

}
