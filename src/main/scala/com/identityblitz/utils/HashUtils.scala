package com.identityblitz.utils

import java.security.MessageDigest

object HashUtils {

  def calcSha1(data: Array[Byte]): Array[Byte] = {
    val md = MessageDigest.getInstance("SHA-1")
    md.digest(data)
  }

}
