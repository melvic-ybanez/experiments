package com.melvic.emails

final case class Email (local: String, domain: String) {
  lazy val obfuscate: Email = {
    val lowerLocal = local.toLowerCase
    Email(lowerLocal.head + "*****" + lowerLocal.last, domain.toLowerCase)
  }

  lazy val show: String = s"$local@$domain"
}
