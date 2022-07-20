package com.melvic.emails

final case class Email(local: String, domain: String) {

  /**
   * Obfuscates an email. The raw email should be converted to lowercase. All the characters in the local
   * part, except the first and the last, should be replaced by 5 asterisks.
   */
  lazy val obfuscate: Email = {
    val lowerLocal = local.toLowerCase
    Email(lowerLocal.head + "*****" + lowerLocal.last, domain.toLowerCase)
  }

  lazy val show: String = s"$local@$domain"
}

object Email {

  /**
   * Creates an email from a raw string.
   */
  def fromString(rawEmail: String): Either[String, Email] =
    EmailParser.parseEmailToEither(rawEmail)
}
