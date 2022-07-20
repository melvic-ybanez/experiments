package com.melvic

import com.melvic.emails.EmailParser
import com.melvic.numbers.PhoneNumber

object Obfuscation {
  def of(input: String): Either[String, String] =
    EmailParser
      .parseEmailToEither(input)
      .map(_.obfuscate.show)
      .orElse(PhoneNumber.validate(input).map(PhoneNumber.obfuscate).left.map(_ => "Invalid input"))
}
