package com.melvic.numbers

import com.melvic.numbers.PhoneNumberError.{InvalidPhoneNumber, NotEnoughDigits}

object PhoneNumber {
  val pattern = "\\+?([0-9]| )+".r

  def validate(rawPhoneNumber: String): Either[PhoneNumberError, String] =
    Either
      .cond(pattern.matches(rawPhoneNumber), rawPhoneNumber, InvalidPhoneNumber)
      .flatMap { number =>
        if (number.count(_.isDigit) < 9) Left(NotEnoughDigits)
        else Right(number)
      }

  /**
   * Obfuscates a phone number. Spaces are converted to dashes, and digits are converted to asterisks, except
   * for the last four.
   */
  def obfuscate(rawPhoneNumber: String): String =
    rawPhoneNumber
      .replace(" ", "-")
      .reverse
      .foldLeft(0, "") { case ((count, acc), c) =>
        if (c.isDigit)
          if (count >= 4) (count + 1, '*' + acc)
          else (count + 1, c + acc)
        else (count, c + acc)
      }
      ._2
}
