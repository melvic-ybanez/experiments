package com.melvic

object PhoneNumber {
  val pattern = "\\+?([0-9]| )+".r

  def validate(rawPhoneNumber: String): Either[String, String] =
    Either
      .cond(pattern.matches(rawPhoneNumber), rawPhoneNumber, "Invalid phone number")
      .flatMap { number =>
        if (number.count(_.isDigit) < 9) Left("Not enough digits")
        else Right(number)
      }
}
