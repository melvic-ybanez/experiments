package com.melvic.numbers

sealed trait PhoneNumberError

object PhoneNumberError {
  case object InvalidPhoneNumber extends PhoneNumberError
  case object NotEnoughDigits extends PhoneNumberError
}
