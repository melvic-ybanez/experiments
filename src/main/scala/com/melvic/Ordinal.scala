package com.melvic

object Ordinal {
  def of(n: Int): String = {
    val suffix = n match {
      case 11 | 12 | 13 => "th" // exception to the rules
      case _ =>
        n % 10 match {
          case 1 => "st"
          case 2 => "nd"
          case 3 => "rd"
          case _ => "th"
        }
    }
    n + suffix
  }
}
