package com.melvic.emails

import fastparse.NoWhitespace._
import fastparse._

object EmailParser {
  val LocalMaxWidth = 64
  val DnsLabelMaxWidth = 63
  val HostNameMaxWidth = 253

  def quoted[_: P]: P[String] =
    P("\"" ~ (basicChars | CharPred(_.isWhitespace) | CharPred(c => c >= 32 && c < 127) | ".") ~ "\"").rep.!

  def unquoted[_: P]: P[String] =
    P((basicChars ~ ".").rep | ".".rep).!

  def local[_: P]: P[String] = {

    P(quoted | unquoted).filter(_.length <= LocalMaxWidth)
  }

  def basicChars[_: P]: P[String] = P(CharPred(_.isLetter) | CharPred(_.isDigit) | CharIn("!#$%&'*+-/=?^_`{|}~")).!

  def label[_: P]: P[String] = P((basicChars ~ "-".?).rep | "-".rep).!.filter(_.length <= DnsLabelMaxWidth)

  def domain[_: P]: P[String] =
    P(label ~ ("." ~ label).rep).!.filter(_.length  <= HostNameMaxWidth)

  def email[_: P] = local ~ "@" ! domain

  def parseEmail(emailString: String): Parsed[String] = parse(emailString, email(_))
}
