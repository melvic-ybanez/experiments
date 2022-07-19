package com.melvic.emails

import fastparse.NoWhitespace._
import fastparse._

import java.nio.charset.Charset

object EmailParser {
  val LocalMaxWidth = 64
  val DnsLabelMaxWidth = 63
  val HostNameMaxWidth = 253

  def quoted[_: P]: P[String] =
    P("\"" ~ (escape.? ~ (ascii | quotedPair)).rep ~ "\"").!

  def escape[_: P]: P[String] =
    P("\\\\" | "\\\"").!

  def quotedPair[_: P]: P[String] =
    P("\"\\\" " ~ ascii).!

  def unquoted[_: P]: P[String] =
    P(unquotedChar ~ ("." ~ unquotedChar).rep).rep(1).!

  def local[_: P]: P[String] =
    P(unquoted | quoted).filter(_.length <= LocalMaxWidth)

  def domain[_: P]: P[String] =
    P(label ~ ("." ~ label).rep).!.filter(_.length <= HostNameMaxWidth)

  def ipAddress[_: P]: P[String] = P("[" ~ (ipv4 | ipv6) ~ "]")

  def email[_: P]: P[Email] = P(Start ~ local ~ "@" ~ (domain | ipAddress) ~ End).map { case (local, domain) =>
    Email(local, domain)
  }

  def parseEmail(rawEmail: String): Parsed[Email] = parse(rawEmail, email(_))

  def parseEmailToEither(rawEmail: String): Either[String, Email] =
    parseEmail(rawEmail).fold((msg, _, _) => Left(msg), (email, _) => Right(email))

  private def ascii[_: P]: P[String] =
    P(CharPred(c => Charset.forName("US-ASCII").newEncoder.canEncode(c) && !"\"\\".contains(c))).!

  private def letterOrDigit[_: P]: P[String] =
    P(CharPred(_.isLetter) | CharPred(_.isDigit)).!

  private def unquotedChar[_: P]: P[String] =
    P(letterOrDigit | CharIn("!#$%&'*+\\-/=?^_`{|}~")).!

  private def labelChar[_: P]: P[String] =
    P(letterOrDigit | CharIn("!#$%&'*+/=?^`{|}~")).!

  private def label[_: P]: P[String] =
    P(labelChar ~ ("-" ~ labelChar).rep).rep(1).!.filter(_.length <= DnsLabelMaxWidth)

  private def octet[_: P]: P[Int] =
    P(CharPred(_.isDigit)).rep(1).!.map(_.toInt).filter(x => x >= 0 && x <= 255)

  private def hexadecimal[_: P]: P[String] =
    P(CharPred(_.isDigit) | CharIn("abcdefABCDEF")).rep(1).!

  private def ipv4[_: P]: P[String] =
    P("IPv4:".? ~ octet ~ ("." ~ octet).rep(exactly = 3)).!

  private def ipv6[_: P]: P[String] =
    P("IPv6:".? ~ hexadecimal.? ~ (":" ~ hexadecimal.?).rep(exactly = 7)).!
}
