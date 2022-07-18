package com.melvic

import java.time.format.DateTimeFormatter
import java.time.{DayOfWeek, LocalDate}
import scala.annotation.tailrec

object Sundays {
  def between(from: String, to: String): Int = {
    val formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy")
    val fromDate = LocalDate.parse(from, formatter)
    val toDate = LocalDate.parse(to, formatter)

    @tailrec
    def loop(count: Int, fromDate: LocalDate): Int =
      if (fromDate.isAfter(toDate)) count
      else {
        val isSunday = fromDate.getDayOfWeek == DayOfWeek.SUNDAY
        loop(if (isSunday) count + 1 else count, fromDate.plusDays(1))
      }

    loop(0, fromDate)
  }
}
