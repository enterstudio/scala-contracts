package org.casualmiracles.finance.contracts

import org.joda.time.Days

case class Date(c: CalendarTime, t: TimeStep) extends Ordered[Date] {
  def localDate = c.plusDays(t)
  def compare(that: Date) = localDate compareTo that.localDate

  def -(d: Date): Int = Days.daysBetween(d.localDate, localDate).getDays
}
