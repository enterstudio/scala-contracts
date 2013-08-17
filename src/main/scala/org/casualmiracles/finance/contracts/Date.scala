package org.casualmiracles.finance.contracts

case class Date(c: CalendarTime, t: TimeStep) extends Ordered[Date] {
  def compare(that: Date) = c.plusDays(t) compareTo that.c.plusDays(that.t)
}
