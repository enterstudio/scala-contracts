package com.daltic.finance.contracts

import org.joda.time.Days

case class Date(c: CalendarTime) extends Ordered[Date] {
  def localDate = c
  def compare(that: Date) = localDate compareTo that.localDate

  def -(d: Date): TimeStep = Days.daysBetween(d.localDate, localDate).getDays
  def +(ts: TimeStep): Date = Date(c plusDays ts)
  def -(ts: TimeStep): Date = Date(c minusDays ts)
}

object Date {
  def apply(c: CalendarTime, t: TimeStep): Date = Date(c) + t
}
