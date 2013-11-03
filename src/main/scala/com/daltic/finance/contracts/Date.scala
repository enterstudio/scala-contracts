package com.daltic.finance.contracts

import org.joda.time.Days

case class Date(c: CalendarTime) extends Ordered[Date] {
  def localDate = c
  def compare(that: Date) = localDate compareTo that.localDate

  def -(d: Date): TimeStep = Days.daysBetween(d.localDate, localDate).getDays
  def +(ts: TimeStep): Date = Date(c plusDays ts)
  def -(ts: TimeStep): Date = Date(c minusDays ts)

  def ->(d: Date): Interval = Interval(this, d)
}

case class Interval(d1: Date, d2: Date) {
  def contains(d: Date): Boolean = d >= d1 && d <= d2
}

object Date {
  def apply(c: CalendarTime, t: TimeStep): Date = Date(c) + t
}
