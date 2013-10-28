package com.daltic.finance.contracts

import scala.language.implicitConversions

import org.joda.time.{DateTimeZone, LocalDate}

object Contracts extends PRs {

  def one = One.apply _
  def when = (When.apply _).curried
  def anytime = (Anytime.apply _).curried
  def until = (Until.apply _).curried
  def scale = (Scale.apply _).curried
  def cond = (Cond.apply _).curried

  implicit def toConst[T](x: T) = const(x)

  def const[T](k: T): Obs[T] = Obs((d: Date) => bigK(k))

  def date: Obs[Date] = Obs((t: Date) => PR(timeSlices(Stream(t), 0)))

  def bigK[T](x: T): PR[T] = PR(konstSlices(x))

  def konstSlices[T](x: T): Stream[RV[T]] = {
    def nextSlice(sl: Stream[T]): Stream[RV[T]] = sl #:: nextSlice(x #:: sl)
    nextSlice(Stream(x))
  }

  // Why does this work this way?
  // Isn't PR supposed to consist of a set of random values, not lists of values containing possible duplicates?
  def timeSlices(sl: RV[Date], n: Int = 0): Stream[RV[Date]] = {
    val ((d: Date) #:: _) = sl
    val nextSlice = Stream.fill(n + 2)(d + 1)
    sl #:: timeSlices(nextSlice, n + 1)
  }

  def between(d1: Date, d2: Date): Obs[Boolean] = (date >= d1) && (date <= d2)

  def at(d: Date): Obs[Boolean] = date === d

  def mkDate(d: CalendarTime, t: TimeStep): Date = Date(d) + t

  def mkDate(t: TimeStep): Date = Date(now(), t)

  def time0: Date = mkDate(now(), 0)

  def now(implicit timeZone: DateTimeZone = DateTimeZone.UTC): CalendarTime = LocalDate.now()

}