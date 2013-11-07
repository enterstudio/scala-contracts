package com.daltic.finance.contracts

import scala.language.implicitConversions

import org.joda.time.{DateTimeZone, LocalDate}

import Obs._

object Contracts extends PRs {

  def one = One.apply _
  def when = (When.apply _).curried
  def anytime = (Anytime.apply _).curried
  def until = (Until.apply _).curried
  def scale = (Scale.apply _).curried
  def cond = (Cond.apply _).curried

  implicit def toConst[T](x: T) = const(x)

  def const[T](k: T): Obs[T] = Obs((d: Date) => bigK(k))

  def date(implicit int: TimeContext): Obs[Date] = Obs((t: Date) => PR(timeSlices(Stream(t), 0)))

  def bigK[T](x: T): PR[T] = PR(konstSlices(x))

  def konstSlices[T](x: T): Stream[RV[T]] = {
    def nextSlice(sl: Stream[T]): Stream[RV[T]] = sl #:: nextSlice(x #:: sl)
    nextSlice(Stream(x))
  }

  // Why does this work this way?
  // Isn't PR supposed to consist of a set of random values, not lists of values containing possible duplicates?
  def timeSlices(sl: RV[Date], n: Int = 0)(implicit tc: TimeContext): Stream[RV[Date]] = {
    val ((d: Date) #:: _) = sl
    val nextSlice = Stream.fill(n + 2)(d + tc.step)
    sl #:: timeSlices(nextSlice, n + 1)
  }

  def between(d1: Date, d2: Date)(implicit tc: TimeContext): Obs[Boolean] = (date >= d1) && (date <= d2)
  def in(interval: Interval)(implicit tc: TimeContext): Obs[Boolean] = date in interval

  def at(d: Date)(implicit tc: TimeContext): Obs[Boolean] = date === d

  def mkDate(d: CalendarTime, t: TimeStep): Date = Date(d) + t

  def mkDate(t: TimeStep)(implicit tc: TimeContext): Date = {
    // todo: something better than rounding up here
    val s = tc.step
    val rounded = (t + s - 1) / s * s
    tc.start + rounded
  }

  def time0: Date = mkDate(now(), 0)

  def now(implicit timeZone: DateTimeZone = DateTimeZone.UTC): CalendarTime = LocalDate.now()

}
