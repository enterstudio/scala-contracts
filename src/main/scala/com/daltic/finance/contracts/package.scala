package com.daltic.finance

import org.joda.time.LocalDate

import com.daltic.finance.contracts.Contracts._

package object contracts {
  type RV[A] = Stream[A]
  type TimeStep = Int
  type CalendarTime = LocalDate
  case class TimeContext(step: TimeStep, start: Date = time0) {
    require(step > 0)
  }
}
