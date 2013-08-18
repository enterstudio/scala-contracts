package org.casualmiracles.finance.contracts.examples

import org.casualmiracles.finance.contracts._

import Cashflows._
import Contracts._
import ExampleModel._
import Instruments._

object SwapExperiment extends App {

  def interestRate = const(1.0) // obviously need a real source of interest rates

  def fixedRate(notional: BigDecimal, currency: Currency, rate: BigDecimal) = {
    scale(notional * rate)(One(currency))
  }

  def floatingRate(notional: BigDecimal, currency: Currency, rate: Obs[BigDecimal]) = {
    scale(rate * notional)(One(currency))
  }

  def uniformSchedule(start: Date, end: Date, frequency: Int): Obs[Boolean] = {
    between(start, end) && ((date - start) % frequency === 0)
  }

 val fixedSchedule = uniformSchedule(mkDate(4), mkDate(10), 2)
 val floatingSchedule = uniformSchedule(mkDate(4), mkDate(10), 3)

 val fixedLeg = when(fixedSchedule)(fixedRate(1, USD, 0.05))
 val floatingLeg = when(floatingSchedule)(floatingRate(1, USD, interestRate))

 val example = swap(fixedLeg, floatingLeg)

 val horizon = 15
 val xm = exampleModel(time0)
 val evalX = evalC(xm, USD)

 printPr(cashflow(xm, USD, horizon)(example), horizon)
}
