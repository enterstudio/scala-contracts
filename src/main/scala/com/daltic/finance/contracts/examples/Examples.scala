package com.daltic.finance.contracts.examples

import com.daltic.finance.contracts._
import Contracts._
import ExampleModel._
import Cashflows._
import Instruments._

object Examples extends App {

  val xm: Model = exampleModel(mkDate(0))
  val evalX: Contract => PR[BigDecimal] = evalC(xm, USD)
  val t1Horizon = 3
  val t1 = mkDate(t1Horizon)
  val c1: Contract = zeroCouponBond(t1, 10, USD)

  val c11 = european(mkDate(20),
    zeroCouponBond(mkDate(20), 0.4, USD) and
      zeroCouponBond(mkDate(30), 9.3, USD) and
      zeroCouponBond(mkDate(40), 109.3, USD) andGive zeroCouponBond(mkDate(12), 100.0, GBP))

  println("C1")
  printPr(evalX(c1), 10)

  def absorbEx(t: Date, x: BigDecimal, k: Currency) = until(const(t) > date)(scale(x)(one(k)))

  // some examples from the paper
  val t2 = mkDate(10)

  def rainInCyprus = const(10.0) // something that generates rainfall figures
  def interestRate = const(1.0) // obviously need a real source of interest rates

  val c8 = scale(rainInCyprus)(One(GBP))

  val c9 = scale((rainInCyprus - const(7)) * const(1000))(One(GBP))

  val c10 = cond(rainInCyprus > const(9))(c8)(c9)

  val c12 = until(interestRate > const(6))(american(t1, t2, c10))

  // custom examples
  val c2: Contract = couponBond(t2, 10, USD, 2, 5, 0.05)

  val c22 = european(t1, c2)

  println("c11 cashflow")
  printPr(cashflow(xm, USD, 10)(c11), 100)

  println("C2")
  println(c2)
  printPr(evalX(c2), 10)

  println("c22 cashflow")
  printPr(cashflow(xm, USD, 10)(c22), 100)
}
