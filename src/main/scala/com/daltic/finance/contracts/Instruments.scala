package com.daltic.finance.contracts

object Instruments {
  import Contracts._

  def money(n: Obs[BigDecimal], c: Currency): Contract = scale(n)(One(c))
  def zeroCouponBond(d: Date, n: Obs[BigDecimal], c: Currency): Contract = when(at(d))(money(n, c))
  def couponBond(d: Date, par: BigDecimal, c: Currency, s: TimeStep, steps: Int, rate: Obs[BigDecimal]): Contract = {
    (1 until steps).foldLeft(zeroCouponBond(d, par, c)) { (contract, i) =>
      contract and zeroCouponBond(d - i*s, rate*par, c)
    }
  }
  def callable(callPrices: Map[Date, Contract], contract: Contract): Contract = {
    callPrices.foldLeft(contract) { case (cont, (d, p)) =>
      cont andGive european(d, cont andGive p)
    }
  }

  def european(d: Date, c: Contract): Contract = when(at(d))(c or Zero)
  def american(d1: Date, d2: Date, c: Contract): Contract = anytime(between(d1, d2))(c)
  def swap(give: Contract, take: Contract) = take andGive give
}
