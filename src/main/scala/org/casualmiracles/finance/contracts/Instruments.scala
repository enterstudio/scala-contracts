package org.casualmiracles.finance.contracts

object Instruments {
  import Contracts._

  def zeroCouponBond(d: Date, n: BigDecimal, c: Currency): Contract = zeroCouponBond(d, n: Obs[BigDecimal], c)
  def zeroCouponBond(d: Date, n: Obs[BigDecimal], c: Currency): Contract = when(at(d))(scale(n)(One(c)))
  def couponBond(d: Date, par: BigDecimal, c: Currency, s: TimeStep, steps: Int, rate: BigDecimal): Contract =
    couponBond(d, par, c, s, steps, rate: Obs[BigDecimal])
  def couponBond(d: Date, par: BigDecimal, c: Currency, s: TimeStep, steps: Int, rate: Obs[BigDecimal]): Contract =
    (1 until steps).foldLeft(zeroCouponBond(d, rate, c)) { (contract, i) =>
      contract and zeroCouponBond(d - i*s, rate*par, c)
    }
  def european(d: Date, c: Contract): Contract = when(at(d))(c or Zero)
  def american(d1: Date, d2: Date, c: Contract): Contract = anytime(between(d1, d2))(c)
  def swap(give: Contract, take: Contract) = take andGive give
}
