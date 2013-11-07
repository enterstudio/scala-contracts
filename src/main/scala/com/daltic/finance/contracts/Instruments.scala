package com.daltic.finance.contracts

object Instruments {
  import Contracts._

  def zero: Contract = Zero

  def money(n: Obs[BigDecimal], c: Currency): Contract =
    scale(n)(One(c))

  def zeroCouponBond(d: Date, n: Obs[BigDecimal], c: Currency)(implicit int: TimeContext): Contract =
    when(at(d))(money(n, c))

  def couponBond(d: Date, par: BigDecimal, c: Currency, s: TimeStep,
      steps: Int, rate: Obs[BigDecimal])(implicit int: TimeContext): Contract = {
    (1 until steps).foldLeft(zeroCouponBond(d, par, c)) { (contract, i) =>
      contract and zeroCouponBond(d - i*s, rate*par, c)
    }
  }
  def callable(principal: Contract, callPrices: Map[Date, Contract],
      contract: Contract)(implicit tc: TimeContext): Contract = {
    callPrices.foldLeft(contract) { case (c, (d, p)) =>
      c andGive european(d, c andGive (principal and p))
    }
  }

  def puttable(principal: Contract, putPrices: Map[Date, Contract],
      contract: Contract)(implicit tc: TimeContext): Contract = {
    putPrices.foldLeft(contract) { case (c, (d, p)) =>
      c and european(d, principal andGive (p and c))
    }
  }

  def european(d: Date, c: Contract)(implicit tc: TimeContext): Contract =
    when(at(d))(c or zero)

  def american(i: Interval, c: Contract)(implicit tc: TimeContext): Contract =
    anytime(in(i))(c)

  def swap(give: Contract, take: Contract) =
    take andGive give
}
