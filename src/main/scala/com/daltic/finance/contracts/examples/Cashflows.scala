package com.daltic.finance.contracts.examples

import com.daltic.finance.contracts._

import Contracts._
import ExampleModel._

object Cashflows {
  import Zip._

  def cashflow(model: Model, k: Currency, steps: Int): Contract => PR[BigDecimal] = {
    def eval(contract: Contract): PR[BigDecimal] = contract match {
      case Zero            => bigK(0: BigDecimal)
      case One(k2)         => model.exch(k)(k2)
      case Give(c)         => bigK(-1.0: BigDecimal) * eval(c)
      case Scale(o, c)     => evalO(o, model.modelStart) * eval(c)
      case And(c1, c2)     => eval(c1) %+ eval(c2)
      case Or(c1, c2)      => max(eval(c1), eval(c2))
      case Cond(o, c1, c2) => condPr(evalO(o, model.modelStart), eval(c1), eval(c2))
      case When(o, c)      => cat(k, evalO(o, model.modelStart), eval(c), steps)
      case Until(o, c)     => absorb(k, evalO(o, model.modelStart), eval(c))
      //      eval (Anytime o c)  = snell  k (evalO o, eval c)
      case _               => sys.error("todo")
    }
    eval _
  }

  def cat(k: Currency, bs: PR[Boolean], rs: PR[BigDecimal], steps: Int): PR[BigDecimal] = {
     PR(catCalc(bs.unPr, rs.unPr, steps))
  }

  private def catCalc(b: Stream[RV[Boolean]], p: Stream[RV[BigDecimal]], steps: Int): Stream[RV[BigDecimal]] = {
    val (bRv #:: bs) = b
    val (pRv #:: ps) = p
    if (steps == 0) {
      Stream(pRv)
    } else {
      val rest = catCalc(bs, ps, steps - 1)
      val thisSlice = zipWith(bRv, pRv)((b, p) => if (b) p else 0.0: BigDecimal)
      thisSlice #:: rest
    }
  }
}
