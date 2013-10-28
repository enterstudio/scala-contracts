package com.daltic.finance.contracts

import java.math.MathContext

trait PRs {
  import Zip._

  val DefaultMathContext = new MathContext(5)

  def max[T : Numeric](pra: PR[T], prb: PR[T]): PR[T] =
    lift2Pr((a: T, b: T) => implicitly[Numeric[T]].max(a, b), pra, prb)

  def condPr[T](aPr: PR[Boolean], bPr: PR[T], cPr: PR[T]): PR[T] =
    lift3Pr((b: Boolean, tru: T, fal: T) => if (b) tru else fal, aPr, bPr, cPr)

  def liftPr[A, B](f: A => B, pr: PR[A]): PR[B] = PR(pr.unPr.map(_ map f))

  def lift2Pr[A, B, C](f: (A, B) => C, aPr: PR[A], bPr: PR[B]): PR[C] = {
    val rvF = (rvA: RV[A], rvB: RV[B]) => zipWith(rvA, rvB)(f)
    PR(zipWith(aPr.unPr, bPr.unPr)(rvF))
  }

  def lift2PrAll[A](f: (A, A) => A, aPr: PR[A], bPr: PR[A]): PR[A] = {
    val rvF = (rvA: RV[A], rvB: RV[A]) => zipWith(rvA, rvB)(f)
    PR(zipWithAll(rvF, aPr.unPr, bPr.unPr))
  }

  def lift3Pr[A, B, C, D](f: (A, B, C) => D, aPr: PR[A], bPr: PR[B], cPr: PR[C]): PR[D] = {
    val rvF = (rvA: RV[A], rvB: RV[B], rvC: RV[C]) => zipWith3(rvA, rvB, rvC)(f)
    PR(zipWith3(aPr.unPr, bPr.unPr, cPr.unPr)(rvF))
  }

  def printPr(pr: PR[_], n: Int)(implicit mc: MathContext = DefaultMathContext) =
    for ((rv, n) <- pr.unPr.take(n).zipWithIndex) {
      val values = rv map {
        case b: BigDecimal => b.round(mc)
        case a => a
      }
      println(s"$n: ${values mkString " "}")
    }
}

object PR extends PRs {
  implicit class PrOps[T](val prA: PR[T])(implicit n: Numeric[T]) {
    def %+(prB: PR[T]): PR[T] = lift2PrAll(n.plus, prA, prB)
    def -(prB: PR[T]): PR[T] = lift2PrAll(n.minus, prA, prB)
    def *(prB: PR[T]): PR[T] = lift2PrAll(n.times, prA, prB)
    def abs: PR[T] = liftPr(n.abs, prA)
    def signum: PR[Int] = liftPr(n.signum, prA)
  }
}

case class PR[A](unPr: Stream[RV[A]])
