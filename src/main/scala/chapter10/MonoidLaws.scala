package chapter10

import org.scalatest.Assertions.assert
import chapter10.MonoidType._

object MonoidLaws {

  def associativeLaw[A](x: A, y: A, z: A)(implicit M: Monoid[A]): Unit = {
    assert(M.op(M.op(x, y), z) == M.op(x, M.op(y, z)))
  }

  def associativeLawAny[A](x: A, y: A, z: A)(equal: (A, A) => Boolean) (implicit M: Monoid[A]): Unit = {
    assert(equal(M.op(M.op(x, y), z), M.op(x, M.op(y, z))))
  }

  def zeroLaw[A](x: A)(implicit M: Monoid[A]): Unit = {
    assert(M.op(x, M.zero) == x)
    assert(M.op(M.zero, x) == x)
  }

  def zeroLawAny[A](x: A)(equal: (A, A) => Boolean)(implicit M: Monoid[A]): Unit = {
    assert(equal(M.op(x, M.zero), x))
    assert(equal(M.op(M.zero, x), x))
  }

  def testLaws[A: Monoid](x: A, y: A, z: A): Unit = {
    associativeLaw(x, y, z)
    zeroLaw(x)
    zeroLaw(y)
    zeroLaw(z)
  }

  def testLawsAny[A: Monoid](x: A, y: A, z: A)(equal: (A, A) => Boolean): Unit = {
    associativeLawAny(x, y, z)(equal)
    zeroLawAny(x)(equal)
    zeroLawAny(y)(equal)
    zeroLawAny(z)(equal)
  }
}
