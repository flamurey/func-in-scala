package chapter10
import org.scalatest.Assertions._

import scala.util.Random

object MonoidType {
  trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }

  val stringMonoid: Monoid[String] = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2
    override def zero: String = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    override def zero: List[A] = Nil
  }

  def optionMonoid[A](implicit M: Monoid[A]): Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = (a1, a2) match {
      case (Some(v1), Some(v2)) => Some(M.op(v1, v2))
      case (Some(v1), None)     => Some(v1)
      case (None, Some(v2))     => Some(v2)
      case (None, None)         => None
    }
    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(f1: A => A, f2: A => A): A => A = {
      x: A => f2(f1(x))
    }
    override def zero: A => A = identity
  }
}

object MonoidLaws {
  import chapter10.MonoidType._

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

object Run extends App {
  import MonoidType._
  import MonoidLaws._

  testLaws[String]("A", "B", "C")(stringMonoid)
  testLaws[List[Int]](List(1, 2), List(3), List(4))(listMonoid)
  testLaws[Option[String]](Some("A"), Some("B"), Some("C"))(optionMonoid(stringMonoid))
  testLaws[Option[String]](Some("A"), None, Some("C"))(optionMonoid(stringMonoid))

  val add2 = (x: Int) => x + 2
  val add3 = (x: Int) => x + 3
  val add5 = (x: Int) => x + 5

  testLawsAny[Int => Int](add2, add5, add3) { (f1, f2) =>
    val seed = Random.nextInt()
    f1(seed) == f2(seed)
  }(endoMonoid)


  print("All monoid law test passed")
}
