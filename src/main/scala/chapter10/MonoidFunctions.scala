package chapter10

object MonoidFunctions {
  import MonoidType._

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.zero)((acc, a) => m.op(acc, f(a)))
  }

  def foldLeft[A, B](as: List[A], m: Monoid[B])(z: B)(f: A => B): B = {
    m.op(z, foldMap(as, m)(f))
  }

  def foldRight[A, B](as: List[A], m: Monoid[B])(z: B)(f: A => B): B = {
    m.op(foldMap(as, m)(f), z)
  }
}
