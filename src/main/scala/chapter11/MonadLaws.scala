package chapter11

import org.scalatest.Assertions.assert

object MonadLaws {

  def associativeLaw[A, F[_], C, B](as: F[A], f: A => F[B], g: B => F[C])(implicit M: Monad[F]) = {
    val left = M.flatMap(M.flatMap(as)(f))(g)
    val right = M.flatMap(as)(a => M.flatMap(f(a))(g))
    assert(left == right)
  }

  def associativeLawKleishi[F[_], A, C, B, D](f: A => F[B], g: B => F[C], h: C => F[D])(implicit M: Monad[F]) = {
    val left = M.compose(M.compose(f, g), h)
    val right = M.compose(f, M.compose(g, h))
    assert(left == right)
  }

  def identity[F[_], A, B](f: A => F[B])(implicit M: Monad[F]) = {
    assert(M.compose(f, M.unit[B]) == f)
    assert(M.compose(M.unit[A], f) == f)
  }
}
