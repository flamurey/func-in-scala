package chapter12

import chapter11.Functor

trait Map2Applicative[F[_]] extends Functor [F] {
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  def unit[A](a: => A): F[A]

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fa, fab)((a, ab) => ab(a))

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

}
