package chapter12

import chapter11.Functor

trait Applicative[F[_]] extends Functor[F] {
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val bc = (a: A) => f(a, _)
    val fbc = apply(unit(bc))(fa)
    apply(fbc)(fb)
  }

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    val abcToD = (a: A) => (b: B) => f(a, b, _)
    val fabcToD = unit(abcToD)
    val fbcToD = apply(fabcToD)(fa)
    val fcToD = apply(fbcToD)(fb)
    apply(fcToD)(fc)
  }

  def lift[A, B](f: A => B): F[A] => F[B] = {
    fa => map(fa)(f)
  }

  def lift[A, B, C](f: (A, B) => C): (F[A], F[B]) => F[C] = {
    (fa, fb) => map2(fa, fb)(f)
  }

  def lift[A, B, C, D](f: (A, B, C) => D): (F[A], F[B], F[C]) => F[D] = {
    (fa, fb, fc) => map3(fa, fb, fc)(f)
  }

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def tupled[R, A, B](f: (F[A], F[B]) => F[R]): F[(A, B)] => F[R] =
    mab => f(map(mab)(_._1), map(mab)(_._2))

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def sequence[A](lma: List[F[A]]): F[List[A]] = {
    lma.foldLeft(unit(List.empty[A])) { (fla, fa) =>
      map2(fa, fla)(_ :: _)
    }
  }

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    sequence(List.fill(n)(ma))
  }

  def productA[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: => A): (F[A], G[A]) = (Applicative.this.unit(a), G.unit(a))

      override def apply[A, B](fab: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) = {
        (Applicative.this.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))
      }
    }
  }

  def composeA[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def apply[A, B](fab: F[G[A => B]])(fa: F[G[A]]): F[G[B]] = {
        val A = Applicative.this
        A.map2(fab, fa)((gab, ga) => G.map2(gab, ga)((ab, a) => ab(a)))
      }

      override def unit[A](a: => A): F[G[A]] = Applicative.this.unit(G.unit(a))
    }
  }

}

object Applicative {
  val optionA: Applicative[Option] = new Applicative[Option] {
    override def apply[A, B](fab: Option[A => B])(fa: Option[A]): Option[B] = {
      fa match {
        case Some(a) =>
          fab match {
            case Some(ab) =>
              Some(ab(a))
            case None => Option.empty[B]
          }
        case None => Option.empty[B]
      }
    }

    override def unit[A](a: => A): Option[A] = Option(a)
  }
  val listA: Applicative[List] = new Applicative[List] {
    override def apply[A, B](fab: List[A => B])(fa: List[A]): List[B] = {
      fa.flatMap(a => fab.map(f => f(a)))
    }

    override def unit[A](a: => A): List[A] = List(a)
  }
}