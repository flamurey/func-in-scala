package chapter11

trait Monad[F[_]] extends Functor[F] {

  def unit[A](a: => A): F[A]
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] = {
    lma.foldLeft(unit(List.empty[A])) { (fla, fa) =>
      map2(fa, fla)(_ :: _)
    }
  }

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = {
    la.foldLeft(unit(List.empty[B])) { (flb, a) =>
      map2(f(a), flb)(_ :: _)
    }
  }

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    sequence(List.fill(n)(ma))
  }

  def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def filterM[A](la: List[A])(f: A => F[Boolean]): F[List[A]] = {
    la.foldLeft(unit(List.empty[A])) { (fla, a) =>
      map2(f(a), fla) { (passed, la) =>
        if (passed) a :: la else la
      }
    }
  }

  def findM[A](la: List[A])(f: A => F[Boolean]): F[Option[A]] = {
    val z = Option.empty[A]
    la.foldLeft(unit(z)) { (foa, a) =>
      flatMap(foa) { oa =>
        if (oa.isEmpty) {
          map(f(a))(passed => if (passed) Some(a) else z)
        } else {
          foa
        }
      }
    }
  }

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = {
    a: A => flatMap(f(a))(g)
  }

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(identity)
}

object Monad {
  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] =
      ma.flatMap(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Option(a)
    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma.flatMap(f)
  }

  implicit class Monadify[A, F[_]](monad: F[A])(implicit M: Monad[F]) {
    def map[B](f: A => B): F[B] = M.map(monad)(f)
    def flatMap[B](f: A => F[B]): F[B] = M.flatMap(monad)(f)
  }
}
