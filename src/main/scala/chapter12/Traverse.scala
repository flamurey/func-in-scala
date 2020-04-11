package chapter12

import chapter11.Functor

trait Traverse[F[_]] extends Functor[F] {

  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = {
    sequence(map(fa)(f))
  }

  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)
}

object Traverse {
  val optionT: Traverse[Option] = new Traverse[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

    override def traverse[G[_] : Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = {
      val A = implicitly[Applicative[G]]
      fa match {
        case Some(a) => A.map(f(a))(Some(_))
        case None => A.unit(None)
      }
    }
  }

  val listT: Traverse[List] = new Traverse[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

    override def traverse[G[_] : Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = {
      val G = implicitly[Applicative[G]]
      fa.foldLeft(G.unit(List.empty[B])) {(flb, a) =>
        G.map2(f(a), flb)(_ :: _)
      }
    }
  }
}
