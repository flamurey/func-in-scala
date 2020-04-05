package chapter10
import MonoidType._

object FoldableType {
  trait Foldable[F[_]] {
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
    def concatenate[A](as: F[A])(m: Monoid[A]): A =
      foldLeft(as)(m.zero)(m.op)

    def toList[A](fa: F[A]): List[A] = {
      foldRight(fa)(List.empty[A])(_ :: _)
    }
  }

  val listFoldable: Foldable[List] = new Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
      as.map(f).fold(mb.zero)(mb.op)
  }

  val indexedSeqFoldable: Foldable[IndexedSeq] = new Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
      as.map(f).fold(mb.zero)(mb.op)
  }

  val optionFoldable: Foldable[Option] = new Foldable[Option] {
    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
      as.fold(z)(v => f(v, z))

    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
      as.fold(z)(v => f(z, v))

    override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
      as.fold(mb.zero)(v => f(v))
  }
}
