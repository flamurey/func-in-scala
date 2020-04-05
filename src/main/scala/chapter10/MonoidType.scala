package chapter10

object MonoidType {
  trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }

  val intMonoid: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
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

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) = (a1, a2) match {
      case ((a1, b1), (a2, b2)) => (A.op(a1, a2), B.op(b1, b2))
    }

    override def zero: (A, B) = (A.zero, B.zero)
  }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B]{
    override def op(f1: A => B, f2: A => B): A => B = {
      x: A => B.op(f1(x), f2(x))
    }

    override def zero: A => B = {
      x: A => B.zero
    }
  }
}
