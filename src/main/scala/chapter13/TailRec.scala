package chapter13

sealed trait TailRec[A] {
  def flatMap(f: A => TailRec[A]): TailRec[A] =
    FlatMap(this, f)

  def map(f: A => A): TailRec[A] =
    flatMap(f andThen (Return(_)))
}

case class Return[A](a: A) extends TailRec[A]

case class Suspend[A](resume: () => A) extends TailRec[A]

case class FlatMap[A](sub: TailRec[A], k: A => TailRec[A]) extends TailRec[A]

object TailRec {
  def unit[A](a: => A): TailRec[A] = Return(a)

  def flatMap[A](a: TailRec[A])(f: A => TailRec[A]): TailRec[A] = a flatMap f

  def suspend[A](zero: A, a: => TailRec[A]): TailRec[A] =
    Suspend(()  => zero).flatMap(_ => a)

  @annotation.tailrec def run[A](t: TailRec[A]): A = {
    t match {
      case Return(a) => a
      case Suspend(r) => r()
      case FlatMap(x, f) => x match {
        case Return(a) => run(f(a))
        case Suspend(r) => run(f(r()))
        case FlatMap(y, g) => run(flatMap(y)(a => flatMap(g(a))(f)))
      }

    }
  }
}


