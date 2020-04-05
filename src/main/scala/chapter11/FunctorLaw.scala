package chapter11
import org.scalatest.Assertions._

object FunctorLaw {

  def identityLaw[A, F[_]](as: F[A])(implicit F: Functor[F]) = {
    assert(F.map(as)(identity) == as)
  }

}
