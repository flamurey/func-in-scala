package other

object FunctionCovariance {

  class A
  class AA extends A
  class B
  class BB extends B
  class C
  class CC extends C

  var base: (A => BB) => C = ???

  var sub: (AA => B) => CC = ???

  base = sub

  var base2: AA => (BB => C) = ???

  var sub2: A => (B => CC) = ???

  base2 = sub2
}
