package chapter10

import scala.util.Random

object Run extends App {
  import MonoidType._
  import MonoidLaws._

  testLaws[String]("A", "B", "C")(stringMonoid)
  testLaws[List[Int]](List(1, 2), List(3), List(4))(listMonoid)
  testLaws[Option[String]](Some("A"), Some("B"), Some("C"))(optionMonoid(stringMonoid))
  testLaws[Option[String]](Some("A"), None, Some("C"))(optionMonoid(stringMonoid))

  val add2 = (x: Int) => x + 2
  val add3 = (x: Int) => x + 3
  val add5 = (x: Int) => x + 5

  testLawsAny[Int => Int](add2, add5, add3) { (f1, f2) =>
    val seed = Random.nextInt()
    f1(seed) == f2(seed)
  }(endoMonoid)

  val add2S = (x: Int) => (x + 2).toString
  val add3S = (x: Int) => (x + 3).toString
  val add5S = (x: Int) => (x + 5).toString

  testLawsAny[Int => String](add2S, add3S, add5S) { (f1, f2) =>
    val seed = Random.nextInt()
    f1(seed) == f2(seed)
  }(functionMonoid(stringMonoid))

  println("All monoid law test passed")

  import MonoidFunctions._

  val intList = List(1, 3, 5)
  println(s"$intList concatenated to: " + concatenate(intList, intMonoid))

  println(s"$intList foldLeft with z=L: " + foldLeft(intList, stringMonoid)("L")(x => x.toString))
  println(s"$intList foldRight with z=R: " + foldRight(intList, stringMonoid)("R")(x => x.toString))

  import FoldableType._

  println(s"Foldable: $intList foldLeft with z=L: " + listFoldable.foldLeft(intList)("L")((acc, x) => acc + x.toString))

}
