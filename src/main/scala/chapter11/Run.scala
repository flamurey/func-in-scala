package chapter11
import Monad._

object Run extends App {
  val ints = List(1, 3, 5, 9)
  val filterM = (number: Int) => if (number == 5) Some(true) else Some(false)

  val result = optionMonad.filterM(ints)(filterM)
  println(s"FilterM for $ints: $result")

  val findM = (number: Int) => {
    println(s"Execute findM on $number")
    if (number == 5) Some(true) else Some(false)
  }
  println(s"FindM for $ints: " + optionMonad.findM(ints)(findM))

  implicit val idMonad = Id
  val hm = for {
    h <- Id("Hello, ")
    m <- Id("Monada!")
  } yield h + m
  println(hm.value)

  implicit val stringReader = Reader.readerMonad[String]

  val r2 = Reader[String, String](_ + " Second read")
  val reading = for {
    increased <- Reader[String, Int](_.toInt + 1)
    v2 <- Reader[String, String](s => s"Raw number: $s. Increased: $increased")
  } yield v2
  print("Reading '3': " + reading.run("3"))
}
