package chapter13
import scala.io.StdIn._

object Run extends App {

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0/9.0

  def ReadLine: IO[String] = IO { readLine }
  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }
  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- IO { 3 } //ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()
  converter.run

  val fz: Int => TailRec[Int] = (i: Int) => Return(i)
  val f: Int => TailRec[Int] = (i: Int) => Return(i + 1)

  val g: Int => TailRec[Int] =
    List.fill(100)(f).foldLeft(fz){
      (a: (Int) => TailRec[Int],
       b: (Int) => TailRec[Int]) => {
        (x: Int) => TailRec.suspend(0, a(x).flatMap(b))
      }
    }

  val gFortyTwo = g(42)
  println("g(42) = " + gFortyTwo)
  println("run(g(42)) = " + TailRec.run(gFortyTwo))
}
