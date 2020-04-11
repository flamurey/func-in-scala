package chapter12

object Run extends App {

  def f(x: Int) = Some(() => {
    println(s"Getting $x")
    x
  })

  implicit val AO = Applicative.optionA
  AO.map3(f(3), Option.empty[() => Int], f(5))((x, y, z) => s"Sum: ${x() + y() + z()}")

  case class Person(name: String, salary: Int)
  def format(name: String, salary: Int) = s"$name have follow salary: $salary"

  val person = Some(Person("Ivan", 100))
  println(AO.lift(format _)(person.map(_.name), person.map(_.salary)))
  println(AO.lift((format _).tupled)(person.map(p => p.name -> p.salary)))
  println(AO.tupled(AO.lift(format _))(person.map(p => p.name -> p.salary)))

  val AL = Applicative.listA
  val ALO  = AL.composeA(AO)

  val flo2s = List(Option((x: Int) => s"Power: ${x * x}"), None)
  val flo = List(Option(2), Option(3), None)
  println(ALO.apply(flo2s)(flo))

  val listT = Traverse.listT

  println(listT.sequence(List[Option[Int]](Some(1), Some(2))))

}
