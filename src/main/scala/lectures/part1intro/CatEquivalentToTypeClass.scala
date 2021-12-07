package lectures.part1intro

object CatEquivalentToTypeClass extends App {

  // 1. type class

  import cats.Eq

  // 2. instance

  import cats.instances.int._

  // 3. usr type class instance

  val intEquality = Eq[Int]
  val aComparsion = intEquality.eqv(2,3)

  // 4. use extended methods by implicit class

  import cats.syntax.eq._
  val anotherComp = 2 === 3
  val andAnotherComp = 2 =!= 3

  // extend to futher composite types

  import cats.instances.list._
  val listComp = List(1) === List(1)

  // or make one yourself

  case class ExampleThing(name: String)
  implicit val eThingComp: Eq[ExampleThing] = Eq.instance[ExampleThing] { (a, b) =>
    a.name == b.name
  }
  val twoThingsComp = ExampleThing("abc") === ExampleThing("abc")

  // if any imports under question, just use:
  import cats._
  import cats.implicits._
}
