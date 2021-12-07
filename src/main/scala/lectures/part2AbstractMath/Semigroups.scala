package lectures.part2AbstractMath

object Semigroups extends App {

  import cats.Semigroup

  // for Int
  import cats.instances.int._
  val naturalIntSemigroup = Semigroup[Int]
  val intCombination = naturalIntSemigroup.combine(2,44)

  // for String
  import cats.instances.string._
  val naturalStringSemigroup = Semigroup[String]
  val stringCombination = naturalStringSemigroup.combine("Hi", "There")

  // use e.g. to reduce list
  // universal case
  def reduceThings[T](l: List[T])(implicit semigroup: Semigroup[T]): T = l reduce semigroup.combine
  println {
    val numbersList = (1 to 10).toList
    reduceThings(numbersList)
  }
  println {
    val stringList = List("Hallo, ", "from ", "there")
    reduceThings(stringList)
  }
  println {
    import cats.instances.option._
    val stringList = List(Some("Hallo, "), None, Some("from "), Some("there"))
    reduceThings(stringList)
  }

  // for custom case
  case class Expense(id: Long, amount: Double)
  implicit val eThingComp: Semigroup[Expense] = Semigroup.instance[Expense] { (a, b) =>
    Expense(a.id + b.id, a.amount + b.amount)
  }
  println {
    val expensesList = List(Expense(12, 34), Expense(15, 78), Expense(14, 28))
    reduceThings(expensesList)
  }

  // extension methods (via implicit type class) e.g. |+|
  import cats.syntax.semigroup._ // must have
  1 |+| 2
  "Hallo" |+| "there"
  def reduceThings2[T](l: List[T])(implicit semigroup: Semigroup[T]): T = l reduce(_ |+| _)
  println {
    val numbersList = (1 to 10).toList
    reduceThings2(numbersList)
  }
}
