package lectures.part2AbstractMath

object Monoids extends App {

  import cats.Monoid

  import cats.instances.int._
  import cats.instances.option._

  import cats.syntax.monoid._

  // MONOID IS DEFINING (OR EMPTY) ELEMENT OF T
  // It extends Semigroup with an empty value

  // combine method is associative:
  val numbers = (1 to 100).toList
  // same result here. And 0 is actually Monoid
  numbers.foldRight(0)(_ |+| _)
  numbers.foldLeft(0)(_ |+| _)

  // 0 previously could be monoid:
  val intMonoid = Monoid[Int]
  val zero = intMonoid.empty

  // OPTIONS
  println {
    val combOption = Monoid[Option[Int]].combine(Option(2), Monoid[Option[Int]].empty)
    combOption
  } // Some(2)
  Option(1) |+| Option(2)

  // COMMON FOLD CASE
  def combineFold[T](l: List[T])(implicit monoid: Monoid[T]): T = {
    val empty: T = monoid.empty
    l.foldLeft (empty) (_ |+| _)
  }
  println {
    combineFold((1 to 10).toList)
  }


  // EXERCISES
  // 1. With Map.
  // (trick to find the correct import)
  val phonebooks = List(
    Map("bob" -> 123, "ray" -> 345),
    Map("larisa" -> 789)
  )
  println {
    import cats.instances.map._
    combineFold(phonebooks)
  }

  // 2. With custom case class
  // trick is, when define Monoid.instance, check for its signature. And pass parameters required (they are different for different instances).
  case class ShoppingCart(items: List[String], total: Double)
  val empty = ShoppingCart(List(), 0)
  val comb = { (a: ShoppingCart, b: ShoppingCart) => ShoppingCart(a.items ++ b.items, a.total + b.total) }
  implicit val shoppingCartMonoid: Monoid[ShoppingCart] = Monoid.instance(empty, comb)
  def checkout(carts: List[ShoppingCart]): ShoppingCart =
    combineFold(carts)
  println {
    val listToTest: List[ShoppingCart] = List(ShoppingCart(List("irem1", "item2"), 13), ShoppingCart(List("otherItem"), 42))
    checkout(listToTest)
  }
}
