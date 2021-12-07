package lectures.part1intro

object Variance extends App {

  import cats.Eq
  import cats.instances.int._
  import cats.instances.option._
  import cats.syntax.eq._

  val aComp = Option(2) === Option(2)

  //val aProblem = Some(2) === None // variance problem

  // RULE FOR VARIANCE - "HAS a T" covariant, "ACT on T" contravariant.

  // CATS USE INVARIANCE (!)

  // to solve problem:
  Option(2) === Option.empty
}
