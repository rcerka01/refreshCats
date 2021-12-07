package lectures.part2AbstractMath

import scala.util.Try

object Functors extends App {

  // it is:
  trait MyFunctor[F[_]] {
    def map[A, B](initValue: F[A])(f: A => B): F[B]
  }

  // CATS FUNCTOR
  import cats.Functor

  // LIST
  import cats.instances.list._ // include Functor[List]
  val listFunctor = Functor[List]
  listFunctor.map(List(1,2,3))( _ + 1 )

  // OPTION
  import cats.instances.option._ // include Functor[List]
  val optionFunctor = Functor[Option]
  optionFunctor.map(Some(2))( _ + 1 )

  // TRY
  import cats.instances.try_._ // include Functor[List]
  val tryFunctor = Functor[Try]
  tryFunctor.map(Try(2))( _ + 1 )

  // GENERAL
  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)
  println {
    println {
      println {
        do10x(Try(2))
      }
      do10x(Option(3))
    }
    do10x(List(1,2,3))
  }




  // EXERCISE 1
  trait Tree[+T]
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]
  object Tree {
    def leaf[T](value: T): Leaf[T] = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]): Branch[T] = Branch(value, left, right)
  }

  implicit object TreeObject extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(v) => Leaf(f(v))
      case Branch(v, l, r) => Branch(f(v), map(l)(f), map(r)(f))
    }
  }

  // test
  import cats.syntax.functor._
  val tree: Tree[Int] = Tree.branch(40,
    Tree.branch(3, Tree.leaf(10), Tree.leaf(30)),
    Tree.leaf(20))
  println {
    tree.map(_ + 1)
  }

  // EXERCISE 2
  // shorter version of:
  // def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)
  def do10x2[F[_] : Functor](container: F[Int]): F[Int] = container.map(_ * 10)
  // in type declaration that means there is implicit Functor (context bound).
  println {
    do10x2(List(1,2,3))
  }
}
