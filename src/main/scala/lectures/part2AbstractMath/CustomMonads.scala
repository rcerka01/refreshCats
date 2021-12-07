package lectures.part2AbstractMath

import scala.annotation.tailrec

object CustomMonads extends App {

  import cats.Monad

  // for Option
  implicit object OptionMonad extends Monad[Option] {
    override def pure[A](x: A): Option[A] = Option(x)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f) // option already has flatMap
    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case None => None
      case Some(Left(v)) => tailRecM(v)(f)
      case Some(Right(v)) => Some(v)
    }
  }

  // for CustomType
  type Identity[T] = T
  val example: Identity[Int] = 34

  implicit object IdentityMonad extends Monad[Identity] {
    override def pure[A](x: A): Identity[A] = x
    override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)
    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = ???
  }

  // for binary tree
  sealed trait Tree[+A]
  final case class Leaf[+A](value: A) extends Tree[A]
  final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  implicit object TreeMonad extends Monad[Tree] {
    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(x) => f(x)
      case Branch(l, r) => Branch(flatMap(l)(f),flatMap(r)(f))
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def stackRec(t: Tree[Either[A, B]]): Tree[B] = t match {
        case Leaf(Left(v)) => stackRec(f(v))
        case Leaf(Right(v)) => Leaf(v)
        case Branch(l,r) => Branch(stackRec(l), stackRec(r))
      }
      stackRec(f(a))
    }
  }

  val tree = Branch(Leaf(10), Leaf(20))
  println {
    TreeMonad.flatMap(tree)(x => Branch(Leaf(x + 1), Leaf(x + 2)))
  }
}
