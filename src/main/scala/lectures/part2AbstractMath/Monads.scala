package lectures.part2AbstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future}

object Monads extends App {

  // lists
  val numberList = List(1,2,3)
  val charsList = List("a", "b", "c")

  println {
    numberList flatMap { number =>
      charsList map { char =>
        number + char
      }
    }
  }

  // options
  val numberOption = Option(2)
  val charOption = Option("a")

  println {
    numberOption flatMap { number =>
      charOption map { char =>
        Option(number + char)
      }
    }
  }

  // futures
  implicit val ec: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(5))
  val futureInt = Future(42)
  val futureChar = Future('a')

  println {
    futureInt flatMap { number =>
      futureChar map { char =>
        number + char
      }
    }
  }

  // MONAD
  // - wrapping value into M value
  // - the flatMap mechanism

  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)( x => pure(f(x)) )
  }

  // CATS MONAD
  import cats.Monad

  // options
  import cats.instances.option._
  val optionMonad = Monad[Option]
  val anOption = optionMonad.pure(4)
  val transformedMonad = optionMonad.flatMap ( anOption )( x => if ( x % 2 == 0 ) Some(x + 1) else None)

  // list
  import cats.instances.list._
  val listMonad = Monad[List]
  val list = listMonad.pure(4)
  val transformedListMonad = listMonad.flatMap ( list )( x => List(x, x + 1))

  // future
  import cats.instances.future._
  val futureMonad = Monad[Future]
  val future = futureMonad.pure(7)
  val transformedFutureMonad = futureMonad.flatMap ( future )( x => Future(x + 1))

  // GENERALISE
  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]) =
    monad.flatMap(ma)(x => monad.map(mb)(y => (x, y)))


  println {
      getPairs(numberList, charsList)
    }
    println {
      getPairs(numberOption, charOption)
    }
    getPairs(futureInt, futureChar) map println

  // EXTENSION METHODS
  import cats.syntax.applicative._
  val axtOption = 1.pure[Option]
  val axtList = "a".pure[List]
  // flatMap would be added for types which naturally don't have it (my own).

  // EXERCISE shorter get pairs function
  import cats.syntax.functor._ // map is here
  import cats.syntax.flatMap._ // flat map is here

  // def getPairsShorter[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]) =
  // can be
  def getPairsShorter[M[_] : Monad, A, B](ma: M[A], mb: M[B]) =
    for {
      a <- ma
      b <- mb
    } yield (a, b)

}
