package lectures.part1intro

import cats.Eval

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Intro extends App {
  // 1. FUTURE GLOGAL
  // Global not supporting nested futures + her is some control on thread pool size. Use this.
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(5))
  Future(32)

  // 2. HIGHER KINDED TYPES
  trait SeqChecker[F[_]] {
    def isSeq: Boolean
  }
  val someChecker = new SeqChecker[List] { // Can be just List, as it is Higher Kinded type. List of anything.
    override def isSeq: Boolean = true
  }

  // 3. CATS
  // check if its work
  val out = Eval.later {
    println(42)
    "Some text"
  }

  println(out.value)

  // if any imports under question, just use:
  import cats._
  import cats.implicits._
}
