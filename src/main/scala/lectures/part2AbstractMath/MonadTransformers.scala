package lectures.part2AbstractMath

import java.util.concurrent.{Executor, Executors}
import scala.concurrent.{ExecutionContext, Future}

object MonadTransformers extends App {

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(6))

  import cats.data.OptionT
  import cats.instances.list._ // fetch OptionT[List]
  import cats.instances.future._

  // ORINARY
  // List((Some(1),Some(a)), (Some(1),Some(b)), (Some(1),None), (Some(2),Some(a)), (Some(2),Some(b)), (Some(2),None))
  val numOptionsList = List(Option(1),    Option(2))
  val chaOptionsList = List(Option('a'), Option('b'), Option.empty[Char])
  println {
    for {
      n <- numOptionsList
      c <- chaOptionsList
    } yield (n, c)
  }

  // VS

  // OPTIONT
  // OptionT(List(Some((1,a)), Some((1,b)), None, Some((2,a)), Some((2,b)), None))
  val numOptions:  OptionT[List, Int]  = OptionT(List(Option(1),    Option(2)))
  val chaOptions:  OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option.empty[Char]))
  println {
    for {
      n <- numOptions
      c <- chaOptions
    } yield (n, c)
  }


  // EITHER
  import cats.data.EitherT
  // lists
  val listOfEithers: EitherT[List, String, Int] = EitherT(List(Left("Smth wrong"), Right(32), Right(15)))
  // futures
  // val futreEithers: EitherT[Future, String, Int] = EitherT(Future[Either[String, Int]](Right(15)))
  // or
  val futreEithers: EitherT[Future, String, Int] = EitherT.right(Future(15))


  // EXERCISE
  val bandw = Map(
    "server1" -> 50,
    "server2" -> 300,
    "server3" -> 170
  )

  type AsyncResp[T] = EitherT[Future, String, T]

  def getBandw(server: String): AsyncResp[Int] = bandw.get(server) match {
    case None => EitherT(Future[Either[String, Int]](Left("Server not exist")))
    case Some(v) => EitherT(Future[Either[String, Int]](Right(v)))
  }

  def canWithstandSurge(s1: String, s2: String): AsyncResp[Boolean] =
    for {
      s1b <- getBandw(s1)
      s2b <- getBandw(s2)
    } yield (s1b + s2b < 250)

  // transfer Boolean Either to String Either
  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResp[String] = {
    canWithstandSurge(s1, s2).transform {
      case Left(v) => Left(s"Server error $v")
      case Right(false) => Left("Cannot withstand bandwidth")
      case Right(true) => Right("Can cope")
    }
  }

  println {
    generateTrafficSpikeReport("server1", "server3").value map println
  }


}
