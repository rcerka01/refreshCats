package lectures.part2AbstractMath

object UsingMonads extends App {

  import cats.Monad
  import cats.instances.list._
  val monadList = Monad[List]

  // EITHER
  val someOrinaryEither: Either[String, Int] = Right(42)

  // typed
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  // instances
  import cats.instances.either._
  val monadEither = Monad[LoadingOr]
  val anEither = monadEither.pure(46)
  println {
    monadEither.flatMap (anEither)(x => if (x % 2 == 0) Right(x) else Left(s"Not an odd $x"))
  }

  // EXERCISE 1
  case class OrderStatus(id: Long, status: String)
  def getOrderStatus(id: Long): LoadingOr[OrderStatus] = Right(OrderStatus(id, "Ready to ship"))
  def trackLocation(os: OrderStatus): LoadingOr[String] =
    if (os.id > 1000) Left("Not jet available")
    else Right("Amsterdam NL")

  val orderId = 457L
  val orderLocation = monadEither.flatMap(getOrderStatus(orderId))(trackLocation)

  // or with flatMap and map syntax extended, it can be done with for comprehensions
  import cats.syntax.flatMap._ //  somehow not actually needed
  import cats.syntax.functor._ //  somehow not actually needed
  println {
    for {
      orderStatus <- getOrderStatus(orderId)
      location = trackLocation(orderStatus)
    } yield location
  }

  // EXERCISE 2
  case class Connection(host: String, port: String)
  val conf = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }

  // with Either (can be done with anything - Try, Option, Future etc.)
  type ConnectOr[T] = Either[String, T]
  val connectMonad = Monad[ConnectOr]

  object solWitEither extends HttpService[ConnectOr] {
    override def getConnection(cfg: Map[String, String]): ConnectOr[Connection] =
      if (cfg("host") == "localhost") Right(Connection(cfg("host"), cfg("port"))) else Left("Incorrect host")

    override def issueRequest(connection: Connection, payload: String): ConnectOr[String] =
      if (payload.length > 20 && connection.host == "localhost") Right("Connected") else Left("Payload to small")
  }

  import solWitEither._
  println {
    for {
      con <- getConnection(conf)
      report <- issueRequest(con, "Zub zub zub zub zub zub zub")
    } yield report
  }

  // more general
  //  import cats.syntax.flatMap._ //  needed here, but already defined above
  //  import cats.syntax.functor._ //  needed here, but already defined above
  def getResponse[M[_]](service: HttpService[M], payload: String)(implicit monad: Monad[M]): M[String] =
    for {
      con <- service.getConnection(conf)
      resp <- service.issueRequest(con, payload)
    } yield resp

  println {
    getResponse(solWitEither, "Short payload")
  }
}
