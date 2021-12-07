package lectures.part1intro

object TypeClass extends App {

  // 1. Deffinition

  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  // 2. Implicit type class instance

  implicit object StringSerializer extends JSONSerializer[String] {
    override def toJson(value: String): String =
      s"""
        |"$value"
        |""".stripMargin.trim
  }

  // 3. Common method

  def convertToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list map serializer.toJson mkString("[", ",", "]")

  // 4. Or cool implicit class

  object SomeObjectToBringIntoScope {
    implicit class SomeName[T](value: List[T])(implicit serializer: JSONSerializer[T]) {
      def toJson: String = (value map ( item => serializer.toJson(item) )).mkString("[", ",", "]")
    }
  }


  // OUTPUT
  println {
    val listOfStrings = List("Ray", "Bob", "Marley")
    convertToJson(listOfStrings)

    // after step 4
    import SomeObjectToBringIntoScope._ // like import spray.json
    listOfStrings.toJson
   // "arturo".toJson
  }
}
