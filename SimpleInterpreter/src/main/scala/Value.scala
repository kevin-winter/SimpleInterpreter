sealed trait ExpValue
case class ExpInteger(i : Int) extends ExpValue
case class ExpList(xs : List[ExpValue]) extends ExpValue
case class ExpFunction(parameters: List[String], body: Expression) extends ExpValue
case class ExpBoolean(b:Boolean) extends ExpValue

object PrettyPrinter {
  def print(value: ExpValue):String = value match {
    case ExpBoolean(true) => "True"
    case ExpBoolean(false) => "False"
    case ExpInteger(i) => i.toString
    case ExpList(xs) => xs.map(print).mkString("[",",","]")
    case ExpFunction(parameters, body) => parameters.mkString("(",",",")") + " => " + body
  }
}
