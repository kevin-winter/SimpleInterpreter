object SimpleInterpreter {

  def main(args: Array[String]) {
  val Array(file) = args
  val source = scala.io.Source.fromFile(file)
  val fileString = source.mkString
  source.close()

  val result = evaluateProgram(fileString)

  Console.println(result.fold(identity, PrettyPrinter.print))
  }

  def evaluateProgram(program:String):Either[String,ExpValue] = {
    val parseResult = ParseProgram.parse(program)
    if (parseResult.successful) {
      val exp = ParseProgram.parse(program).get
      val result = Interpreter.interpret(Map(), exp)
      Right(result)
    } else {
      Left(parseResult.toString)
    }
  }

  def checkProgramGrammer(program:String):Boolean = {
    ParseProgram.parse(program).successful
  }

  def checkProgramGrammerStringResult(program:String):String = {
    ParseProgram.parse(program).toString
  }

}
