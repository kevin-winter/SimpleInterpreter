import org.scalatest.FunSuite
import scala.util.parsing.combinator._

class SimpleInterpreterTests extends FunSuite {

  test("Parser minimal example") {
    val prog = """
    42
    """
    if(SimpleInterpreter.checkProgramGrammer(prog)) {
      assert(true)
    } else {
      fail(SimpleInterpreter.checkProgramGrammerStringResult(prog))
    }
  }

  test("Parser short program") {
    val prog = """
    let
      concat = (xs, ys)=> if eq?(xs,[]) then ys else build(first(xs), concat(rest(xs),ys))
    in
      concat([1,2,3],[4,5])
    """
    if(SimpleInterpreter.checkProgramGrammer(prog)) {
      assert(true)
    } else {
      fail(SimpleInterpreter.checkProgramGrammerStringResult(prog))
    }
  }

  test("Parser defect program") {
    val prog = """
    let
      concat = (xs, ys)=> if eq?(xs,[] then ys else build(first(xs), concat(rest(xs),ys))
    in
      concat([1,2,3],[4,5])
    """
    assert(! SimpleInterpreter.checkProgramGrammer(prog))
  }

  test("add") {
    val prog = """
    add(4,2)
    """
    assertResult(Right(ExpInteger(6))){
      SimpleInterpreter.evaluateProgram(prog)
    }
  }

  test("Interpreter program with only build in functions") {
    val prog = """
    if eq?([1],build(1,[])) then add(3, negate(1)) else add(4,2)
    """
    assertResult(Right(ExpInteger(2))){
      SimpleInterpreter.evaluateProgram(prog)
    }
  }

  test("not") {
    val prog = """
    not(False)
    """
    assertResult(Right(ExpBoolean(true))){
      SimpleInterpreter.evaluateProgram(prog)
    }
  }


  test("Interpreter short program") {
    val prog = """
    let
      concat = (xs, ys)=> if eq?(xs,[]) then ys else build(first(xs), concat(rest(xs),ys))
    in
      concat([1,2,3],[4,5])
    """
    assertResult(Right(ExpList(List(1,2,3,4,5).map(ExpInteger)))){
      SimpleInterpreter.evaluateProgram(prog)
    }
  }

  test("Interpreter let expr") {
    val prog = """
    let
      a = 10;
      b = add(a,22)
    in
      add(a,b)
    """
    assertResult(Right(ExpInteger(42))){
      SimpleInterpreter.evaluateProgram(prog)
    }
  }

  test("anonymous function expr 1") {
    val prog = """
    let
      a = 10
    in
      ((x) => add(x, 2))(1)
               """
    assertResult(Right(ExpInteger(3))){
      SimpleInterpreter.evaluateProgram(prog)
    }
  }
  test("anonymous function expr 2") {
    val prog = """
    let
      a = 10
    in
      (first([(x) => add(x,2)]))(1)
               """
    assertResult(Right(ExpInteger(3))){
      SimpleInterpreter.evaluateProgram(prog)
    }
  }

  test("Interpreter even/odd") {

    val prog = """
    let
      even? = (n) => if eq?(n, 0) then True else odd?(add(n, negate(1)));
      odd? = (n) => if eq?(n, 0) then False else even?(add(n, negate(1)))
    in
      even?(10)
    """

    assertResult(Right(ExpBoolean(true))){
      SimpleInterpreter.evaluateProgram(prog)
    }
  }

  test("Interpreter list comprehensions") {

    val prog = """
    [add(x,x) | x <- [1,2,3]]
    """

    assertResult(Right(ExpList(List(2,4,6).map(ExpInteger)))){
      SimpleInterpreter.evaluateProgram(prog)
    }
  }




}
