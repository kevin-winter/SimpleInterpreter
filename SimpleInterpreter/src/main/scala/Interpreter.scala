object Interpreter {
  type VariableName = String
  type Environment = Map[VariableName, ExpValue]
  type FunctionEnvironment = Map[VariableName, (List[Id], Expression)]

  val builtinFunctions: Map[VariableName, List[ExpValue] => ExpValue] =
    Map(
      "not" -> { case List(ExpBoolean(b)) => ExpBoolean(!b) },
      "add" -> { case List(ExpInteger(a), ExpInteger(b)) => ExpInteger(a+b)},
      "negate" -> { case List(ExpInteger(a)) => ExpInteger((-1)*a)},
      "rest" -> { case List(ExpList(a)) => ExpList(a.tail)},
      "first" -> {case List(ExpList(a)) => a.head},
      "build" -> {case  (List(ExpList(x1),ExpList(xs1))) => ExpList(x1:::xs1)
                  case  (List(ExpList(x1),ExpInteger(xs1))) => ExpList(x1:::List(ExpInteger(xs1)))
                  case  (List(ExpInteger(x1),ExpList(xs1))) => ExpList(List(ExpInteger(x1)):::xs1)
                  case  (List(ExpInteger(x1),ExpInteger(xs1))) => ExpList(List(ExpInteger(x1)):::List(ExpInteger(xs1)))},
      "empty?" -> {case  List(ExpList(x)) => if(x.isEmpty){ExpBoolean(true)} else {ExpBoolean(false)}},
      "eq?" -> { case (List(ExpList(l), ExpList(r))) => if(l == r) {ExpBoolean(true)} else {ExpBoolean(false)}
                case (List(ExpInteger(l), ExpInteger(r))) => if(l == r) {ExpBoolean(true)} else {ExpBoolean(false)}},
      "lt?" -> {case List(ExpInteger(a), ExpInteger(b)) => if(a < b) {ExpBoolean(true)} else {ExpBoolean(false)}},
      "and" -> {case List(ExpBoolean(a), ExpBoolean(b)) => if(a && b) {ExpBoolean(true)} else {ExpBoolean(false)}},
      "or" -> {case List(ExpBoolean(a), ExpBoolean(b)) => if(a || b) {ExpBoolean(true)} else {ExpBoolean(false)}}
    )

  private val builtinFunctionNames = builtinFunctions.keys.toList

  def interpret(environment: Environment,
                expression: Expression): ExpValue = {
    interpret(environment, Map(), expression)
  }

  def interpret(environment: Environment, functionEnvironment: FunctionEnvironment,
                expression: Expression): ExpValue = expression match {

    case AnonymousFunExpr(params,b,v) => {
      params match {
        case Nil => {
          val ids: List[Id] = dig(environment, functionEnvironment, b)
          applyBindings(environment, functionEnvironment, (ids zip v).map{case (i, p) => Binding(i,p)}, b)
        }
        case ids => applyBindings(environment, functionEnvironment, (ids zip v).map{case (i, p) => Binding(i,p)}, b)
      }
    }

    case ApplyExpr(Id(l),r) => execFunction(l, r, environment, functionEnvironment)

    case LetExpr(l, e) => applyBindings(environment, functionEnvironment, l, e)

    case ListComp(ApplyExpr(Id(l),r), Id(p), Listing(li)) => {
      ExpList(li.map(x => execFunction(l, r, environment + (p -> interpret(environment, functionEnvironment, x)), functionEnvironment)))
    }

    case CondExpr(ifc, thenc, elsec) => interpret(environment, functionEnvironment, ifc) match
    {
      case ExpBoolean(b) => {
        if (b) {
          interpret(environment, functionEnvironment, thenc)
        }
        else {
          interpret(environment, functionEnvironment, elsec)
        }
      }
    }

    case Build(x,xs) => (interpret(environment,functionEnvironment,x),interpret(environment,functionEnvironment,xs)) match
    {
      case  (ExpList(x1),ExpList(xs1)) => builtinFunctions("build")(List(ExpList(x1), ExpList(xs1)))
      case  (ExpList(x1),ExpInteger(xs1)) => builtinFunctions("build")(List(ExpList(x1), ExpInteger(xs1)))
      case  (ExpInteger(x1),ExpList(xs1)) => builtinFunctions("build")(List(ExpInteger(x1), ExpList(xs1)))
      case  (ExpInteger(x1),ExpInteger(xs1)) => builtinFunctions("build")(List(ExpInteger(x1), ExpInteger(xs1)))
    }

    case Integer(i) => ExpInteger(i)
    case Bool(b) => ExpBoolean(b)

    case Listing(xs) => ExpList(xs.map(x => interpret(environment,functionEnvironment, x)))

    case Add(l,r) => (interpret(environment,functionEnvironment,l),interpret(environment,functionEnvironment,r)) match
    {
      case (ExpInteger(l1), ExpInteger(r1)) => builtinFunctions("add")(List(ExpInteger(l1), ExpInteger(r1)))

    }
    case Negate(e) => interpret(environment, functionEnvironment, e) match {
      case (ExpInteger(i)) => builtinFunctions("negate")(List(ExpInteger(i)))
    }
    case First(xs) => interpret(environment, functionEnvironment, xs) match {
      case ExpList(x) => builtinFunctions("first")(List(ExpList(x)))
    }
    case Rest(xs) => interpret(environment, functionEnvironment, xs) match {
      case ExpList(x) => builtinFunctions("rest")(List(ExpList(x)))
    }

    case Equals(x, y) => builtinFunctions("eq?")(List(interpret(environment,functionEnvironment,x), interpret(environment,functionEnvironment,y)))

    case Not(e) => interpret(environment,functionEnvironment,e) match
    {
      case ExpBoolean(b) => builtinFunctions("not")(List(ExpBoolean(b)))
    }

    case And(a,b) => (interpret(environment, a), interpret(environment, functionEnvironment, b)) match
    {
      case (ExpBoolean(x),ExpBoolean(y)) => builtinFunctions("and")(List(ExpBoolean(x),ExpBoolean(y)))
    }
    case Or(a,b) => (interpret(environment, a), interpret(environment, functionEnvironment, b)) match
    {
      case (ExpBoolean(x),ExpBoolean(y)) => builtinFunctions("or")(List(ExpBoolean(x),ExpBoolean(y)))
    }
    case Lesser(a,b) => (interpret(environment, functionEnvironment, a), interpret(environment, functionEnvironment, b)) match
    {
     case (ExpBoolean(x),ExpBoolean(y)) => builtinFunctions("lt?")(List(ExpBoolean(x),ExpBoolean(y)))
    }
    case Empty(x) => interpret(environment, functionEnvironment, x) match
    {
      case ExpList(xs) => builtinFunctions("empty?")(List(ExpList(xs)))
    }

    case Id(id) => environment(id)

    case FunExpr(params, body) => interpret(environment, functionEnvironment, body)
  }

  def applyBindings(environment: Environment, fEnvironment: FunctionEnvironment, bindings: List[Binding], e: Expression): ExpValue = {
    bindings match {
      case Binding(Id(id), FunExpr(params, body)) :: xs => applyBindings(environment, fEnvironment + (id -> (params, body)), xs, e)
      case Binding(Id(id), value) :: xs => applyBindings(environment + (id -> interpret(environment, fEnvironment, value)), fEnvironment, xs, e)
      case Nil => interpret(environment, fEnvironment, e)
    }
  }

  def execFunction(id: String, params: List[Expression], env: Environment, fEnv: FunctionEnvironment): ExpValue = {
    id match {
      case "build" => interpret(env, fEnv, Build(params.head, params.tail.head))
      case "first" => interpret(env, fEnv, First(params.head))
      case "rest" => interpret(env, fEnv, Rest(params.head))
      case "add" => interpret(env, fEnv, Add(params.head, params.tail.head))
      case "negate" => interpret(env, fEnv, Negate(params.head))
      case "not" =>  interpret(env, fEnv, Not(params.head))
      case "eq?" =>  interpret(env, fEnv, Equals(params.head, params.tail.head))
      case "and" => interpret(env, fEnv, And(params.head, params.tail.head))
      case "or" => interpret(env, fEnv, Or(params.head, params.tail.head))
      case "lt?" => interpret(env, fEnv, Lesser(params.head, params.tail.head))
      case "empty?" => interpret(env, fEnv, Empty(params.head))
      case other => fEnv.get(other) match {
        case Some((ids, expr)) => applyBindings(env, fEnv, (ids zip params).map{case (i, p) => Binding(i,p)}, expr)
        case _ => ExpBoolean(false)
      }
    }
  }

  def dig(env: Environment, fEnv: FunctionEnvironment, e: Expression): List[Id] = e match {
    case FunExpr(params, _) => params
    case ApplyExpr(_,r) => r match {
      case x :: xs => dig(env, fEnv, x)
    }
    case Listing(r) => r match {
      case x :: xs => dig(env, fEnv, x)
    }
  }
}