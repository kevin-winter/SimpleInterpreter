import scala.util.parsing.combinator._

class ExpParser extends JavaTokenParsers {
  def expr : Parser[Expression] = apply_expr | fun_expr | cond_expr | let_expr | int | list | bool | id | anonymous_fun_expr | listcomp
  def anonymous_fun_expr : Parser[AnonymousFunExpr] = ("("~>expr<~")")~("("~>repsep(expr,",")<~")") ^^ {
    case FunExpr(p,b)~v => AnonymousFunExpr(p,b,v)
    case e~v => AnonymousFunExpr(Nil,e,v)
  }
  def apply_expr : Parser[ApplyExpr] = id~("("~>repsep(expr,",")<~")") ^^ {case l~r => ApplyExpr(l,r)}
  def cond_expr : Parser[CondExpr] = "if"~>expr~"then"~expr~"else"~expr ^^ {case ifc~_~thenc~_~elsec => CondExpr(ifc, thenc, elsec)}
  def let_expr : Parser[LetExpr] = "let"~>rep1sep(binding,";")~"in"~expr ^^ {case let~_~in => LetExpr(let,in)}
  def binding : Parser[Binding] = id~"="~expr ^^ {case id ~ _ ~ e => Binding(id,e)}
  def listcomp : Parser[ListComp] = "["~>apply_expr~"|"~id~"<-"~list<~"]" ^^ {case f~_~p~_~l => ListComp(f,p,l)}

  def fun_expr : Parser[FunExpr] = ("("~>repsep(id,",")<~")")~("=>"~>expr) ^^ {case params~body => FunExpr(params, body)}
  def id : Parser[Id] = "[a-z][A-Za-z0-9?_]*".r ^^ { str => Id(str)}
  def list : Parser[Listing] = "["~>repsep(expr,",")<~"]" ^^ { l => Listing(l)}
  def bool : Parser[Bool] = ("True" | "False") ^^ { str => Bool(str.toBoolean)}
  def int : Parser[Integer] = "[1-9][0-9]*|0|-[1-9][0-9]*".r ^^ { str => Integer(str.toInt)}
}

object ParseProgram extends ExpParser {
  def parse(s: String): ParseResult[Expression] = {
    parseAll(expr, s)
  }
}
