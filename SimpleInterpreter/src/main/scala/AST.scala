sealed trait Expression

case class ApplyExpr(l: Expression, r: List[Expression]) extends Expression
case class CondExpr(ifc: Expression, thenc: Expression, elsec: Expression) extends Expression
case class LetExpr(let: List[Binding], in: Expression) extends Expression
case class FunExpr(params: List[Id], body: Expression) extends Expression
case class Binding(id: Id, value: Expression) extends Expression
case class ListComp(function: ApplyExpr, params: Id, l: Listing) extends Expression
case class AnonymousFunExpr(params: List[Id], body: Expression, value: List[Expression]) extends Expression

case class Listing(xs : List[Expression]) extends Expression
case class Bool(bool: Boolean) extends Expression
case class Integer(i: Int) extends Expression
case class Id(str: String) extends Expression

//Eingebaute Funktionen
case class Add(left: Expression, right: Expression) extends Expression
case class Negate(expression: Expression) extends Expression
case class First(list: Expression) extends Expression
case class Rest(list: Expression) extends Expression
case class Build(list1: Expression, list2: Expression) extends Expression
case class Empty(list: Expression) extends Expression
case class Equals(left: Expression, right: Expression) extends Expression
case class Lesser(left: Expression, right: Expression) extends Expression
case class And(left: Expression, right: Expression) extends Expression
case class Or(left: Expression, right: Expression) extends Expression
case class Not(expression: Expression) extends Expression