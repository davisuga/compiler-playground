package golang.ast;

sealed trait Node

case class Ident(name: String) extends Node

enum BasicLit(val value: String) extends Node:
  case IntLit(v: String) extends BasicLit(v)
  case FloatLit(v: String) extends BasicLit(v)
  case ImagLit(v: String) extends BasicLit(v)
  case RuneLit(v: String) extends BasicLit(v)
  case StringLit(v: String) extends BasicLit(v)

extension (basicList: BasicLit) {
  def unapply = basicList.value
}
val _ = BasicLit.FloatLit("")

case class FuncLit(body: BlockStmt) extends Node

case class FuncDecl(
    name: Ident,
    params: List[Field],
    results: List[Field],
    body: BlockStmt
) extends Node

case class BlockStmt(stmts: List[Stmt]) extends Node

case class AssignStmt(lhs: List[Expr], rhs: List[Expr]) extends Node

case class ExprStmt(x: Expr) extends Node

case class ReturnStmt(results: List[Expr]) extends Node

case class IfStmt(
    init: Option[Stmt],
    cond: Expr,
    body: BlockStmt,
    elseStmt: Option[Stmt]
) extends Node

case class ForStmt(
    init: Option[Stmt],
    cond: Option[Expr],
    post: Option[Stmt],
    body: BlockStmt
) extends Node

case class SwitchStmt(
    init: Option[Stmt],
    tag: Option[Expr],
    body: List[CaseClause]
) extends Node

case class TypeSwitchStmt(
    init: Option[Stmt],
    assign: Stmt,
    body: List[CaseClause]
) extends Node

case class CaseClause(list: List[Expr], body: List[Stmt]) extends Node

case class SelectStmt(body: List[CommClause]) extends Node

case class CommClause(comm: Stmt, body: List[Stmt]) extends Node

case class SendStmt(chan: Expr, value: Expr) extends Node

case class DeferStmt(call: CallExpr) extends Node

sealed trait Stmt extends Node

case class DeclStmt(decl: Decl) extends Stmt

sealed trait Decl extends Node

case class GenDecl(tok: Token, specs: List[Spec]) extends Decl

sealed trait Spec extends Node

case class ImportSpec(name: Option[Ident], path: BasicLit) extends Spec

case class ValueSpec(names: List[Ident], typ: Option[Expr], values: List[Expr])
    extends Spec

case class TypeSpec(name: Ident, typ: Expr) extends Spec

sealed trait Expr extends Node

case class ParenExpr(x: Expr) extends Expr

case class SelectorExpr(x: Expr, sel: Ident) extends Expr

case class IndexExpr(x: Expr, index: Expr) extends Expr

case class SliceExpr(
    x: Expr,
    low: Option[Expr],
    high: Option[Expr],
    max: Option[Expr]
) extends Expr

case class TypeAssertExpr(x: Expr, typ: Option[Expr]) extends Expr

case class CallExpr(fun: Expr, args: List[Expr]) extends Expr

case class StarExpr(x: Expr) extends Expr

case class UnaryExpr(op: Token, x: Expr) extends Expr

case class BinaryExpr(x: Expr, op: Token, y: Expr) extends Expr

case class KeyValueExpr(key: Expr, value: Expr) extends Expr

sealed trait Token extends Node

case class IDENT(value: String) extends Token

case class INT(value: String) extends Token

case class FLOAT(value: String) extends Token

case class IMAG(value: String) extends Token

case class CHAR(value: String) extends Token

case class STRING(value: String) extends Token

case class ADD(value: String) extends Token

case class SUB(value: String) extends Token

case class MUL(value: String) extends Token

case class QUO(value: String) extends Token

case class REM(value: String) extends Token

case class AND(value: String) extends Token

case class OR(value: String) extends Token

case class XOR(value: String) extends Token

case class SHL(value: String) extends Token

case class SHR(value: String) extends Token

case class AND_NOT(value: String) extends Token

case class ADD_ASSIGN(value: String) extends Token

case class SUB_ASSIGN(value: String) extends Token

case class MUL_ASSIGN(value: String) extends Token

case class QUO_ASSIGN(value: String) extends Token

case class REM_ASSIGN(value: String) extends Token

case class AND_ASSIGN(value: String) extends Token

case class OR_ASSIGN(value: String) extends Token

case class XOR_ASSIGN(value: String) extends Token

case class SHL_ASSIGN(value: String) extends Token

case class SHR_ASSIGN(value: String) extends Token

case class AND_NOT_ASSIGN(value: String) extends Token

case class LAND(value: String) extends Token

case class LOR(value: String) extends Token

case class ARROW(value: String) extends Token

case class INC(value: String) extends Token

case class DEC(value: String) extends Token

case class EQ(value: String) extends Token

case class LTE(value: String) extends Token

case class GT(value: String) extends Token

case class NEQ(value: String) extends Token

case class LAND_NOT(value: String) extends Token

case class LOR_NOT(value: String) extends Token

case class BREAK(value: String) extends Token

case class CASE(value: String) extends Token

case class CHAN(value: String) extends Token

case class CONST(value: String) extends Token

case class CONT(value: String) extends Token

case class DEFAULT(value: String) extends Token

case class DEF(value: String) extends Token

case class ELSE(value: String) extends Token

case class FALL(value: String) extends Token

case class FOR(value: String) extends Token

case class FUNC(value: String) extends Token

case class GO(value: String) extends Token

case class GOTO(value: String) extends Token

case class IF(value: String) extends Token

case class IMP(value: String) extends Token

case class INTER(value: String) extends Token

case class MAP(value: String) extends Token

case class PACK(value: String) extends Token

case class RANG(value: String) extends Token

case class RET(value: String) extends Token

case class SELECT(value: String) extends Token

case class STR(value: String) extends Token

case class STRU(value: String) extends Token

case class SWITCH(value: String) extends Token

case class TYPE(value: String) extends Token

case class VAR(value: String) extends Token

case class Field(names: List[Ident], typ: Expr) extends Node

case class FieldList(list: List[Field]) extends Node

def generate[T <: Node](node: T): String = node match {
  case Ident(name)           => name
  case BasicLit.FloatLit(x)  => x
  case BasicLit.IntLit(x)    => x
  case BasicLit.ImagLit(x)   => x
  case BasicLit.RuneLit(x)   => x
  case BasicLit.StringLit(x) => x
  case FuncLit(x)            => generate(x)
  case SendStmt(a, b)        => s"${generate(a)} <- ${b}"
  case FuncDecl(name, params, results, body) =>
    val paramString = params.map(generate).mkString(", ")
    val resultString = results.map(generate).mkString(", ")
    s"func ${generate(name)}($paramString) ($resultString) {\n${generate(body)}\n}"
  case BlockStmt(stmts) => stmts.map(generate).mkString("\n")
  case AssignStmt(lhs, rhs) =>
    val lhsString = lhs.map(generate).mkString(", ")
    val rhsString = rhs.map(generate).mkString(", ")
    s"$lhsString = $rhsString"
  case ExprStmt(x) => generate(x)
  case ReturnStmt(results) =>
    val resultString = results.map(generate).mkString(", ")
    s"return $resultString"
  case IfStmt(init, cond, body, elseStmt) =>
    val initString = init.map(generate).getOrElse("")
    val elseString =
      elseStmt.map(s => s"\n else {\n${generate(s)}\n}").getOrElse("")
    s"$initString\nif ${generate(cond)} {\n${generate(body)}\n}$elseString"
  case ForStmt(init, cond, post, body) =>
    val initString = init.map(generate).getOrElse("")
    val condString = cond.map(generate).getOrElse("")
    val postString = post.map(generate).getOrElse("")
    s"$initString\nfor $condString {\n${generate(body)}\n$postString}"
  case SwitchStmt(init, tag, body) =>
    val initString = init.map(generate).getOrElse("")
    val tagString = tag.map(generate).getOrElse("")
    val bodyString = body.map(generate).mkString("\n")
    s"$initString\nswitch $tagString {\n$bodyString\n}"
  case TypeSwitchStmt(init, assign, body) =>
    val initString = init.map(generate).getOrElse("")
    val bodyString = body.map(generate).mkString("\n")
    s"$initString\nswitch $assign {\n$bodyString\n}"
  case CaseClause(list, body) =>
    val listString = list.map(generate).mkString(", ")
    val bodyString = body.map(generate).mkString("\n")
    s"case $listString:\n$bodyString"
  case SelectStmt(body) =>
    val bodyString = body.map(generate).mkString("\n")
    s"select {\n$bodyString\n}"
  case CommClause(comm, body) =>
    val commString = generate(comm)
    val bodyString = body.map(generate).mkString("\n")
    s"$commString:\n$bodyString"
  case UnaryExpr(op, x)     => s"${generate(op)}${generate(x)}"
  case BinaryExpr(x, op, y) => s"${generate(x)} ${generate(op)} ${generate(y)}"
  case KeyValueExpr(key, value) => s"${generate(key)}: ${generate(value)}"
  case Field(names, typ) =>
    s"${names.map(generate).mkString(", ")} ${generate(typ)}"
  case FieldList(list) => list.map(generate).mkString("\n")

}
