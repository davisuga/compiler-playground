package grammar
import org.parboiled2._
import CharPredicate.*
class Calculator(val input: ParserInput) extends Parser {
  def InputLine = rule { Expr ~ EOI }

  def Expr: Rule1[Int] = rule {
    Term ~ zeroOrMore(
      '+' ~ Term ~> ((_: Int) + _)
        | '-' ~ Term ~> ((_: Int) - _)
    )
  }

  def Term = rule {
    Factor ~ zeroOrMore(
      '*' ~ Factor ~> ((_: Int) * _)
        | '/' ~ Factor ~> ((_: Int) / _)
    )
  }
  def FuncDecl = rule {
    "fun" ~ Variable ~ "(" ~ Variable.+ ~ ")" ~ "=" ~ Expr
  }

  def Variable = rule { capture(Alpha.+ ~ AlphaNum.*) }
  def Factor = rule { Number | Parens }

  def Parens = rule { '(' ~ Expr ~ ')' }

  def Number = rule { capture(Digits) ~> (_.toInt) }

  def Digits = rule { oneOrMore(CharPredicate.Digit) }
}

val test = Calculator("1+1").InputLine.run()
