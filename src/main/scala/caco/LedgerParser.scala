package caco

import org.parboiled2._
import shapeless.{::,HNil}
import caco.ast.ledger._
import caco.ast.ledger.operators._

case class LedgerParser (val input: ParserInput, val fname: String) extends Parser  {

  private def loc (cursor: Int) = Location(fname, cursor)

  def main :Rule1[Ledger] = pLedger

  def pLedger = rule { WS ~ zeroOrMore (pLine) ~ EOI ~> { _.toList }  }

  def pLine: Rule1[Line] = rule {
    pOperation | pInvariant | pAssertion | pUnit | pAccount }

  def pUnit: Rule1[Unit] = rule {
    push(cursor) ~ UNIT ~ ID ~ (PRECISION ~ INT).? ~ pDescription ~> {
      (cu: Int, id: Id, pr: Option[Int], de: Description) =>
        Unit (id, de, loc(cu), pr getOrElse 2) }
  }

  def pInvariant :Rule1[Invariant] = rule {
    push(cursor) ~ pDate ~ INVARIANT ~ pExpr ~ pDescription ~>
      { (cu: Int, da: Date, ex: Expr, de: Description) =>
        Invariant (ex, da, de, loc(cu)) }
  }

  def pAssertion :Rule1[Assertion] = rule {
    push(cursor) ~ pDate ~ pExpr ~ pDescription ~>
      { (cu: Int, da: Date, ex: Expr, de: Description) =>
        Assertion (ex, da, de, loc(cu)) }
  }

  def pAccount: Rule1[Account] = rule { pActiveAccount | pDerivedAccount }

  def pActiveAccount: Rule1[ActiveAccount] = rule {
    push (cursor) ~ ACCOUNT ~ ID ~ LBRK ~ ID ~ RBRK ~ pDescription ~> {
      (cu: Int, ac: Id, un: Id, de: Description) =>
        ActiveAccount (id=ac, unit=un, descr=de, loc=loc(cu)) }
  }

  def pDerivedAccount: Rule1[DerivedAccount] = rule {
    push (cursor) ~ ACCOUNT ~ ID ~ EQ ~ pExpr ~ pDescription ~> {
      (cu: Int, ac: Id, _: BOp, ex: Expr, de: Description) =>
        DerivedAccount (id=ac, value=ex, descr=de, loc=loc(cu)) }
  }

  def pOperation = rule { pOperation1 | pOperation2 }

  def pOperation1: Rule1[Operation] = rule {
    push (cursor) ~ optional(PENDING) ~ pDate ~ pAccounts ~ PLUSEQ ~ pExpr ~ optional (EQMINUS ~ pAccounts) ~
    pDescription ~>
    { (cu: Int, pe: Option[Boolean], da: Date, pa: Seq[Id],
       ex: Expr, ma: Option[Seq[Id]], de: Description) =>
        Operation(
          src = pa.toList, tgt = (ma getOrElse Nil).toList,
          value = ex, tstamp = da, descr = de,
          loc = loc(cu),
    pending = !pe.isEmpty) }
  }

  def pOperation2: Rule1[Operation] = rule {
    push (cursor) ~ optional(PENDING) ~ pDate ~ pAccounts ~ MINUSEQ ~ pExpr ~ optional (EQPLUS ~ pAccounts) ~
    pDescription ~>
    { (cu: Int, pe: Option[Boolean], da: Date, ma: Seq[Id],
       ex: Expr, opa: Option[Seq[Id]], de: Description) =>
        Operation(
          src = (opa getOrElse Nil).toList,
          tgt = ma.toList,
          value = ex,
          tstamp = da,
          descr = de,
          loc = loc (cu),
          pending = !pe.isEmpty) }
  }

  private def oneDescription :Rule1[String] = rule {
    PIPE ~ capture(zeroOrMore (noneOf("\n"))) ~ ("\n" ~ WS | EOI) }

  def pDescription :Rule1[Description] = rule {
    oneDescription.* ~> { (de: Seq[String]) => de.toList }
  }

  def pAccounts :Rule1[Seq[Id]] = rule { ID.+ separatedBy COMMA }



  // Expression language

  def pExpr:     Rule1[Expr] = rule { pBoolExpr | pTermExpr }

  def pBoolExpr: Rule1[Expr] = rule { pCompExpr ~ addComp.* }
  def addComp :Rule[Expr :: HNil, Expr :: HNil] = rule { BOOLOP ~ pCompExpr ~> bexpr _ }

  def pCompExpr: Rule1[Expr] = rule {
    pTermExpr ~ COMPOP ~ pTermExpr ~> {(l:Expr,o:BOp,r:Expr) => BExpr(l,r,o)}}

  def pTermExpr: Rule1[Expr] = rule { pTerm ~ addTerm.* }
  def bexpr (l: Expr, o: BOp, r: Expr) :Expr = BExpr (l,r,o)
  def addTerm :Rule[Expr :: HNil, Expr :: HNil] = rule { TERMOP ~ pTerm ~> bexpr _ }

  // we don't have multiplication but we prime for it in the future
  def pTerm:   Rule1[Expr]  = rule { pFactor }
  def pFactor: Rule1[Expr]  = rule { pAmount | pParens | pRef }
  def pRef:    Rule1[Ref]   = rule { ID ~> { (id:String) => Ref (id) }  }
  def pParens: Rule1[Expr]  = rule { LPAR ~ pExpr ~ RPAR }
  def pAmount: Rule1[Const] = rule {
    FINUM ~> { (nprec: (Long,Int)) => Const(nprec._1,nprec._2) } }



  // tokens

  private def kwd (st: String): Rule0 = rule {
    atomic(st) ~ (WS | test(cursorChar == EOI)) }
  private def kwd1 (st: String): Rule0 = rule {
    atomic(st) ~ (WS1 | test(cursorChar == EOI)) }

  def WS:  Rule0 = rule { (COMMENT | (anyOf(" \t\n")).+).* }
  def WS1: Rule0 = rule { (COMMENT | (anyOf(" \t\n")).+).+ }

  def COMMENT: Rule0 = rule { "--" ~ (noneOf("\n")).* ~ ("\n" | test(cursorChar == EOI)) }

  def ID: Rule1[Id] = rule {
    (capture(
      (CharPredicate.Alpha | "_") ~
        zeroOrMore (CharPredicate.AlphaNum | "_" )
    ) ~ WS) }

  def COMMA = kwd (",")
  def PIPE  = rule { atomic("|")      }
  def LPAR  = kwd ("(")
  def RPAR  = kwd (")")
  def LBRK  = kwd ("[")
  def RBRK  = kwd ("]")
  def MINUS = rule { atomic("-" ~ WS) ~ push (BOp_MINUS) }
  def PLUS  = rule { atomic("+" ~ WS) ~ push (BOp_PLUS)   }

  def PLUSEQ  = kwd ("+=")
  def EQPLUS  = kwd ("=+")
  def MINUSEQ = kwd ("-=")
  def EQMINUS = kwd ("=-")

  def EQ  = rule { atomic("==" ~ WS) ~ push (BOp_EQ) }
  def LT  = rule { atomic("<"  ~ WS) ~ push (BOp_LT) }
  def LTE = rule { atomic("<=" ~ WS) ~ push (BOp_LTE) }
  def GT  = rule { atomic(">"  ~ WS) ~ push (BOp_GT) }
  def GTE = rule { atomic(">=" ~ WS) ~ push (BOp_GTE) }
  def AND = rule { atomic("&&" ~ WS) ~ push (BOp_AND) }
  def OR = rule { atomic("||" ~ WS) ~ push (BOp_OR) }

  def TERMOP :Rule1[BOp] = rule {  MINUS | PLUS }
  def COMPOP :Rule1[BOp] = rule { EQ | LT | LTE | GT | GTE }
  def BOOLOP :Rule1[BOp] = rule { AND | OR }


  def pDate  :Rule1[Date] = rule { (pDate8 | pDate6) ~ WS }
  def pDate8 = rule { push (cursor) ~
    capture(8 times CharPredicate.Digit) ~> { (c:Int, x:String) => Date (x,loc (c)) } }
  def pDate6 = rule { push (cursor) ~
    capture(6 times CharPredicate.Digit) ~> { (c:Int, x:String) => Date ("20"+x,loc (c)) } }

  def INT = rule { INTSTR ~ WS ~> { _.toInt } }

  def FINUM: Rule1[(Long,Int)] = rule {
    FINUM_PRE ~ optional(FIXEDPNO_SUF) ~ WS ~> {
    (pre: String, suf: Option[String]) =>
      val dec = suf getOrElse ""
      ((pre+dec).toLong, dec.size) }
  }

  private def INTSTR: Rule1[String] = rule { capture(CharPredicate.Digit.+) }

  private def FINUM_PRE  = rule {
    INTSTR.+.separatedBy (",") ~> { (cs:Seq[String]) => cs.mkString }
  }

  private def FIXEDPNO_SUF: Rule1[String] = rule {
    '.' ~ INTSTR ~> { (s: String) => s.replaceAll("0+$", "") }
  }

  def UNIT      = kwd1 ("unit")
  def INVARIANT = kwd1 ("invariant")
  def PRECISION = kwd1 ("precision")
  def ACCOUNT   = kwd1 ("account")

  // not sure how I can otherwise make a rule that informed whether it matched
  // (there should be an easier way thay having the rule to push onto stack,
  //  but could not figure out)
  def PENDING   :Rule1[Boolean] = rule { "pending" ~ WS1 ~ push (true) }
}

