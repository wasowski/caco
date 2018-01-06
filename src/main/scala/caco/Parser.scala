package caco

import org.parboiled2._
import shapeless.{::,HNil}
import caco.model.ledger._

case class LedgerParser (val input: ParserInput, val fname: String) extends Parser  {

  private def loc (cursor: Int) = Location(fname, cursor)

  def main = pLedger

  def pLedger = rule { zeroOrMore (pLine) ~ EOI }

  def pLine = rule { "TODO" } // pOperation | pAssertion | pOperation | pAccount  }

  def pUnit = rule { "TODO "}


  def pInvariant = rule { "TODO" }
  def pAssertion = rule { "TODO" }

  def pAccount = rule { pActiveAccount | pDerivedAccount }
  def pActiveAccount = rule { "TODO" }
  def pDerivedAccount = rule { "TODO" }

  def pOperation = rule { pOperation1 | pOperation2 }

  def pOperation1: Rule1[Operation] = rule {
    pDate ~ pAccounts ~ PLUSEQ ~ pExpr ~ optional (EQMINUS ~ pAccounts) ~
    zeroOrMore (pDescription) ~>
    { (d: Date, pa: Seq[Id], e: Expr, oma: Option[Seq[Id]], de:Seq[Id]) =>
        Operation(
          src = pa.toList,
          tgt = (oma getOrElse Nil).toList,
          value = e,
          tstamp = d,
          descr = de.toList) }
  }

  def pOperation2: Rule1[Operation] = rule {
    pDate ~ pAccounts ~ MINUSEQ ~ pExpr ~ optional (EQPLUS ~ pAccounts) ~
    zeroOrMore (pDescription) ~>
    { (d: Date, ma: Seq[Id], e: Expr, opa: Option[Seq[Id]], de:Seq[Id]) =>
        Operation(
          src = (opa getOrElse Nil).toList,
          tgt = ma.toList,
          value = e,
          tstamp = d,
          descr = de.toList) }
  }



  def pDescription :Rule1[String] = rule { PIPE ~ capture(zeroOrMore (noneOf("\n"))) }

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

  def WS: Rule0 = rule { ((anyOf(" \t \n")).+ | COMMENT).* }

  def COMMENT: Rule0 = rule { "--" ~ (noneOf(" \n")).* ~ "\n" }

  def ID :Rule1[Id] = rule {
    atomic(capture(
      (CharPredicate.Alpha | "_") ~
        zeroOrMore (CharPredicate.AlphaNum | "_" )
    ) ~ WS) }

  def COMMA = rule { atomic("," ~ WS) }
  def PIPE  = rule { atomic("|")      }
  def LPAR  = rule { atomic("(" ~ WS) }
  def RPAR  = rule { atomic(")" ~ WS) }
  def MINUS = rule { atomic("-" ~ WS) ~ push (BOp_MINUS) }
  def PLUS  = rule { atomic("+" ~ WS) ~ push (BOp_PLUS)   }

  def PLUSEQ  = rule { atomic("+=" ~ WS) }
  def EQPLUS  = rule { atomic("=+" ~ WS) }
  def MINUSEQ = rule { atomic("-=" ~ WS) }
  def EQMINUS = rule { atomic("=-" ~ WS) }

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


}

