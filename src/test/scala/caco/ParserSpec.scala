package caco

import org.scalatest.{FreeSpec, Matchers,Inside}
import scala.util.{Success,Failure}

class LedgerParserSpec extends FreeSpec with Matchers with Inside {

  import caco.model.ledger._

  val fname = "MOCK"

  "token rules" - {

    "pDate8" in {

      LedgerParser ("20171202", fname).pDate8.run() should matchPattern
      { case Success (Date("20171202",_)) => }

      LedgerParser ("20171201  ", fname).pDate8.run() should matchPattern
      { case Success (Date("20171201",_)) => }

    }

    "pDate6" in {

      LedgerParser ("171201  ", fname).pDate6.run() should matchPattern
      { case Success (Date("20171201",Some(Location("MOCK",0)))) => }

      LedgerParser ("171201  ", fname).pDate6.run() shouldNot matchPattern
      { case Success (Date("171201",_)) => }

    }

    "ID" in {
      LedgerParser ("171201  ", fname).ID.run() should matchPattern
      { case Failure (_) => }

      LedgerParser ("self_insurance  ", fname).ID.run() should matchPattern
      { case Success ("self_insurance") => }
    }

    "amounts in fixedpoint financial notation" in {

      def pass (s: String) (n: Long, prec: Precision) =
        inside(LedgerParser (s, fname).FINUM.run())
        { case Success ((r1,r2)) =>
            r1 should equal (n)
            r2 should equal (prec) }

      pass  ("171201") (171201,0)
      pass  ("0") (0,0)
      pass  ("0.0") (0,0)
      pass  ("203.102") (203102,3)
      pass  ("1,203.102") (1203102,3)
      pass  ("123,456,789.01200") (123456789012L,3)
      pass  ("0.10") (1,1)

    }

  }

  "AST" - {

    "pAccounts" in {

      LedgerParser ("bike,self_insurance,D", fname).pAccounts.run() should matchPattern
      { case Success (Vector("bike","self_insurance","D")) => }

      LedgerParser ("bike  ,  self_insurance, D", fname).pAccounts.run() should matchPattern
      { case Success (Vector("bike","self_insurance","D")) => }

      LedgerParser ("", fname).pAccounts.run() should matchPattern
      { case Failure (_) => }

      LedgerParser ("_alamakoTa1", fname).pAccounts.run() should matchPattern
      { case Success(Vector("_alamakoTa1")) => }

    }

    "pExpr" in {

      LedgerParser ("self_insurance", fname).pRef.run() should matchPattern
      { case Success (Ref("self_insurance")) => }

      LedgerParser ("self_insurance", fname).pFactor.run() should matchPattern
      { case Success (Ref("self_insurance")) => }

      LedgerParser ("self_insurance", fname).pTerm.run() should matchPattern
      { case Success (Ref("self_insurance")) => }

      LedgerParser ("bike", fname).pExpr.run() should matchPattern
      { case Success (Ref("bike")) => }

      LedgerParser ("bike + car", fname).pExpr.run() should matchPattern
      { case Success (BExpr(Ref("bike"),Ref("car"),BOp_PLUS)) => }

      LedgerParser ("bike + car - dog", fname).pExpr.run() should matchPattern
      { case Success (BExpr(BExpr(Ref("bike"),Ref("car"),BOp_PLUS),Ref("dog"),BOp_MINUS)) => }

      LedgerParser ("bike + (car - dog)", fname).pExpr.run() should matchPattern
      { case Success (BExpr(Ref("bike"),BExpr(Ref("car"),Ref("dog"),BOp_MINUS),BOp_PLUS)) => }

      LedgerParser ("bike < (car - dog)", fname).pExpr.run() should matchPattern
      { case Success (BExpr(Ref("bike"),BExpr(Ref("car"),Ref("dog"),BOp_MINUS),BOp_LT)) => }

      LedgerParser ("bike < 1 && car > 2", fname).pExpr.run() should matchPattern
      { case Success (BExpr(BExpr(Ref("bike"),Const(1,0),BOp_LT),
                            BExpr(Ref("car"),Const(2,0),BOp_GT),BOp_AND)) => }

    }


  }

}
