package caco

import org.scalatest.{FreeSpec, Matchers,Inside}
import scala.util.{Try,Success,Failure}
import reflect.io.Path._
import caco.ast.ledger._
import caco.ast.ledger.operators._

class LedgerParserSpec extends FreeSpec with Matchers with Inside {


  val fname = "MOCK"

  "tokens" - {

    "pDate" in {

      LedgerParser ("20171202", fname).pDate.run() should matchPattern
      { case Success (Date("20171202",_)) => }

      LedgerParser ("20171201  ", fname).pDate.run() should matchPattern
      { case Success (Date("20171201",_)) => }

      LedgerParser ("171201  ", fname).pDate.run() should matchPattern
      { case Success (Date("20171201",_)) => }

      LedgerParser ("171201  ", fname).pDate.run() shouldNot matchPattern
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
        inside(LedgerParser (s, fname).FINUM.run()) {
          case Success ((r1,r2)) =>
            r1 should equal (n)
            r2 should equal (prec) }

      pass  ("171201") (171201,Precision(0))
      pass  ("0") (0,Precision(0))
      pass  ("0.0") (0,Precision(0))
      pass  ("203.102") (203102,Precision(3))
      pass  ("1,203.102") (1203102,Precision(3))
      pass  ("123,456,789.01200") (123456789012L,Precision(3))
      pass  ("0.10") (1,Precision(1))

    }

  }


  "AST" - {

    "pAccounts" in {

      // Apparently due to type erasue the following matches of Seq[AccountId]
      // to Seq[String] works, and even worse so, allows to avoid warnings.
      // Otherwise we get warnings and still AccountId matches as well as UnitId
      // in all the below positions.  Apparently, we should not really pattern
      // match on the tagged types.

      LedgerParser ("bike,self_insurance,D", fname).pAccounts.run() should matchPattern
      { case Success (Seq("bike","self_insurance","D")) => }

      LedgerParser ("bike  ,  self_insurance, D", fname).pAccounts.run() should matchPattern
      { case Success (Seq("bike","self_insurance","D")) => }

      LedgerParser ("", fname).pAccounts.run() should matchPattern
      { case Failure (_) => }

      LedgerParser ("_alamakoTa1", fname).pAccounts.run() should matchPattern
      { case Success(Seq("_alamakoTa1")) => }

    }

    "pExpr" in {

      LedgerParser ("self_insurance", fname).pRef.run() should matchPattern
      { case Success (Ref(AccountId("self_insurance"))) => }

      LedgerParser ("self_insurance", fname).pFactor.run() should matchPattern
      { case Success (Ref(AccountId("self_insurance"))) => }

      LedgerParser ("self_insurance", fname).pTerm.run() should matchPattern
      { case Success (Ref(AccountId("self_insurance"))) => }


      LedgerParser ("bike", fname).pExpr.run() should matchPattern
      { case Success (Ref(AccountId("bike"))) => }

      LedgerParser ("bike + car", fname).pExpr.run() should matchPattern
      { case Success (BExpr(Ref(AccountId("bike")),Ref(AccountId("car")),BOp_PLUS)) => }

      LedgerParser ("bike + car - dog", fname).pExpr.run() should matchPattern
      { case Success (BExpr(BExpr(Ref(AccountId("bike")),Ref(AccountId("car")),BOp_PLUS),Ref(AccountId("dog")),BOp_MINUS)) => }

      LedgerParser ("bike + (car - dog)", fname).pExpr.run() should matchPattern
      { case Success (BExpr(Ref(AccountId("bike")),BExpr(Ref(AccountId("car")),Ref(AccountId("dog")),BOp_MINUS),BOp_PLUS)) => }

      LedgerParser ("bike < (car - dog)", fname).pExpr.run() should matchPattern
      { case Success (BExpr(Ref(AccountId("bike")),BExpr(Ref(AccountId("car")),Ref(AccountId("dog")),BOp_MINUS),BOp_LT)) => }

      LedgerParser ("bike < 1 && car > 2", fname).pExpr.run() should matchPattern
      { case Success (BExpr(BExpr(Ref(AccountId("bike")),Const(1,Precision(0)),BOp_LT),
                            BExpr(Ref(AccountId("car")),Const(2,Precision(0)),BOp_GT),BOp_AND)) => }

    }

    "operations" in {

      LedgerParser ("20180102 pl += 10000.00 =- ferie",fname).pOperation.run() should matchPattern {
        case Success(Operation(
            List(AccountId("pl")), List(AccountId("ferie")), Const(10000,Precision(0)), Date("20180102",_), Nil, _, false)) => }

      LedgerParser ("180103 pl -= 75,011.32 | Transfer to beneficiary",fname).pOperation.run() should matchPattern {
        case Success(Operation(
            Nil, List(AccountId("pl")), Const(7501132,Precision(2)), Date("20180103",_),
            List(" Transfer to beneficiary"), _, false)) => }

      val multline = """180102 skat +=    158.35
                       #| description
                       #| line 1
                       #| line 2""".stripMargin ('#')

      LedgerParser (multline,fname).pOperation.run() should matchPattern {
        case Success(Operation(
            List(AccountId("skat")), Nil, Const(15835,Precision(2)), Date("20180102",_),
            List(" description", " line 1", " line 2"), _, false)) => }

    }

    "invariants" in {

      val in00 = """171201 invariant D == bike + ferie
                    #
                    #-- Initialize the accounts """ stripMargin '#'

      LedgerParser (in00,fname).pInvariant.run() should matchPattern {
        case Success(Invariant(
          BExpr(Ref(AccountId("D")),BExpr(Ref(AccountId("bike")),Ref(AccountId("ferie")),BOp_PLUS),BOp_EQ),
          Date("20171201",_),Nil,_)
        ) => }

    }

    "assertions" in {

      val as00 = "180106 self_insurance == 4,056.15 | with a description -- and comment"
      LedgerParser (as00,fname).pAssertion.run() should matchPattern {
        case Success(Assertion(
          BExpr(Ref(AccountId("self_insurance")),
          Const(405615,Precision(2)),BOp_EQ),
          Date("20180106",_),
          List(" with a description -- and comment"),_)
        ) => }

    }

    "units" in {
      val un00 = "unit DKK precision 42 | danske kroner"
      LedgerParser (un00,fname).pUnit.run() should matchPattern {
        case Success(Unit(UnitId("DKK"),List(" danske kroner"),_,Precision(42))) => }
    }

    "active accounts" in {
      val ac00 = "account pt             [DKK] | a buffer awaiting transfer"
      LedgerParser (ac00, fname).pAccount.run() should matchPattern {
        case Success(ActiveAccount(AccountId("pt"),UnitId("DKK"),List(" a buffer awaiting transfer"),_)) =>
      }
    }

    "derived accounts" in {

      val ac00 = """account D == bike + skat
                   #| Allocated funds""" stripMargin '#'
      LedgerParser (ac00, fname).pAccount.run() should matchPattern {
        case Success(DerivedAccount(AccountId("D"),BExpr(Ref(AccountId("bike")),Ref(AccountId("skat")),BOp_PLUS),
          List(" Allocated funds"),_)) =>
      }
    }

  }

    "ledger level AST tests with snippets" - {

      val cases = List(
        "--",
        "-- This is a file with random content, exercising most of the grammar",
        "171201 invariant D == bike + ferie + flights + self_insurance + skat"
      )

      def test (ca: String, id: Int) {
        ("case " + id) in {
          LedgerParser (ca, fname).pLedger.run() should matchPattern {
            case Success(_) => }
        }
      }

      cases.zipWithIndex.foreach { ci => test(ci._1,ci._2) }

    }


  "file-based tests" - {

    // Directory in which you should put the test files
    val dir = "test-files/LedgerParser/"
    // Test files should match this regex (others are ignored)
    val r = """.*\.caco$""".r

    def test (na: String) = {

      na in {
        val sr = scala.io.Source.fromFile (na)
        var pa = LedgerParser (sr.mkString, na)
        inside(pa.main.run()) {
          case Success(_) =>
          case Failure(er: org.parboiled2.ParseError) =>
              fail (pa formatError er)

          case _ => fail }
        sr.close
      }
    }

    val cases = dir.toDirectory.files.map (dir + _.name).filter {
      r.findFirstIn(_).isDefined }.toList.sorted
    cases foreach (test _)
  }

}
