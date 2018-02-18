package caco

import org.scalatest.{FreeSpec, Matchers,Inside}

import caco.ast.{ledger => in}
import caco.model.{ledger => out}


import scalaz._
import Scalaz._
import scala.util.{Try,Success,Failure} // hide these from Scalaz

class Ast2ModelSpec extends FreeSpec with Matchers with Inside {

  "instance-to-instance tests" - {

    val loc = Location("filename")

    val uni = in.Unit(in.UnitId("USD"),"descr",loc,in.Precision(42))
    val ty = out.UnitTy(uni)

    val aac     = in.ActiveAccount (in.AccountId("vacation"), in.UnitId("USD"), "descr1", loc)
    val aac_out = out.ActiveAccount (aac.id, uni, aac.descr, aac.loc)

    val dac = in.DerivedAccount (
      id = in.AccountId("plusone"),
      value = in.BExpr(in.Ref(in.AccountId("vacation"),loc), in.Const(1,in.Precision(0),loc), in.BOp.PLUS, loc),
      descr = List("descr2"),
      loc = Location("filename")
    )
    val dac_out = out.DerivedAccount (
      id = dac.id,
      value = out.BExpr (out.Ref (aac_out,ty,loc), out.Const(1, out.Precision(0), out.NumericTy, loc), out.BOp.PLUS, ty, loc),
      descr = dac.descr,
      loc = dac.loc,
      unit = uni
    )

    "Empty input gives empty output" in {
      Ast2Model.convert (Nil) shouldBe \/- (out.Ledger (Nil,Nil,Nil))
    }

    "Single Unit converts" in {
      val led = List(uni)
      Ast2Model.convert  (led: in.Ledger) shouldBe \/- (out.Ledger(led,Nil,Nil))
    }

    "Single ActiveAccount converts" in {
      val led: in.Ledger = List[in.Line](uni, aac)
      Ast2Model.convert (led) shouldBe \/- (out.Ledger(uni ::Nil, aac_out ::Nil, Nil))
    }

    "Single DerivedAccount converts" in {

      val ledger: in.Ledger =  List[in.Line] (uni, aac, dac)
      Ast2Model.convert (ledger) shouldBe \/- (out.Ledger(uni ::Nil, aac_out ::dac_out ::Nil, Nil))

    }
  }

  def mkModel (s: String, name: String) :StaticError \/ out.Ledger = {
    val ast = LedgerParser (s, name).pLedger.run()
    ast match {
      case Success (l) => Ast2Model.convert (l)
      case _ => fail }
  }

  "string-to-instance tests" - {

    "unit" in {
      val m = mkModel ("unit DKK | danske kroner",  "unit")
      m should matchPattern { case \/-(_) => }
      m match {
        case \/-(l) => l.units should matchPattern {
          case in.Unit (in.UnitId("DKK"),List(" danske kroner"),_,in.Precision(2))::_ => }
        case -\/(_) => fail
      }
    }

    "unit-account" in {
      val m = mkModel (
        """|unit DKK | danske kroner
           |account bike [DKK]""".stripMargin,  "unit-account")
      m should matchPattern { case \/-(_) => }
      m match {
        case \/-(l) => l.accounts should matchPattern {
          case out.ActiveAccount (in.AccountId("bike"),in.Unit(out.UnitId("DKK"),_,_,_),_,_)::_ => }
        case -\/(_) => fail
      }
    }

    "unit-account-derived" in {
      val m = mkModel (
        """|unit DKK | danske kroner
           |account bike [DKK]
           |account ferie [DKK]
           |account D == bike + ferie | Allocated funds in Denmark""".stripMargin,  "unit-account-derived")
      m should matchPattern { case \/-(_) => }
      m match {
        case \/-(l) => l.accounts should matchPattern {
          case List(_,_,out.DerivedAccount (
            in.AccountId("D"),
            out.BExpr(out.Ref(_,_,_),_,_,_,_),_,_,in.Unit(out.UnitId("DKK"),_,_,_))) => }
        case -\/(_) => fail
      }
    }

    "unit-account-derived-invariant" in {

        val result = mkModel (
        """|unit DKK | danske kroner
           |account bike [DKK]
           |account ferie [DKK]
           |account D == bike + ferie + 1 | Allocated funds in Denmark
           |171201 invariant D == bike + ferie + 1""".stripMargin,  "unit-account-derived-invariant")

        result.isRight  shouldBe true
        result map { l => l.commands should matchPattern { case out.Invariant (_,_,_,_)::Nil => } }
    }

  }

}
