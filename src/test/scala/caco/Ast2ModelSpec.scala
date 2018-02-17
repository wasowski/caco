package caco

import org.scalatest.{FreeSpec, Matchers,Inside}

import caco.ast.{ledger => in}
import caco.model.{ledger => out}


import scalaz._
import Scalaz._

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

  "string-to-instance tests" - {

    "to be written" in { fail /* TODO */ }

  }

}
