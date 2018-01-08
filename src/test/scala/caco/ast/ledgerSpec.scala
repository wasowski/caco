package caco.ast

import org.scalatest.{FreeSpec, Matchers}
import caco.ast.ledger._
import caco.ast.ledger.operators._
import caco.Location

class ledgerSpec extends FreeSpec with Matchers {

  private val NOLOC = Location ("",-1) // makes testing without parser easier, don't use outside testing code

  "ADT representation tests (should compile)" - {


    "units" in {

      Unit ("DKK", List("danske kroner"))
      Unit ("DKK", "danske kroner", NOLOC, 3)
      Unit ("DKK", "danske kroner", 3)
      Unit ("DKK", "danske kroner")
      Unit ("DKK")

    }

    "locations" in { Location ("testfile.caco",1) }

    val l = Location ("test-file")

    "active accounts" in {

      val bike  = ActiveAccount ("bike", "DKK")
      val queue = ActiveAccount ("queue", "DKK", "a buffer awaiting for transfer")

      ActiveAccount ("queue", "DKK", "a buffer awaiting for transfer", l)
      DerivedAccount ("D", BExpr(Ref("bike"),Ref("queue"),BOp_PLUS))

    }

    "invariants and assertions" in {

      Invariant (BExpr(Ref("bike"), Ref("queue"), BOp_EQ))
      Invariant (BExpr(Ref("bike"), Ref("queue"), BOp_EQ), Date ("20171201"))
      Assertion (BExpr(Ref("bike"), Ref("queue"), BOp_EQ), Date ("20171201"))
    }

    "operations" in {


      Operation (
        src = List("bike","queue"),
        tgt = List("queue"),
        value = Const (378138,2),
        tstamp = Date("20171201") )

      Operation (List("bike"), Nil, Const(5000,2), Date("20171204"))
      Operation (List("bike"), Nil, Const(5000,2), Date("20171204"), Nil, NOLOC, true)

    }

  }

}
