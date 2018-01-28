package caco.ast

import org.scalatest.{FreeSpec, Matchers}
import caco.ast.ledger._
import caco.Location

class ledgerSpec extends FreeSpec with Matchers {

  private val NOLOC = Location ("",-1) // makes testing without parser easier, don't use outside testing code

  "ADT representation tests (should compile)" - {


    "units" in {

      Unit (UnitId("DKK"), List("danske kroner"))
      Unit (UnitId("DKK"), "danske kroner", NOLOC, Precision(3))
      Unit (UnitId("DKK"), "danske kroner", Precision(3))
      Unit (UnitId("DKK"), "danske kroner")
      Unit (UnitId("DKK"))

    }

    "locations" in { Location ("testfile.caco",1) }

    val l = Location ("test-file")

    "active accounts" in {

      val bike  = ActiveAccount (AccountId("bike"), UnitId("DKK"))
      val queue = ActiveAccount (AccountId("queue"), UnitId("DKK"), "a buffer awaiting for transfer")

      ActiveAccount (AccountId("queue"), UnitId("DKK"), "a buffer awaiting for transfer", l)
      DerivedAccount (AccountId("D"), BExpr(Ref(AccountId("bike"),NOLOC),Ref(AccountId("queue"),NOLOC),BOp.PLUS,NOLOC))

    }

    "invariants and assertions" in {

      Invariant (BExpr(Ref(AccountId("bike"),NOLOC), Ref(AccountId("queue"),NOLOC), BOp.EQ,NOLOC))
      Invariant (BExpr(Ref(AccountId("bike"),NOLOC), Ref(AccountId("queue"),NOLOC), BOp.EQ,NOLOC), Date ("20171201"))
      Assertion (BExpr(Ref(AccountId("bike"),NOLOC), Ref(AccountId("queue"),NOLOC), BOp.EQ,NOLOC), Date ("20171201"))
    }

    "operations" in {


      Operation (
        src = List(AccountId("bike"),AccountId("queue")),
        tgt = List(AccountId("queue")),
        value = Const (378138,Precision(2),NOLOC),
        tstamp = Date("20171201") )

      Operation (List(AccountId("bike")), Nil, Const(5000,Precision(2),NOLOC), Date("20171204"))
      Operation (List(AccountId("bike")), Nil, Const(5000,Precision(2),NOLOC), Date("20171204"), Nil, NOLOC, true)

    }

  }

}
