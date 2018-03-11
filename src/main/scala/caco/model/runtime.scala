/** Runtime state model */
package caco.model

import caco.model.ledger._
import com.github.nscala_money.money.Imports._

object runtime {

  type Precision = caco.ast.ledger.Precision
  val Precision = caco.ast.ledger.Precision
  import Precision._

  type AccountId = ledger.AccountId

  // The units map is really fake (the org.joda.money) library tracks this
  // imperatively.  I maintain this redundant map, so that it is clear that this
  // is part of state (a type hack, an ugly one to admit)
  case class State (
    units: Map[UnitId,CurrencyUnit],
    accounts: Map[AccountId,Money],
    derived: List[ledger.DerivedAccount])
  {
    def balance (account: ledger.AccountId): Money = accounts (account)
    def balance (account: ledger.Account): Money = accounts (account.id)

    lazy val idColumnWidth =
      scala.math.max(
        accounts.keys       .map { _.toString.length }.max,
        derived.map { _.id }.map { _.toString.length }.max)

    override def toString: String =
      accounts
        .map { case (a,v) => f"${a}%-20s ${v.toString}" }
        .mkString ("\n")

  }

  object State {

    val INITIAL = State (Map(),Map(), List())
  }

}
