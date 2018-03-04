/** Runtime state model */
package caco.model

object runtime {

  type Precision = ledger.Precision
  type AccountId = ledger.AccountId

  case class Balance (value: Int, prec: Precision)

  case class State (accounts: Map[AccountId,Balance])
  {
    def balance (account: ledger.AccountId): Balance = accounts (account)
    def balance (account: ledger.Account): Balance = accounts (account.id)
  }

}
