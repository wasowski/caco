package caco

import caco.model.runtime
import caco.model.ledger

import com.github.nscala_money.money.Imports._

import scalaz._
import Scalaz._

object Interpreter {

  type MState[A] = State[runtime.State,A]

  def unitV (u: ledger.Unit, v: Long = 0) :Money =
    Money.of (CurrencyUnit.of(u.id.toString), v)

  // TODO implement
  def processCommand (c: ledger.Command): MState[Unit] = State { case s => (s,()) }

  def processUnit (u: ledger.Unit): MState[Unit] =
    State {
      case s =>
        val c = CurrencyUnit register (u.id.toString,1,u.prec.##,List(),true);
        (s.copy (units=s.units + (u.id->c)),()) // TODO: lenses
    }

  def processAccount (a: ledger.Account): MState[Unit] =
    State {
      case s => (a match {
        case a: ledger.ActiveAccount => s.copy(accounts=s.accounts + (a.id -> unitV (a.unit))) // TODO: lenses
        case a: ledger.DerivedAccount => s.copy(derived=a::s.derived)
      },())
    }


  def run (l: ledger.Ledger): MState[Unit] =
    for { // in[A]: MState[A]

      _ <- l.units
             .map { processUnit _ }
             .sequenceU
             .map { _ => () }

      _ <- l.accounts
             .map { processAccount _ }
             .sequenceU
             .map { _ => () }

      _ <- l.commands
             .map { processCommand _ }
             .sequenceU
             .map { _ => () }

    } yield get

}
