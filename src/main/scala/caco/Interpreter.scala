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
  // start with assignments, then do the others.
  def processCommand (c: ledger.Command): MState[Unit] = State { case s => (s,()) }

  def outputUnit[A] (a: A): (A,Unit) = (a,()) // isn't this some standard scalaz operation?

  def processUnit (u: ledger.Unit): MState[Unit] =
    State {
      case s =>
        val c = CurrencyUnit register (u.id.toString,1,u.prec.##,List(),true);
        s.copy (units=s.units + (u.id->c)) |> outputUnit // TODO: lenses
    }

  def processAccount (a: ledger.Account): MState[Unit] =
    State {
      case s => a match {
        case a: ledger.ActiveAccount =>
          s.copy (accounts=s.accounts + (a.id -> unitV (a.unit))) |> outputUnit // TODO: lenses
        case a: ledger.DerivedAccount =>
          s.copy (derived=a::s.derived) |> outputUnit // TODO: lenses
      }
    }


  def run (l: ledger.Ledger): MState[Unit] =
    for { // in[A]: MState[A]

      _ <- l.units
             .map { processUnit _ }
             .sequenceU

      _ <- l.accounts
             .map { processAccount _ }
             .sequenceU

      _ <- l.commands
             .map { processCommand _ }
             .sequenceU

    } yield get

}
