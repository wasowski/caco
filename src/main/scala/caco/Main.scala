package caco

import caco.model.ledger
import caco.model.runtime

import scalaz._
import Scalaz._

object Main extends App {

  // assume that the argument is a list of files for now

  for { // in[A]: StaticError \/ ledger.Ledger

    le <- Loader.load (args) |> Ast2Model.convert _
    runtimeState = le |> Interpreter.run _

    _ = println ("------------------------")
    _ = println (runtimeState (runtime.State.INITIAL)._1.toString)
    _ = println ("------------------------")

  } yield (runtimeState)

}
