package caco

import caco.model.ledger.Ledger

import scalaz._
import Scalaz._

object Main extends App {

  // assume that the argument is a list of files for now

  for { // in[A]: StaticError \/ Ledger

    le <- Loader.load (args) |> Ast2Model.convert _
    runtimeState = le |> Interpreter.run _

    _ = print(le)
    _ = print(runtimeState)

  } yield (runtimeState)

}
