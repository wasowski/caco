package caco

import caco.model.ledger.Ledger

import scalaz._
import Scalaz._

object Main extends App {

  // we assume that the argument is a list of files for now

  val le: StaticError \/ Ledger =
    Loader.load (args) |> (Ast2Model.convert _)



  print(le)


}
