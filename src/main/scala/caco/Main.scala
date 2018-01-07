package caco

import caco.ast.ledger.Ledger

object Main extends App {

  // we assume that the argument is a list of files for now

  var le: Ledger = Loader.load (args)

  print(le map (_.toString) mkString "\n")

  // name analysis

  // case class Ledger (
  //   units: Map[Id,Unit]
  //   accounts: Map[Id,Account],
  //   operations: List[Operation],
  //   invariants: List[Invariant],
  //   assertions: List[Assertions]
  // )

}
