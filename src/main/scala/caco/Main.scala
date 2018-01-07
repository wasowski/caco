package caco

import caco.model.ledger.Ledger

object Main extends App {

  // we assume that the argument is a list of files for now

  var le: Ledger = Loader.load (args)

  print(le map (_.toString) mkString "\n")

}
