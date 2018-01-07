package caco

import model.ledger.Ledger
import scala.util.{Success,Failure}

object Loader {

  def load (fi: String): Ledger = {

    val sr = scala.io.Source.fromFile (fi)
    var pa = LedgerParser (sr.mkString, fi)
    pa.main.run() match {
      case Success(le1) =>
        sr.close
        le1
      case Failure(er: org.parboiled2.ParseError) =>
        print (pa formatError er)
        sr.close
        throw er
      case Failure(ex @ _) =>
        sr.close
        throw ex
      }

  }

  def load (fi: Seq[String]): Ledger =
    fi.sorted
      .foldLeft[List[Ledger]] (Nil) { (le: List[Ledger], fi: String) => load (fi) ::le }
      .reverse
      .flatten
}
