package caco

sealed trait Error {
  def msg: String
  def loc: Location
}

sealed trait InternalError extends Error

case class StaticError (msg: String, loc: Location) extends Error

case class ShouldNotHappen (msg: String = "Internal Error", loc: Location) extends InternalError

object ShouldNotHappen {

  def apply (fn: String) :ShouldNotHappen = ShouldNotHappen (loc=Location(fn))

}

