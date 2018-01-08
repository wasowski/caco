package caco

sealed trait Error {
  def msg: String
  def loc: Location
}

case class StaticError (msg: String, loc: Location) extends Error

