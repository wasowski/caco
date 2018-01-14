package caco.ast

import caco.Location
import scalaz.{Tag, @@}


object ledger {

object Tag {
  sealed class AccountId
  sealed class UnitId
  sealed class Precision
  sealed class Description
}

type AccountId = String @@ Tag.AccountId
object AccountId {
  def apply(s: String) = s.asInstanceOf[AccountId]
  def unapply (s: AccountId): Option[String] = Some(s.asInstanceOf[String])
}

type UnitId = String @@ Tag.UnitId
object UnitId {
  def apply(s: String) = s.asInstanceOf[UnitId]
  def unapply (s: UnitId): Option[String] = Some(s.asInstanceOf[String])
}

// not tagging descriptions as the risk of confusion is close to none
type Description = List[String]



type Precision = Int @@ Tag.Precision
object Precision {
  def apply (n: Int) = n.asInstanceOf[Precision]
  def unapply (pr: Precision): Option[Int] = Some(pr.asInstanceOf[Int])
}

case class Date (value: String,
                 loc: Location = NOLOC) extends Traceable {
}

private val NULLDATE = Date ("00000000")

trait Named[Id]    { def id: Id
                     def toString: String      }
trait TimeStamped  { def tstamp: Date          }
trait Describable  { def descr: Description    }
trait Typed        { def unit: UnitId          }
trait Traceable    { def loc: Location         }
trait ModelElement { def validate = true       }


type Ledger = List[Line]

sealed trait Line extends Describable with Traceable

private val NOLOC = Location ("",-1) // makes testing without parser easier, don't use outside testing code

case class Unit (
  id: UnitId,
  descr: Description = Nil,
  loc: Location = NOLOC,
  prec: Precision = Precision(2)

) extends Line with Named[UnitId]

trait Account extends Line with Named[AccountId]

case class ActiveAccount (
  id: AccountId,
  unit: UnitId,
  descr: Description = Nil,
  loc: Location = NOLOC ) extends Account with Typed

case class DerivedAccount (
  id: AccountId,
  value: Expr,
  descr: Description = Nil,
  loc: Location = NOLOC ) extends Account

case class Invariant (
  predicate: Expr,
  tstamp: Date = NULLDATE,
  descr: Description = Nil,
  loc: Location = NOLOC ) extends Line with TimeStamped

case class Assertion (
  predicate: Expr,
  tstamp: Date = NULLDATE,
  descr: Description = Nil,
  loc: Location = NOLOC ) extends Line with TimeStamped

case class Operation (
  src: List[AccountId], // ActiveAccount
  tgt: List[AccountId], // ActiveAccount
  value: Expr,
  tstamp: Date,
  descr: Description = Nil,
  loc: Location = NOLOC,
  pending: Boolean = false ) extends Line with TimeStamped


sealed trait Expr

object operators {

  sealed trait BOp
  case object BOp_PLUS extends BOp
  case object BOp_MINUS extends BOp
  case object BOp_EQ  extends BOp
  case object BOp_LT  extends BOp
  case object BOp_LTE extends BOp
  case object BOp_GT  extends BOp
  case object BOp_GTE extends BOp
  case object BOp_AND extends BOp
  case object BOp_OR extends BOp

  sealed trait UOp
  case object UOp_MINUS extends UOp
}

import operators._

case class Ref (id: AccountId) extends Expr
case class BExpr (left: Expr, right: Expr, op: BOp) extends Expr
case class UExpr (op: UOp, right: Expr) extends Expr
case class Const (value: Long, prec: Precision) extends Expr // prec records how much we scaled up during parsing

// companion objects

object Unit {

  def apply (id: UnitId, de: String, loc: Location, prec: Precision): Unit =
      Unit (id, List(de), loc, prec)

  def apply (id: UnitId, de: String, prec: Precision): Unit =
      Unit (id, List(de), NOLOC, prec)

  def apply (id: UnitId, de: String): Unit =
      Unit (id, List(de), NOLOC)
}


object ActiveAccount {

  def apply (id: AccountId, un: UnitId, de: String): ActiveAccount =
      ActiveAccount (id, un, de ::Nil, NOLOC)

  def apply (id: AccountId, un: UnitId, de: String, lo: Location): ActiveAccount =
      ActiveAccount (id, un, de ::Nil, NOLOC)
}

}

