package caco.model

object ledger {

type Id = String
type Description = List[String]
type Precision = Int

case class Date (value: String,
                 loc: Location = NOLOC) extends Traceable {
}

val NULLDATE = Date ("00000000")

case class Location (file: String, offset: Int = 0)
val NOLOC = Location ("",-1) // makes testing without parser easier, don't use outside testing code

trait Named        { def id: Id                }
trait TimeStamped  { def tstamp: Date          }
trait Describable  { def descr: Description    }
trait Typed        { def unit: Id              }
trait Traceable    { def loc: Location         }
trait ModelElement { def validate = true       }


type Ledger = List[Line]

trait Line extends Describable with Traceable

case class Unit (
  id: Id,
  descr: Description = Nil,
  loc: Location = NOLOC,
  prec: Precision = 2
) extends Line with Named

trait Account extends Line with Named

case class ActiveAccount (
  id: Id,
  unit: Id,
  descr: Description = Nil,
  loc: Location = NOLOC ) extends Account with Typed { }

case class DerivedAccount (
  id: Id,
  value: Expr,
  descr: Description = Nil,
  loc: Location = NOLOC ) extends Account { } // these should be typed, too but we need to infer the type

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
  src: List[Id], // ActiveAccount
  tgt: List[Id], // ActiveAccount
  value: Expr,
  tstamp: Date,
  descr: Description = Nil,
  loc: Location = NOLOC,
  pending: Boolean = false ) extends Line with TimeStamped


sealed trait Expr

case class Ref (id: Id) extends Expr
case class BExpr (left: Expr, right: Expr, op: BOp) extends Expr
case class UExpr (op: UOp, right: Expr) extends Expr
case class Const (value: Long, prec: Precision) extends Expr // prec records how much we scaled up during parsing

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

object Unit {

  def apply (id: Id, de: String, loc: Location, prec: Precision): Unit =
      Unit (id, List(de), loc, prec)

  def apply (id: Id, de: String, prec: Precision): Unit =
      Unit (id, List(de), NOLOC, prec)

  def apply (id: Id, de: String): Unit =
      Unit (id, List(de), NOLOC, 2)
}


object ActiveAccount {

  def apply (id: Id, un: Id, de: String): ActiveAccount =
      ActiveAccount (id, un, de ::Nil, NOLOC)

  def apply (id: Id, un: Id, de: String, lo: Location): ActiveAccount =
      ActiveAccount (id, un, de ::Nil, NOLOC)
}

}

