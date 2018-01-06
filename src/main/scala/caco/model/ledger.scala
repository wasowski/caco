package caco.model

object ledger {

type Id = String
type Description = List[String]
type Precision = Int

case class Date (value: String,
                 loc: Option[Location] = None) extends Traceable {
  def validate: Boolean = value.length == 8 // more TODO
}

case class Location (file: String, offset: Int = 0)


trait Named        { def id: Id                }
trait TimeStamped  { def tstamp: Date          }
trait Describable  { def descr: Description    }
trait Typed        { def unit: Unit            }
trait Traceable    { def loc: Option[Location] }
trait ModelElement { def validate = true       }


type Ledger = List[Line]

trait Line extends Describable with Traceable

case class Unit (
  id: Id,
  descr: Description = Nil,
  loc: Option[Location] = None,
  prec: Precision = 2
) extends Line with Named

trait Account extends Line with Named with Typed

case class ActiveAccount (
  id: Id,
  unit: Unit,
  descr: Description = Nil,
  loc: Option[Location] = None ) extends Account { }

case class DerivedAccount (
  id: Id,
  unit: Unit,
  value: Expr,
  descr: Description = Nil,
  loc: Option[Location] = None ) extends Account { }

case class Invariant (
  predicate: Expr,
  tstamp: Date = Date("00000000"),
  descr: Description = Nil,
  loc: Option[Location] = None) extends Line with TimeStamped

case class Assertion (
  predicate: Expr,
  tstamp: Date,
  descr: Description = Nil,
  loc: Option[Location] = None) extends Line with TimeStamped


case class Operation (
  src: List[Id], // ActiveAccount
  tgt: List[Id], // ActiveAccount
  value: Expr,
  tstamp: Date,
  descr: Description = Nil,
  loc: Option[Location] = None,
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

sealed trait UOp
case object UOp_MINUS extends UOp

object Unit {

  def apply (id: Id, descr: String,
    loc: Option[Location], prec: Precision): Unit =
      Unit (id, List(descr), loc, prec)

  def apply (id: Id, descr: String, loc: Location,
      prec: Precision): Unit =
      Unit (id, List(descr), Some(loc), prec)

  def apply (id: Id, descr: String, prec: Precision): Unit =
      Unit (id, List(descr), None, prec)

  def apply (id: Id, descr: String): Unit =
      Unit (id, List(descr), None, 2)
}


object ActiveAccount {

  def apply (id: Id, unit: Unit, descr: String): ActiveAccount =
      ActiveAccount (id, unit, List(descr), None)
  def apply (id: Id, unit: Unit, descr: String, loc: Location): ActiveAccount =
      ActiveAccount (id, unit, List(descr), Some(loc))
}


object Date {
  def apply (value: String, loc: Location): Date = Date(value,Some(loc))
}

}

