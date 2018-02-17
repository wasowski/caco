package caco.ast

import caco.Location
import scalaz.@@

object ledger {

object Tag {
  sealed class AccountId
  sealed class UnitId
  sealed class Precision
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

object Named {

  def toMap[Id, A <: Named[Id]] (l: List[A]): Map[Id,A] =
    l.map { (a: A) => a.id -> a }.toMap

}

trait TimeStamped  { def tstamp: Date          }
trait Describable  { def descr: Description    }
trait Typed        { def unit: UnitId          }
trait Traceable    { def loc: Location         }
trait ModelElement { def validate = true       }


type Ledger = List[Line]

sealed trait Line extends Describable with Traceable {
  def getUnit: Option[Unit] = None
  def getActiveAccount: Option[ActiveAccount] = None
  def getDerivedAccount: Option[DerivedAccount] = None
  def getInvariant: Option[Invariant] = None
  def getAssertion: Option[Assertion] = None
  def getOperation: Option[Operation] = None
  def getCommand: Option[Command] = None
}

sealed trait Command extends Line with TimeStamped
{ override def getCommand: Option[Command] = Some (this) }

private val NOLOC = Location ("",-1) // makes testing without parser easier, don't use outside testing code

case class Unit (
  id: UnitId,
  descr: Description = Nil,
  loc: Location = NOLOC,
  prec: Precision = Precision(2)
) extends Line with Named[UnitId]
{ override def getUnit = Some(this) }


trait Account extends Line with Named[AccountId]

case class ActiveAccount (
  id: AccountId,
  unit: UnitId,
  descr: Description = Nil,
  loc: Location = NOLOC ) extends Account with Typed
{ override def getActiveAccount = Some(this) }

case class DerivedAccount (
  id: AccountId,
  value: Expr,
  descr: Description = Nil,
  loc: Location = NOLOC ) extends Account
{ override def getDerivedAccount = Some(this) }

case class Invariant (
  predicate: Expr,
  tstamp: Date = NULLDATE,
  descr: Description = Nil,
  loc: Location = NOLOC ) extends Command
{ override def getInvariant = Some(this) }

case class Assertion (
  predicate: Expr,
  tstamp: Date = NULLDATE,
  descr: Description = Nil,
  loc: Location = NOLOC ) extends Command
{ override def getAssertion = Some(this) }

case class Operation (
  src: List[AccountId], // ActiveAccount
  tgt: List[AccountId], // ActiveAccount
  value: Expr,
  tstamp: Date,
  descr: Description = Nil,
  loc: Location = NOLOC,
  pending: Boolean = false ) extends Command
{ override def getOperation = Some(this) }


sealed trait Expr extends Traceable

sealed trait UOp

object UOp {

  case object MINUS extends UOp
  case object NOT extends UOp

}



sealed trait BOp {
  def arith = false
  def logical = false
}

trait Arithmetic extends BOp { override def arith = true }
trait Logical extends BOp { override def logical = true }

object BOp {

  case object PLUS extends BOp with Arithmetic
  case object MINUS extends BOp with Arithmetic
  case object EQ  extends BOp with Arithmetic with Logical
  case object LT  extends BOp with Arithmetic
  case object LTE extends BOp with Arithmetic
  case object GT  extends BOp with Arithmetic
  case object GTE extends BOp with Arithmetic
  case object AND extends BOp with Logical
  case object OR extends BOp with Logical

}

case class Ref (id: AccountId, loc: Location) extends Expr
case class BExpr (left: Expr, right: Expr, op: BOp, loc: Location) extends Expr
case class UExpr (op: UOp, right: Expr, loc: Location) extends Expr
case class Const (value: Long, prec: Precision, loc: Location) extends Expr // prec records how much we scaled up during parsing

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

