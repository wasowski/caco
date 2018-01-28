package caco.model

import caco.ast.{ledger => ast}
import caco.Location
import scalaz.@@

object ledger {

  // Types shared with AST

  type Date = ast.Date
  type Description = ast.Description
  type AccountId = ast.AccountId

  type UnitId = ast.UnitId
  val UnitId = ast.UnitId

  type Precision = ast.Precision
  val Precision = ast.Precision

  type Unit = ast.Unit

  type Named[Id] = ast.Named[Id]
  type TimeStamped = ast.TimeStamped
  type Describable = ast.Describable
  type Traceable = ast.Traceable

  trait Typed { def ty: Type }
  trait HasUnit { def unit: Unit }

  sealed trait Command extends TimeStamped with Describable with Traceable

  sealed trait Account extends Describable with HasUnit with Typed with Traceable with Named[AccountId] {
    def ty: Type = UnitTy (unit)
    def prec: Precision = unit.prec
  }

  case class ActiveAccount (
    id: AccountId,
    unit: Unit,
    descr: Description,
    loc: Location,
  ) extends Account

  case class DerivedAccount (
    id: AccountId,
    value: Expr,
    descr: Description,
    loc: Location,
    unit: Unit) extends Account

  case class Operation (
    src: List[ActiveAccount],
    tgt: List[ActiveAccount],
    value: Expr,
    tstamp: Date,
    descr: Description,
    loc: Location,
    pending: Boolean,
    unit: Unit) extends Command with Typed
  { def ty = value.ty }

  case class Invariant (
    predicate: Expr,
    tstamp: Date,
    descr: Description,
    loc: Location) extends Command

  case class Assertion (
    predicate: Expr,
    tstamp: Date,
    descr: Description,
    loc: Location ) extends Command

  case class Ledger (
    units:    List[Unit],
    accounts: List[Account],
    commands: List[Command] )

  // Type language

  object Tag {
    sealed class TypeVarId
  }


  type TypeVarId = Int @@ Tag.TypeVarId
  object TypeVarId {
    def apply (n: Int) = n.asInstanceOf[TypeVarId]
    def unapply (n: TypeVarId): Option[Int] = Some(n.asInstanceOf[Int])
  }

  sealed trait Type
  case object BooleanTy extends Type
  { override def toString = "Boolean" }
  case class  UnitTy (unit: Unit) extends Type
  { override def toString = unit.id.toString }
  case class  TypeVar (id: TypeVarId) extends Type with Named[TypeVarId]
  { override def toString = "'" + id }

  // Expressions with resolved links

  sealed trait Expr extends Typed with Traceable

  type UOp = ast.UOp
  type BOp = ast.BOp
  val BOp = ast.BOp
  val UOp = ast.UOp

  case class Ref (ac: Account, ty: Type, loc:Location) extends Expr
  case class BExpr (left: Expr, right: Expr, op: BOp, ty: Type, loc: Location) extends Expr
  case class UExpr (op: UOp, right: Expr, ty: Type, loc: Location) extends Expr
  case class Const (value: Long, prec: Precision, ty: Type, loc: Location) extends Expr // prec records how much we scaled up during parsing

}
