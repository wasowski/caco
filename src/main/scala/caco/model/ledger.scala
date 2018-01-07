package caco.model

import caco.ast.{ledger => ast}

object ledger {

  // Types shared with AST

  type Date = ast.Date
  type Description = ast.Description
  type Id = ast.Id
  type Location = ast.Location
  type Precision = ast.Precision
  type Unit = ast.Unit

  type Named = ast.Named
  type TimeStamped = ast.TimeStamped
  type Describable = ast.Describable
  type Traceable = ast.Traceable

  trait Typed { def ty: Type }
  trait HasUnit { def unit: Unit }

  sealed trait Command extends TimeStamped with Describable with Traceable

  sealed trait Account extends Describable with HasUnit with Typed with Traceable {
    def ty = UnitTy (this.unit)
  }

  case class ActiveAccount (
    id: Id,
    unit: Unit,
    descr: Description,
    loc: Location,
    prec: Precision) extends Account

  case class DerivedAccount (
    id: Id,
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
  {
    def ty = value.ty
  }

  case class Invariant (
    predicate: Expr,
    tstamp: Date,
    descr: Description,
    loc: Location) extends Command

  case class Assertion (
    predicate: Expr,
    tstamp: Date,
    descr: Description,
    loc: Location) extends Command

  case class Ledger (
    units:    Map[Id,Unit],
    accounts: Map[Id,Account],
    commands: List[Command] )

  // Type language

  sealed trait Type
  case object BooleanTy extends Type
  case class  UnitTy (unit: Unit) extends Type

  // Expressions with resolved links

  sealed trait Expr extends Typed

  import caco.ast.ledger.operators._

  // TODO think how to avoid recomputing types, make them a decoration that is easily
  // accessible; perhaps we will need to make "untyped" reconstructors

  case class Ref (ac: Account, ty: Type) extends Expr
  case class BExpr (left: Expr, right: Expr, op: BOp, ty: Type) extends Expr
  case class UExpr (op: UOp, right: Expr, ty: Type) extends Expr
  case class Const (value: Long, prec: Precision, ty: Type) extends Expr // prec records how much we scaled up during parsing

}
