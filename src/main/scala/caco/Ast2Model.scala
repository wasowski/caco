package caco

import caco.ast.{ledger => in}
import caco.model.{ledger => out}

import scalaz._
import Scalaz._

object Ast2Model {

  import in.{Unit,UnitId,AccountId}

  type UnitEnv = Map[UnitId, Unit]
  type AccountEnv = Map[AccountId, out.Account]
  type ActiveAccountEnv = Map[AccountId, out.ActiveAccount]
  type DerivedAccountEnv = Map[AccountId, out.DerivedAccount]

  def convert (ac: in.ActiveAccount) (un_env: UnitEnv): StaticError \/ out.ActiveAccount =
    for {
      un <- un_env.get (ac.unit) \/> StaticError ("Undefined unit:" + ac.unit, ac.loc)
    } yield out.ActiveAccount (ac.id, un, ac.descr, ac.loc)

  def convert (e: in.Expr) (ac_env: AccountEnv) : StaticError \/ out.Expr = e match {

      case in.Ref (id,loc) =>
        for { ac <- ac_env.get (id) \/> StaticError ("Undefined account " + id, loc) }
        yield out.Ref (ac,  ac.ty, loc)

      case in.BExpr (left, right, op, loc) =>
        for { l <- convert (left) (ac_env)
              r <- convert (right) (ac_env)
              t1 <- unify (l.ty, r.ty) (loc)
              t2 <- if (op.arith && !op.logical) unify (t1, out.NumericTy) (loc)
                    else if (op.logical && !op.arith) unify (t1, out.BooleanTy) (loc)
                    else t1.right
        } yield out.BExpr (l, r, op, t2, loc)

      case in.UExpr (op, right, loc) =>
        for {
          r <- convert (right) (ac_env)
          t <- op match { case in.UOp.MINUS => unify (r.ty, out.NumericTy) (loc)
                          case in.UOp.NOT => unify (r.ty, out.BooleanTy) (loc) }
        } yield out.UExpr (op, r, t, loc)

      case in.Const (value, prec, loc) =>
        \/- { out.Const(value, prec, out.NumericTy, loc) }

    }

  def unify (ty1: out.Type, ty2: out.Type) (loc: Location) :StaticError \/ out.Type =
    (ty1, ty2) match {

      case (out.BooleanTy, out.BooleanTy) => ty1.right

      case (out.UnitTy (un1), out.UnitTy (un2)) =>
        if (un1 == un2) ty1.right
        else StaticError ("Incompatible units " + un1 + " and " + un2, loc).left

      case (out.BooleanTy,out.NumericTy) =>
        StaticError ("Incompatible types: Boolean and Numeric", loc).left
      case (out.NumericTy,out.BooleanTy) =>
        StaticError ("Incompatible types: Numeric and Boolean", loc).left

      case (ty, out.NumericTy) => ty.right
      case (out.NumericTy, ty) => ty.right

      case (out.BooleanTy, _) | (_, out.BooleanTy) =>
        StaticError ("Incompatible types " + ty1 + " and " + ty2, loc).left
    }



  def convert (ac: in.DerivedAccount) (un_env: UnitEnv, aa_env: ActiveAccountEnv)
    : StaticError \/ out.DerivedAccount =
    for { // in[A]: StaticError \/ A

      expr <- convert (ac.value) (aa_env)
      un <- expr.ty match {
                case out.UnitTy (un) => un.right
                case _ =>
                    StaticError ("Expression defining a derived account " +
                                 " is not of known unit type", ac.loc).left
      }
    } yield out.DerivedAccount (ac.id, expr, ac.descr, ac.loc, un)


  def convert (inv: in.Invariant) (un_env: UnitEnv, ac_env: AccountEnv)
    : StaticError \/ out.Invariant =
    for { expr <- convert (inv.predicate) (ac_env) }
    yield out.Invariant (expr, inv.tstamp, inv.descr, inv.loc)

  // TODO: we need a converter of assertions as well
  def convert (assert: in.Assertion) (un_env: UnitEnv, ac_env: AccountEnv)
    : StaticError \/ out.Assertion = ???

  // Should this be moved out to model?
  def ensureActive[B] (error: => B) (account: out.Account): B \/ out.ActiveAccount =
    account match {
      case a: out.ActiveAccount => \/- (a)
      case _ => -\/ (error)
    }

  def convert (oper: in.Operation) (ac_env: AccountEnv)
    : StaticError \/ out.Operation = {

    val errAccountGet = StaticError ("Operations can only get means from active accounts", oper.loc)
    val errAccountSet = StaticError ("Operations can only put means into active accounts", oper.loc)
    val errUnit = StaticError ("All source and target account of an operation must have the same unit", oper.loc)

    for { // in[A]: StaticError \/ A

      src <- oper.src
              .map { ac_env (_) }
              .right
              .flatMap { _.map { ensureActive (errAccountGet) (_) }.sequenceU }

      tgt <- oper.tgt
              .map { a => ac_env (a) }
              .right
              .flatMap { _.map { ensureActive (errAccountSet) (_) }.sequenceU }

      all = src ++ tgt

      expr <- convert (oper.value) (ac_env)

      unit <- all
              .map {_.unit}
              .right
              .ensure (errUnit) { !_.exists (_ != all.head.unit)  }
              .map { _.head }

    } yield out.Operation (src, tgt, expr, oper.tstamp, oper.descr, oper.loc, oper.pending, unit)

  }




  def convert (cm: in.Command) (un_env: UnitEnv, ac_env: AccountEnv)
    : StaticError \/ out.Command = cm match {
      case inv: in.Invariant =>  convert (inv) (un_env, ac_env)
      case as: in.Assertion => convert (as) (un_env, ac_env)
      case op: in.Operation => convert (op) (ac_env)
  }



  def convert (le: in.Ledger): StaticError \/ out.Ledger =

    for { // in[A]: StaticError \/ A

      un     <- le.flatMap { _.getUnit }.right[StaticError]
      un_env =  in.Named.toMap[UnitId,Unit] (un)

      aa     =  le.flatMap { _.getActiveAccount }
      out_aa <- aa.map { (a: in.ActiveAccount) => convert (a) (un_env)}.sequenceU
      aa_env =  in.Named.toMap[out.AccountId,out.ActiveAccount] (out_aa)

      da     =  le flatMap { _.getDerivedAccount }
      out_da <- da.map { (a: in.DerivedAccount) => convert (a) (un_env, aa_env)}.sequenceU
      da_env =  in.Named.toMap[out.AccountId,out.DerivedAccount] (out_da)

      ac_env = aa_env ++ da_env

      cm     =  le flatMap { _.getCommand }
      out_cm <- cm.map { (c: in.Command) => convert (c) (un_env, ac_env)}.sequenceU

    } yield out.Ledger (un, out_aa ::: out_da, out_cm)

}
