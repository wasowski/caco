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
  type TypeEnv = Map[out.TypeVarId, out.Type]


  def convert (ac: in.ActiveAccount) (un_env: UnitEnv): StaticError \/ out.ActiveAccount =
    for {
      un <- un_env.get (ac.unit) \/> StaticError ("Undefined unit:" + ac.unit, ac.loc)
    } yield out.ActiveAccount (ac.id, un, ac.descr, ac.loc)


  def convert (e: in.Expr) (ac_env: AccountEnv, ty_env: TypeEnv)
    : StaticError \/ (out.Expr, TypeEnv) = e match {

      case in.Ref (id,loc) =>
        for {
          ac <- ac_env.get (id) \/> StaticError ("Undefined account " + id, loc)
        } yield out.Ref (ac,  ac.ty, loc) -> ty_env

      case in.BExpr (left, right, op, loc) =>
        for {
          l_t <- convert (left) (ac_env, ty_env)
          r_t <- convert (right) (ac_env, l_t._2)
          ty_t <- unify (l_t._1.ty, r_t._1.ty) (loc) (r_t._2)
        } yield out.BExpr (l_t._1, r_t._1, op, ty_t._1, loc) -> ty_t._2

      case in.UExpr (op, right, loc) =>
        for {
          r_t <- convert (right) (ac_env, ty_env)
        } yield out.UExpr (op, r_t._1, r_t._1.ty, loc) -> r_t._2

      case in.Const (value, prec, loc) =>  {
        val id = out.TypeVarId(ty_env.size)
        val tyvar = out.TypeVar (id)
        \/- { out.Const(value, prec, tyvar, loc) -> (ty_env + (id -> tyvar)) }
      }
    }

  // This seems overly general, but may come useful later
  // with these we could almsot do untyped accounts type inference.
  // This is not entirely correct (as we do not shorten all the paths in the
  // uf-structure). Also we cannot very easily detect the root. It is unlikely
  // that this will cause trouble with the simple expression language that we
  // have now.

  def unify (ty1: out.Type, ty2: out.Type) (loc: Location) (ty_env: TypeEnv) :StaticError \/ (out.Type, TypeEnv) =
    (ty1, ty2) match {

      case (out.BooleanTy, out.BooleanTy) => (ty1, ty_env).right

      case (out.UnitTy (un1), out.UnitTy (un2)) =>
        if (un1 == un2) (ty1, ty_env).right
        else StaticError ("Incompatible units " + un1 + " and " + un2, loc).left

      case (_, out.TypeVar (va)) =>
        assert (ty_env.get (va).isDefined)
        if (ty_env (va) == ty2  || ty_env (va) == ty1)
          (ty1, ty_env + (va -> ty1)).right
        else StaticError ("Incompatible types " + ty1 + " and " + ty_env(va), loc).left

      case (out.TypeVar (va),_) =>
        assert (ty_env.get (va).isDefined)
        if (ty_env (va) == ty1  || ty_env (va) == ty2)
          (ty2, ty_env + (va -> ty2)).right
        else StaticError ("Incompatible types " + ty_env(va) + " and " + ty2, loc).left

      case (out.BooleanTy, _) | (_, out.BooleanTy) =>
        StaticError ("Incompatible types " + ty1 + " and " + ty2, loc).left
    }

  // TODO: something is still wrong (we are not unigying at BExprs, UExprs, etc)
  // or we are, but we are not enforcing the right type for the operands.
  // Perhaps fine -> we may do another typechecking pass. Although normally type
  // checking should be done during inference, so it would be better to simplify

  def concretize (e: out.Expr) (ty_env: TypeEnv): out.Expr = e match {
    case out.Ref (ac,  ty, loc) => out.Ref (ac, concretize (ty) (ty_env), loc)
    case out.BExpr (l,r,op,ty,loc) => ???
    case out.UExpr (op,r,ty,loc) => ???
    case out.Const (value, prec, tyvar, loc) => ???
  }

  def concretize (ty: out.Type) (ty_env: TypeEnv): out.Type = ???

  def trans_get (id: out.TypeVarId) (ty_env: TypeEnv): out.Type =  {
    assert (ty_env.get(id).isDefined)
    ty_env (id) match {
      case t @ (out.UnitTy (_) | out.BooleanTy) => t
      case t :out.TypeVar => if (t.id == id) t else trans_get (t.id) (ty_env)
    }
  }

  def convert (ac: in.DerivedAccount) (un_env: UnitEnv, aa_env: ActiveAccountEnv)
    : StaticError \/ out.DerivedAccount =
    for { // in[A]: StaticError \/ A
      va_t <- convert (ac.value) (aa_env, Map[out.TypeVarId,out.TypeVar]())
      va   =  concretize (va_t._1) (va_t._2)
      ty   <- va.ty match {
                case out.UnitTy (un) => un.right
                case _ => StaticError ("Expression defining a derived account " +
                                ac.id + " is not of known unit type", ac.loc).left }
      // TODO: ensure that no type variables remain (all type inference should
      // succeed within a single expression in this implementation - we might
      // not need more)
    } yield out.DerivedAccount (ac.id, va_t._1, ac.descr, ac.loc, ty)


  def convert (cm: in.Command) (un_env: UnitEnv, ac_env: AccountEnv)
    : StaticError \/ out.Command = cm match {
      case inv: in.Invariant => ???
      case as: in.Assertion => ???
      case op: in.Operation => ???
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
