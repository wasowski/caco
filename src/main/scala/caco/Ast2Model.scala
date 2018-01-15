package caco

import caco.ast.{ledger => in}
import caco.model.{ledger => out}

import scalaz._
import Scalaz._

object Ast2Model {

  import in.{Unit,UnitId,AccountId}

  type UnitEnv = Map[UnitId, Unit]
  type ActiveAccountEnv = Map[AccountId, out.ActiveAccount]
  type DerivedAccountEnv = Map[AccountId, out.DerivedAccount]


  def convert (ac: in.ActiveAccount) (un_env: UnitEnv): StaticError \/ out.ActiveAccount =
    for {
      un <- un_env.get (ac.unit) \/> StaticError ("Undefined unit:" + ac.unit,ac.loc)
    } yield out.ActiveAccount (ac.id, un, ac.descr, ac.loc)


  def convert (ex: in.Expr) (un_env: UnitEnv, aa_env: ActiveAccountEnv)
    : StaticError \/ (out.Expr, Unit) = ??? // TODO


  def convert (ac: in.DerivedAccount) (un_env: UnitEnv, aa_env: ActiveAccountEnv)
    : StaticError \/ out.DerivedAccount =
    for {
      vaun <- convert (ac.value) (un_env, aa_env)
    } yield out.DerivedAccount (ac.id, vaun._1, ac.descr, ac.loc, vaun._2)


  def convert (le: in.Ledger): StaticError \/ out.Ledger =

    for { // in: forall A. StaticError \/ A

      un     <- le.flatMap { _.getUnit }.right // TODO: can't remove 'right'?
      un_env =  un.map { (u: Unit) => u.id -> u }.toMap

      aa     =  le.flatMap { _.getActiveAccount }
      out_aa <- aa.map { (a: in.ActiveAccount) => convert (a) (un_env)}.sequenceU
      aa_env =  out_aa.map ((a: out.ActiveAccount) => a.id -> a).toMap

      da     =  le flatMap { _.getDerivedAccount }
      out_dd <- da.map { (a: in.DerivedAccount) => convert (a) (un_env, aa_env)}.sequenceU

    } yield out.Ledger (un, ??? ::: out_aa, ???)

}
