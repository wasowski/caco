package caco

import caco.ast.{ledger => in}
import caco.model.{ledger => out}

import scalaz._
import Scalaz._

object Ast2Model {

  import in.{Unit,Id}

  def convert (un_env: Map[Id, Unit]) (ac: in.ActiveAccount)
    : StaticError \/ out.ActiveAccount = for {
      un <- Maybe.fromOption (un_env.get (ac.unit))
              .toRight (StaticError ("Undefined unit:" + ac.unit,ac.loc))       // TODO: why is this so ugly?
    } yield out.ActiveAccount (ac.id, un, ac.descr, ac.loc)

  def convert (un_env: Map[Id, Unit], aa_env: Map[Id, out.ActiveAccount]) (ac: in.DerivedAccount)
    : StaticError \/ out.DerivedAccount = ???

  def convert (le: in.Ledger): StaticError \/ out.Ledger = {

    val un: List[Unit] = le
      .flatMap { case un: Unit => Some(un); case _ => None }

    val un_env: Map[Id,Unit] = un
      .map { (u: Unit) => u.id -> u }
      .toMap

    val aa: List[in.ActiveAccount] = le
      .flatMap { case ac: in.ActiveAccount => Some (ac); case _ => None }

    for {

      out_aa <- aa.map (convert (un_env)).sequenceU
      aa_env  = out_aa.map ((a: out.ActiveAccount) => a.id -> a).toMap
      da      = le.flatMap { case da: in.DerivedAccount => Some (da); case _ => None }
      out_dd <- da.map (convert (un_env, aa_env)).sequenceU

    } yield out.Ledger (un, ??? ::: out_aa, ???)

  }

}
