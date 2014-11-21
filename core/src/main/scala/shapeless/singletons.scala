/*
 * Copyright (c) 2013-14 Miles Sabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package shapeless

import scala.language.existentials
import scala.language.experimental.macros

import scala.reflect.macros.{ blackbox, whitebox }

import tag.@@

trait Witness {
  type T
  val value: T {}
}

object Witness {
  type Aux[T0] = Witness { type T = T0 }
  type Lt[Lub] = Witness { type T <: Lub }

  implicit def apply[T]: Witness.Aux[T] = macro bootstrap.SingletonTypeMacros.materializeImpl[T]

  implicit def apply[T](t: T): Witness.Lt[T] = macro bootstrap.SingletonTypeMacros.convertImpl[T]

  implicit val witness0: Witness.Aux[_0] =
    new Witness {
      type T = _0
      val value = Nat._0
    }

  implicit def witnessN[P <: Nat]: Witness.Aux[Succ[P]] =
    new Witness {
      type T = Succ[P]
      val value = new Succ[P]()
    }
  
  object Boolean {
    val witnessTrue = Witness(true)
    val witnessFalse = Witness(false)
  }

}

trait WitnessWith[TC[_]] extends Witness {
  val instance: TC[T]
}

trait LowPriorityWitnessWith {
  implicit def apply2[H, TC2[_ <: H, _], S <: H, T](t: T): WitnessWith.Lt[({ type λ[X] = TC2[S, X] })#λ, T] =
    macro SingletonTypeMacros.convertInstanceImpl2[H, TC2, S, T]
}

object WitnessWith extends LowPriorityWitnessWith {
  type Aux[TC[_], T0] = WitnessWith[TC] { type T = T0  }
  type Lt[TC[_], Lub] = WitnessWith[TC] { type T <: Lub }

  implicit def apply1[TC[_], T](t: T): WitnessWith.Lt[TC, T] = macro SingletonTypeMacros.convertInstanceImpl1[TC, T]
}

class SingletonTypeMacros(override val c: whitebox.Context) extends bootstrap.SingletonTypeMacros(c) {
  import syntax.SingletonOps
  type SingletonOpsLt[Lub] = SingletonOps { type T <: Lub }

  import c.universe._
  import internal._
  import decorators._

  def mkWitnessWith(parent: Type, sTpe: Type, s: Tree, i: Tree): Tree = {
    val name = TypeName(c.freshName())
    val iTpe = i.tpe.finalResultType

    q"""
      {
        final class $name extends $parent {
          val instance: $iTpe = $i
          type T = $sTpe
          val value: $sTpe = $s
        }
        new $name
      }
    """
  }

  def mkOps(sTpe: Type, w: Tree): Tree = {
    val name = TypeName(c.freshName())

    q"""
      {
        final class $name extends _root_.shapeless.syntax.SingletonOps {
          type T = $sTpe
          val witness = $w
        }
        new $name
      }
    """
  }

  def inferInstance(tci: Type): Tree = {
    val i = c.inferImplicitValue(tci)
    if(i == EmptyTree)
      c.abort(c.enclosingPosition, s"Unable to resolve implicit value of type $tci")
    i
  }

  def convertInstanceImpl1[TC[_], T](t: Expr[T])
    (implicit tcTag: WeakTypeTag[TC[_]]): Tree =
      extractResult(t) { (sTpe, value) =>
        val tc = tcTag.tpe.typeConstructor
        val wwTC = typeOf[WitnessWith[Nothing]].typeConstructor
        val parent = appliedType(wwTC, List(tc))
        val tci = appliedType(tc, List(sTpe))
        val i = inferInstance(tci)
        mkWitnessWith(parent, sTpe, value, i)
      }

  def convertInstanceImpl2[H, TC2[_ <: H, _], S <: H, T](t: Expr[T])
    (implicit tc2Tag: WeakTypeTag[TC2[_, _]], sTag: WeakTypeTag[S]): Tree =
      extractResult(t) { (sTpe, value) =>
        val tc2 = tc2Tag.tpe.typeConstructor
        val s = sTag.tpe

        val parent = weakTypeOf[WitnessWith[({ type λ[X] = TC2[S, X] })#λ]].map {
          case TypeRef(prefix, sym, args) if sym.isFreeType =>
            typeRef(NoPrefix, tc2.typeSymbol, args)
          case tpe => tpe
        }

        val tci = appliedType(tc2, List(s, sTpe))
        val i = inferInstance(tci)
        mkWitnessWith(parent, sTpe, value, i)
      }

  def mkSingletonOps(t: Expr[Any]): Tree =
    extractResult(t) { (tpe, tree) => mkOps(tpe, mkWitness(tpe, tree)) }

  def narrowSymbol[S <: String : WeakTypeTag](t: Expr[scala.Symbol]): Tree = {
    (weakTypeOf[S], t.tree) match {
      case (ConstantType(Constant(s1)), LiteralSymbol(Constant(s2))) if s1 == s2 =>
        mkSingletonSymbol(Constant(s1))
      case _ =>
        c.abort(c.enclosingPosition, s"Expression ${t.tree} is not an appropriate Symbol literal")
    }
  }
}
