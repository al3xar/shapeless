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
package bootstrap

import scala.language.existentials
import scala.language.experimental.macros

import scala.reflect.macros.{ blackbox, whitebox }

trait SingletonTypeUtils {
  val c: blackbox.Context
  import c.universe._
  import internal.constantType

  val SymTpe = typeOf[scala.Symbol]

  object LiteralSymbol {
    def unapply(t: Tree): Option[Constant] = t match {
      case q""" scala.Symbol.apply(${Literal(c: Constant)}) """ => Some(c)
      case _ => None
    }
  }

  object SingletonSymbolType {
    val atatTpe = typeOf[tag.@@[_,_]].typeConstructor
    val TaggedSym = typeOf[tag.Tagged[_]].typeConstructor.typeSymbol

    def apply(c: Constant): Type = appliedType(atatTpe, List(SymTpe, constantType(c)))

    def unapply(t: Type): Option[Constant] =
      t match {
        case RefinedType(List(SymTpe, TypeRef(_, TaggedSym, List(ConstantType(c)))), _) => Some(c)
        case _ => None
      }
  }

  def mkSingletonSymbol(c: Constant): Tree = {
    val sTpe = SingletonSymbolType(c)
    q"""_root_.scala.Symbol($c).asInstanceOf[$sTpe]"""
  }
}

class SingletonTypeMacros(val c: whitebox.Context) extends SingletonTypeUtils {

  import c.universe._

  def mkWitness(sTpe: Type, s: Tree): Tree = {
    val name = TypeName(c.freshName())

    q"""
      {
        final class $name extends _root_.shapeless.Witness {
          type T = $sTpe
          val value: $sTpe = $s
        }
        new $name
      }
    """
  }

  def materializeImpl[T: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T].dealias
    val value =
      tpe match {
        case ConstantType(c: Constant) => Literal(c)

        case SingleType(p, v) if !v.isParameter => q"""$v.asInstanceOf[$tpe]"""

        case SingletonSymbolType(c) => mkSingletonSymbol(c)

        case _ =>
          c.abort(c.enclosingPosition, s"Type argument $tpe is not a singleton type")
      }
    mkWitness(tpe, value)
  }

  def extractResult[T](t: Expr[T])(mkResult: (Type, Tree) => Tree): Tree =
    (t.actualType, t.tree) match {
      case (tpe @ ConstantType(c: Constant), _) =>
        mkResult(tpe, Literal(c))

      case (tpe @ SingleType(p, v), tree) if !v.isParameter =>
        mkResult(tpe, tree)

      case (SymTpe, LiteralSymbol(c)) =>
        mkResult(SingletonSymbolType(c), mkSingletonSymbol(c))

      case _ =>
        c.abort(c.enclosingPosition, s"Expression ${t.tree} does not evaluate to a constant or a stable value")
    }

  def convertImpl[T](t: Expr[T]): Tree = extractResult(t)(mkWitness)

}
