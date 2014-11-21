/*
 * Copyright (c) 2013-15 Miles Sabin
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

import scala.util.Try

trait SingletonTypeUtils {
  val c: blackbox.Context
  import c.universe.{ Try => _, _ }
  import internal._, decorators._

  def singletonOpsTpe = typeOf[syntax.SingletonOps]
  val SymTpe = typeOf[scala.Symbol]

  object LiteralSymbol {
    def unapply(t: Tree): Option[String] = t match {
      case q""" scala.Symbol.apply(${Literal(Constant(s: String))}) """ => Some(s)
      case _ => None
    }
  }

  object SingletonSymbolType {
    val atatTpe = typeOf[tag.@@[_,_]].typeConstructor
    val TaggedSym = typeOf[tag.Tagged[_]].typeConstructor.typeSymbol

    def apply(s: String): Type = appliedType(atatTpe, List(SymTpe, constantType(Constant(s))))

    def unapply(t: Type): Option[String] =
      t match {
        case RefinedType(List(SymTpe, TypeRef(_, TaggedSym, List(ConstantType(Constant(s: String))))), _) => Some(s)
        case _ => None
      }
  }

  def mkSingletonSymbol(s: String): Tree = {
    val sTpe = SingletonSymbolType(s)
    q"""_root_.scala.Symbol($s).asInstanceOf[$sTpe]"""
  }

  object SingletonType {
    def unapply(t: Tree): Option[Type] = (t, t.tpe) match {
      case (Literal(k: Constant), _) => Some(constantType(k))
      case (LiteralSymbol(s), _) => Some(SingletonSymbolType(s))
      case (_, keyType: SingleType) => Some(keyType)
      case (q""" $sops.narrow """, _) if sops.tpe <:< singletonOpsTpe =>
        Some(sops.tpe.member(TypeName("T")).typeSignature)
      case _ => None
    }
  }

  def narrowValue(t: Tree): (Type, Tree) = {
    t match {
      case Literal(k: Constant) =>
        val tpe = constantType(k)
        (tpe, q"$t.asInstanceOf[$tpe]")
      case LiteralSymbol(s) => (SingletonSymbolType(s), mkSingletonSymbol(s))
      case _ => (t.tpe, t)
    }
  }

  def parseLiteralType(typeStr: String): Option[c.Type] =
    for {
      parsed <- Try(c.parse(typeStr)).toOption
      checked = c.typecheck(parsed, silent = true)
      if checked.nonEmpty
      tpe <- SingletonType.unapply(checked)
    } yield tpe

  def parseStandardType(typeStr: String): Option[c.Type] =
    for {
      parsed <- Try(c.parse(s"null.asInstanceOf[$typeStr]")).toOption
      checked = c.typecheck(parsed, silent = true)
      if checked.nonEmpty
    } yield checked.tpe

  def parseType(typeStr: String): Option[c.Type] =
    parseStandardType(typeStr) orElse parseLiteralType(typeStr)

  def typeCarrier(tpe: c.Type) = {
    val carrier = c.typecheck(tq"{ type T = $tpe }", mode = c.TYPEmode).tpe

    // We can't yield a useful value here, so return Unit instead which is at least guaranteed
    // to result in a runtime exception if the value is used in term position.
    Literal(Constant(())).setType(carrier)
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

  def mkAttributedRef(pre: Type, sym: Symbol): Tree = {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val gPre = pre.asInstanceOf[global.Type]
    val gSym = sym.asInstanceOf[global.Symbol]
    global.gen.mkAttributedRef(gPre, gSym).asInstanceOf[Tree]
  }

  def materializeImpl[T: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T].dealias
    val value =
      tpe match {
        case ConstantType(c: Constant) => Literal(c)

        case SingleType(p, v) if !v.isParameter => mkAttributedRef(p, v)

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

      case (SymTpe, LiteralSymbol(s)) =>
        mkResult(SingletonSymbolType(s), mkSingletonSymbol(s))

      case (tpe, tree) if tree.symbol.isTerm && tree.symbol.asTerm.isStable =>
        val sym = tree.symbol.asTerm
        val pre = if(sym.owner.isClass) c.internal.thisType(sym.owner) else NoPrefix
        val symTpe = c.internal.singleType(pre, sym)
        mkResult(symTpe, q"$sym.asInstanceOf[$symTpe]")

      case _ =>
        c.abort(c.enclosingPosition, s"Expression ${t.tree} does not evaluate to a constant or a stable value")
    }

  def convertImpl[T](t: Expr[T]): Tree = extractResult(t)(mkWitness)

  def witnessTypeImpl(tpeSelector: c.Tree): c.Tree = {
    val q"${tpeString: String}" = tpeSelector
    val tpe =
      parseLiteralType(tpeString)
        .getOrElse(c.abort(c.enclosingPosition, s"Malformed literal $tpeString"))

    typeCarrier(tpe)
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

  def mkSingletonOps(t: Expr[Any]): Tree =
    extractResult(t) { (tpe, tree) => mkOps(tpe, mkWitness(tpe, tree)) }

  def narrowSymbol[S <: String : WeakTypeTag](t: Expr[scala.Symbol]): Tree = {
    (weakTypeOf[S], t.tree) match {
      case (ConstantType(Constant(s1: String)), LiteralSymbol(s2)) if s1 == s2 =>
        mkSingletonSymbol(s1)
      case _ =>
        c.abort(c.enclosingPosition, s"Expression ${t.tree} is not an appropriate Symbol literal")
    }
  }
}
