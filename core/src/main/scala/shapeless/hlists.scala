/*
 * Copyright (c) 2011-15 Miles Sabin 
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

import scala.language.dynamics
import scala.language.experimental.macros

import scala.annotation.tailrec
import scala.reflect.macros.whitebox

/**
 * `HList` ADT base trait.
 * 
 * @author Miles Sabin
 */
sealed trait HList extends Product with Serializable {

  def mkString(start: String, sep: String, end: String): String =
    addString(new StringBuilder(), start, sep, end).toString

  def mkString(sep: String): String = mkString("", sep, "")

  def mkString: String = mkString("")

  def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    @tailrec
    def helper(l: HList, first: Boolean): Unit =
      l match {
        case h :: t =>
          if (!first)
            b append sep
          
          b append h
          helper(t, first = false)
        case HNil =>
      }

    b append start
    helper(this, first = true)
    b append end

    b
  }

  def addString(b: StringBuilder, sep: String): StringBuilder = addString(b, "", sep, "")

  def addString(b: StringBuilder): StringBuilder = addString(b, "")

  override def toString = mkString("HList(", ", ", ")")

}

/**
 * Non-empty `HList` element type.
 * 
 * @author Miles Sabin
 */
final case class ::[+H, +T <: HList](head : H, tail : T) extends HList with Dynamic {
  import shapeless.tag.@@
  import ops.record.Selector

  /**
   * Allows dynamic-style access to fields of a record whose keys are Symbols.
   */
  def selectDynamic(key: String)(implicit selector: Selector[H :: T, Symbol @@ key.type]): selector.Out = selector(this)
}

/**
 * Empty `HList` value.
 * 
 * @author Miles Sabin
 */
case object HNilInstance extends HList

object HList extends Dynamic {
  import ops.hlist._
  import syntax.HListOps

  def apply() = HNil

  def apply[T](t: T) = t :: HNil
  
  def apply[P <: Product, L <: HList](p : P)(implicit gen: Generic.Aux[P, L]) : L = gen.to(p)

  /**
   * Produces a HList of length `N` filled with `elem`.
   */
  def fill[A](n: Nat)(elem: A)(implicit fill: Fill[n.N, A]) : fill.Out = fill(elem)

  /**
   * Produces a `N1`-length HList made of `N2`-length HLists filled with `elem`.
   */
  def fill[A](n1: Nat, n2: Nat)(elem: A)(implicit fill: Fill[(n1.N, n2.N), A]) : fill.Out = fill(elem)
  
  implicit def hlistOps[L <: HList](l : L) : HListOps[L] = new HListOps(l)

  /**
   * Convenience aliases for HList :: and List :: allowing them to be used together within match expressions.  
   */
  object ListCompat {
    val :: = scala.collection.immutable.::
    val #: = shapeless.::
  }

  /**
   * Allows to specify an `HList` type with a syntax similar to `Record` and `Union`, as follows,
   * 
   * {{{
   * type ISB = HList.`Int, String, Boolean`.T
   * }}}
   * 
   * Literal types are allowed, so that the following is valid,
   * 
   * {{{
   * type ABC = HList.`'a, 'b, 'c`.T
   * type TwoTrueStr = HList.`2, true, "str"`.T
   * }}}
   */
  def selectDynamic(tpeSelector: String): Any = macro LabelledMacros.hlistTypeImpl
}

/**
 * Trait supporting mapping dynamic argument lists to HList arguments.
 *
 * Mixing in this trait enables method applications of the form,
 *
 * {{{
 * lhs.method(23, "foo", true)
 * }}}
 *
 * to be rewritten as,
 *
 * {{{
 * lhs.methodProduct(23 :: "foo" :: true)
 * }}}
 *
 * ie. the arguments are rewritten as HList elements and the application is
 * rewritten to an application of an implementing method (identified by the
 * "Product" suffix) which accepts a single HList argument.
 */
trait ProductArgs extends Dynamic {
  def applyDynamic(method: String)(args: Any*): Any = macro ProductMacros.forwardImpl
}

/**
 * Trait supporting mapping dynamic argument lists to singleton-typed HList arguments.
 *
 * Mixing in this trait enables method applications of the form,
 *
 * {{{
 * lhs.method(23, "foo", true)
 * }}}
 *
 * to be rewritten as,
 *
 * {{{
 * lhs.methodProduct(23.narrow :: "foo".narrow :: true.narrow)
 * }}}
 *
 * ie. the arguments are rewritten as singleton-typed HList elements and the
 * application is rewritten to an application of an implementing method (identified by the
 * "Product" suffix) which accepts a single HList argument.
 */
trait SingletonProductArgs extends Dynamic {
  def applyDynamic(method: String)(args: Any*): Any = macro ProductMacros.forwardSingletonImpl
}

class ProductMacros(val c: whitebox.Context) extends bootstrap.SingletonTypeUtils with ReprTypes {
  import c.universe._
  import internal.constantType

  def forwardImpl(method: Tree)(args: Tree*): Tree = forward(method, args, false)

  def forwardSingletonImpl(method: Tree)(args: Tree*): Tree = forward(method, args, true)

  def forward(method: Tree, args: Seq[Tree], narrow: Boolean): Tree = {
    val lhs = c.prefix.tree 
    val lhsTpe = lhs.tpe

    val q"${methodString: String}" = method
    val methodName = TermName(methodString+"Product")

    if(lhsTpe.member(methodName) == NoSymbol)
      c.abort(c.enclosingPosition, s"missing method '$methodName'")

    val argsTree = mkProductImpl(args, narrow)

    q""" $lhs.$methodName($argsTree) """
  }

  def mkProductImpl(args: Seq[Tree], narrow: Boolean): Tree = {
    args.foldRight((hnilTpe, q"_root_.shapeless.HNil: $hnilTpe": Tree)) {
      case(elem, (accTpe, accTree)) =>
        val (neTpe, neTree) = if(narrow) narrowValue(elem) else (elem.tpe, elem)
        (appliedType(hconsTpe, List(neTpe, accTpe)), q"""_root_.shapeless.::[$neTpe, $accTpe]($neTree, $accTree)""")
    }._2
  }
}
