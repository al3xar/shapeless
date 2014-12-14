/*
 * Copyright (c) 2011-13 Miles Sabin 
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
import scala.annotation.tailrec

/**
 * `HList` ADT base trait.
 * 
 * @author Miles Sabin
 */
sealed trait HList

/**
 * Non-empty `HList` element type.
 * 
 * @author Miles Sabin
 */
final case class ::[+H, +T <: HList](head : H, tail : T) extends HList with Dynamic {
  override def toString = head+" :: "+tail.toString

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
case object HNilInstance extends HList {
  override def toString = "HNil"
}

object HList {
  import ops.hlist._
  import syntax.HListOps

  def apply() = HNil

  def apply[T](t: T) = t :: HNil
  
  def apply[P <: Product, L <: HList](p : P)(implicit gen: IsGeneric.Aux[P, L]) : L = gen.to(p)

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
}
