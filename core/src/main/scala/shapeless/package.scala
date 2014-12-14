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

import scala.language.experimental.macros

import scala.reflect.macros.whitebox

package object shapeless {
  // Basic definitions
  type Id[+T] = T
  type Const[C] = {
    type λ[T] = C
  }

  /** `Optic` definitions */
  val optic = OpticDefns
  val lens = OpticDefns
  val prism = OpticDefns
  val ^ = Path

  /** `Nat` literals */
  val nat = Nat

  /** 'Fin' */
  val fin = Fin

  /** `Poly` definitions */
  val poly = PolyDefns
  import poly._

  /** Dependent nullary function type. */
  trait DepFn0 {
    type Out
    def apply(): Out
  }

  /** Dependent unary function type. */
  trait DepFn1[T] {
    type Out
    def apply(t: T): Out
  }

  /** Dependent binary function type. */
  trait DepFn2[T, U] {
    type Out
    def apply(t: T, u: U): Out
  }

  /** The SYB everything combinator */
  type Everything[F <: Poly, K <: Poly, T] = Case1[EverythingAux[F, K], T]

  class ApplyEverything[F <: Poly] {
    def apply(k : Poly): EverythingAux[F, k.type] {} = new EverythingAux[F, k.type]
  }

  def everything(f: Poly): ApplyEverything[f.type] {} = new ApplyEverything[f.type]

  /** The SYB everywhere combinator */
  type Everywhere[F <: Poly, T] = Case1[EverywhereAux[F], T]

  def everywhere(f: Poly): EverywhereAux[f.type] {} = new EverywhereAux[f.type]

  type True = Witness.`true`.T
  type False = Witness.`false`.T
  val True: True = true
  val False: False = false

  def cachedImplicit[T]: T = macro CachedImplicitMacros.cachedImplicitImpl[T]

  implicit val witness0: Witness.Aux[_0] =
    new Witness {
      type T = _0
      val value = Nat._0
    }

  /**
   * Empty `HList` element type.
   */
  type HNil = HNilInstance.type
  val HNil = HNilInstance

  implicit def witnessN[P <: Nat]: Witness.Aux[Succ[P]] =
    new Witness {
      type T = Succ[P]
      val value = new Succ[P]()
    }
}

package shapeless {
  class CachedImplicitMacros(val c: whitebox.Context) {
    import c.universe._

    def dropLocal(nme: TermName): TermName = {
      val LOCAL_SUFFIX_STRING = " "
      val TermName(nmeString) = nme
      if(nmeString endsWith LOCAL_SUFFIX_STRING)
        TermName(nmeString.dropRight(LOCAL_SUFFIX_STRING.length))
      else
        nme
    }

    def cachedImplicitImpl[T](implicit tTag: WeakTypeTag[T]): Tree = {
      val tTpe = weakTypeOf[T]
      val owner = c.internal.enclosingOwner
      val ownerNme = dropLocal(owner.name.toTermName)
      val tpe = if(tTpe.typeSymbol.isParameter) owner.typeSignature else tTpe

      q"""
        {
          def $ownerNme = ???
          _root_.shapeless.the[$tpe]
        }
      """
    }
  }
}
