/*
 * Copyright (c) 2013 Miles Sabin
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
package ops

object tuple {
  import shapeless.ops.{ hlist => hl }

  /**
   * Type class witnessing that this tuple is composite and providing access to head and tail.
   *
   * @author Miles Sabin
   */
  trait IsComposite[P] {
    type H
    type T

    def head(p : P) : H
    def tail(p : P) : T
  }

  object IsComposite {
    def apply[P](implicit isComp: IsComposite[P]): Aux[P, isComp.H, isComp.T] = isComp

    type Aux[P, H0, T0] = IsComposite[P] { type H = H0; type T = T0 }

    implicit def isComposite[P, L <: HList, H0, T <: HList]
      (implicit gen: Generic.Aux[P, L], isHCons: hl.IsHCons.Aux[L, H0, T], tp: hl.Tupler[T]): Aux[P, H0, tp.Out] =
        new IsComposite[P] {
          type H = H0
          type T = tp.Out
          def head(p: P): H = isHCons.head(gen.to(p))
          def tail(p: P): T = tp(isHCons.tail(gen.to(p)))
        }
  }

  /**
   * Type class supporting prepending to this tuple.
   *
   * @author Miles Sabin
   */
  trait Prepend[T, U] extends DepFn2[T, U]

  object Prepend {
    def apply[T, U](implicit prepend: Prepend[T, U]): Aux[T, U, prepend.Out] = prepend

    type Aux[T, U, Out0] = Prepend[T, U] { type Out = Out0 }

    implicit def prepend[T, L1 <: HList, U, L2 <: HList, L3 <: HList]
      (implicit gent: Generic.Aux[T, L1], genu: Generic.Aux[U, L2], prepend: hl.Prepend.Aux[L1, L2, L3], tp: hl.Tupler[L3]): Aux[T, U, tp.Out] =
        new Prepend[T, U] {
          type Out = tp.Out
          def apply(t: T, u: U): Out = prepend(gent.to(t), genu.to(u)).tupled
        }
  }

  /**
   * Type class supporting reverse prepending to this tuple.
   *
   * @author Miles Sabin
   */
  trait ReversePrepend[T, U] extends DepFn2[T, U]

  object ReversePrepend {
    def apply[T, U](implicit prepend: ReversePrepend[T, U]): Aux[T, U, prepend.Out] = prepend

    type Aux[T, U, Out0] = ReversePrepend[T, U] { type Out = Out0 }

    implicit def prepend[T, L1 <: HList, U, L2 <: HList, L3 <: HList]
      (implicit gent: Generic.Aux[T, L1], genu: Generic.Aux[U, L2], prepend: hl.ReversePrepend.Aux[L1, L2, L3], tp: hl.Tupler[L3]): Aux[T, U, tp.Out] =
        new ReversePrepend[T, U] {
          type Out = tp.Out
          def apply(t: T, u: U): Out = prepend(gent.to(t), genu.to(u)).tupled
        }
  }

  /**
   * Type class supporting access to the ''nth'' element of this tuple. Available only if this tuple has at least
   * ''n'' elements.
   *
   * @author Miles Sabin
   */
  trait At[T, N <: Nat] extends DepFn1[T]

  object At {
    def apply[T, N <: Nat](implicit at: At[T, N]): Aux[T, N, at.Out] = at

    type Aux[T, N <: Nat, Out0] = At[T, N] { type Out = Out0 }

    implicit def at[T, L1 <: HList, N <: Nat]
      (implicit gen: Generic.Aux[T, L1], at: hl.At[L1, N]): Aux[T, N, at.Out] =
        new At[T, N] {
          type Out = at.Out
          def apply(t: T): Out = at(gen.to(t))
        }
  }

  /**
   * Type class supporting access to the last element of this tuple. Available only if this tuple has at least one
   * element.
   *
   * @author Miles Sabin
   */
  trait Last[T] extends DepFn1[T]

  object Last {
    def apply[T](implicit last: Last[T]): Aux[T, last.Out] = last

    type Aux[T, Out0] = Last[T] { type Out = Out0 }

    implicit def last[T, L <: HList]
      (implicit gen: Generic.Aux[T, L], last: hl.Last[L]): Aux[T, last.Out] =
        new Last[T] {
          type Out = last.Out
          def apply(t: T): Out = gen.to(t).last
        }
  }

  /**
   * Type class supporting access to all but the last element of this tuple. Available only if this tuple has at
   * least one element.
   *
   * @author Miles Sabin
   */
  trait Init[T] extends DepFn1[T]

  object Init {
    def apply[T](implicit init: Init[T]): Aux[T, init.Out] = init

    type Aux[T, Out0] = Init[T] { type Out = Out0 }

    implicit def init[T, L1 <: HList, L2 <: HList]
      (implicit gen: Generic.Aux[T, L1], init: hl.Init.Aux[L1, L2], tp: hl.Tupler[L2]): Aux[T, tp.Out] =
        new Init[T] {
          type Out = tp.Out
          def apply(t: T): Out = init(gen.to(t)).tupled
        }
  }

  /**
   * Type class supporting access to the first element of this tuple of type `U`. Available only if this tuple
   * contains an element of type `U`.
   *
   * @author Miles Sabin
   */
  trait Selector[T, U] extends DepFn1[T] { type Out = U }

  object Selector {
    def apply[T, U](implicit selector: Selector[T, U]): Aux[T, U] = selector

    type Aux[T, U] = Selector[T, U]

    implicit def select[T, L <: HList, U]
      (implicit gen: Generic.Aux[T, L], selector: hl.Selector[L, U]): Aux[T, U] =
        new Selector[T, U] {
          def apply(t: T): U = gen.to(t).select[U]
        }
  }

  /**
   * Type class supporting filtering a tuple with a higher function returning Boolean literals
   * at the tuple elements.
   *
   * @author Alexandre Archambault
   */
  trait Filter[HF, T] extends DepFn1[T]

  object Filter {
    def apply[HF, T](implicit filter: Filter[HF, T]): Aux[HF, T, filter.Out] = filter

    type Aux[HF, T, Out0] = Filter[HF, T] { type Out = Out0 }

    implicit def filterTuple[HF, T, L1 <: HList, L2 <: HList]
      (implicit gen: Generic.Aux[T, L1], filter: hl.Filter.Aux[HF, L1, L2], tp: hl.Tupler[L2]): Aux[HF, T, tp.Out] = new Filter[HF, T] {
        type Out = tp.Out
        def apply(t: T): Out = tp(filter(gen.to(t)))
      }
  }

  /**
   * Type class supporting filtering a tuple with a higher function returning Boolean literals
   * at the tuple elements.
   *
   * @author Alexandre Archambault
   */
  trait FilterNot[HF, T] extends DepFn1[T]

  object FilterNot {
    def apply[HF, T](implicit filterNot: FilterNot[HF, T]): Aux[HF, T, filterNot.Out] = filterNot

    type Aux[HF, T, Out0] = FilterNot[HF, T] { type Out = Out0 }

    implicit def filterNotTuple[HF, T, L1 <: HList, L2 <: HList]
     (implicit gen: Generic.Aux[T, L1], filterNot: hl.FilterNot.Aux[HF, L1, L2], tp: hl.Tupler[L2]): Aux[HF, T, tp.Out] = new FilterNot[HF, T] {
       type Out = tp.Out
       def apply(t: T): Out = tp(filterNot(gen.to(t)))
     }
  }

  /**
   * Type class supporting access to the all elements of this tuple of type `U`.
   *
   * @author Miles Sabin
   */
  trait FilterType[T, U] extends DepFn1[T]

  object FilterType {
    def apply[T, U](implicit filter: FilterType[T, U]): Aux[T, U, filter.Out] = filter

    type Aux[T, U, Out0] = FilterType[T, U] { type Out = Out0 }

    implicit def filterTuple[T, L1 <: HList, U, L2 <: HList]
      (implicit gen: Generic.Aux[T, L1], filter: hl.FilterType.Aux[L1, U, L2], tp: hl.Tupler[L2]): Aux[T, U, tp.Out] = new FilterType[T, U] {
        type Out = tp.Out
        def apply(t: T): Out = tp(filter(gen.to(t)))
      }
  }

  /**
   * Type class supporting access to the all elements of this tuple of type different than `U`.
   *
   * @author Miles Sabin
   */
  trait FilterNotType[T, U] extends DepFn1[T]

  object FilterNotType {
    def apply[T, U](implicit filter: FilterNotType[T, U]): Aux[T, U, filter.Out] = filter

    type Aux[T, U, Out0] = FilterNotType[T, U] { type Out = Out0 }

    implicit def filterNotTuple[T, L1 <: HList, U, L2 <: HList]
      (implicit gen: Generic.Aux[T, L1], filterNot: hl.FilterNotType.Aux[L1, U, L2], tp: hl.Tupler[L2]): Aux[T, U, tp.Out] = new FilterNotType[T, U] {
        type Out = tp.Out
        def apply(t: T): Out = tp(filterNot(gen.to(t)))
      }
  }

  /**
   * Type class supporting removal of an element from this tuple. Available only if this tuple contains an
   * element of type `U`.
   *
   * @author Miles Sabin
   */
  trait Remove[T, U] extends DepFn1[T]

  object Remove {
    def apply[T, E](implicit remove: Remove[T, E]): Aux[T, E, remove.Out] = remove

    type Aux[T, U, Out0] = Remove[T, U] { type Out = Out0 }

    implicit def removeTuple[T, L1 <: HList, U, L2 <: HList]
      (implicit gen: Generic.Aux[T, L1], remove: hl.Remove.Aux[L1, U, (U, L2)], tp: hl.Tupler[L2]): Aux[T, U, (U, tp.Out)] = new Remove[T, U] {
        type Out = (U, tp.Out)
        def apply(t: T): Out = { val (u, rem) = remove(gen.to(t)) ; (u, tp(rem)) }
      }
  }

  /**
   * Type class supporting removal of a sublist from this tuple. Available only if this tuple contains a
   * sublist of type `SL`.
   *
   * The elements of `SL` do not have to be contiguous in this tuple.
   *
   * @author Miles Sabin
   */
  trait RemoveAll[T, S] extends DepFn1[T]

  object RemoveAll {
    def apply[T, S](implicit remove: RemoveAll[T, S]): Aux[T, S, remove.Out] = remove

    type Aux[T, S, Out0] = RemoveAll[T, S] { type Out = Out0 }

    implicit def removeAllTuple[T, ST, SL <: HList, L1 <: HList, L2 <: HList]
      (implicit gent: Generic.Aux[T, L1], gens: Generic.Aux[ST, SL],  removeAll: hl.RemoveAll.Aux[L1, SL, (SL, L2)], tp: hl.Tupler[L2]): Aux[T, ST, (ST, tp.Out)] =
        new RemoveAll[T, ST] {
          type Out = (ST, tp.Out)
          def apply(t: T): Out = { val (e, rem) = removeAll(gent.to(t)) ; (gens.from(e), tp(rem)) }
        }
  }

  /**
   * Type class supporting replacement of the first element of type U from this tuple with an element of type V.
   * Available only if this tuple contains an element of type `U`.
   *
   * @author Miles Sabin
   */
  trait Replacer[T, U, V] extends DepFn2[T, U]

  object Replacer {
    def apply[T, U, V](implicit replacer: Replacer[T, U, V]): Aux[T, U, V, replacer.Out] = replacer

    type Aux[T, U, V, Out0] = Replacer[T, U, V] { type Out = Out0 }

    implicit def replaceTuple[T, L1 <: HList, U, V, L2 <: HList]
      (implicit gen: Generic.Aux[T, L1], replace: hl.Replacer.Aux[L1, V, U, (V, L2)], tp: hl.Tupler[L2]): Aux[T, U, V, (V, tp.Out)] = new Replacer[T, U, V] {
        type Out = (V, tp.Out)
        def apply(t: T, u: U): Out = { val (v, rep) = replace(gen.to(t), u) ; (v, tp(rep)) }
      }
  }

  /**
   * Type class supporting replacement of the Nth element of this tuple with an element of type V. Available only if
   * this tuple contains at least N elements.
   *
   * @author Miles Sabin
   */
  trait ReplaceAt[T, N <: Nat, U] extends DepFn2[T, U]

  object ReplaceAt {
    def apply[T, N <: Nat, V](implicit replacer: ReplaceAt[T, N, V]): Aux[T, N, V, replacer.Out] = replacer

    type Aux[T, N <: Nat, U, Out0] = ReplaceAt[T, N, U] { type Out = Out0 }

    implicit def replaceTuple[T, L1 <: HList, N <: Nat, U, V, L2 <: HList]
      (implicit gen: Generic.Aux[T, L1], replaceAt: hl.ReplaceAt.Aux[L1, N, U, (V, L2)], tp: hl.Tupler[L2]): Aux[T, N, U, (V, tp.Out)] = new ReplaceAt[T, N, U] {
        type Out = (V, tp.Out)
        def apply(t: T, u: U): Out = { val (v, rep) = replaceAt(gen.to(t), u) ; (v, tp(rep)) }
      }
  }

  /**
   * Type class supporting retrieval of the first ''n'' elements of this tuple. Available only if this tuple has at
   * least ''n'' elements.
   *
   * @author Miles Sabin
   */
  trait Take[T, N <: Nat] extends DepFn1[T]

  object Take {
    def apply[T, N <: Nat](implicit take: Take[T, N]): Aux[T, N, take.Out] = take

    type Aux[T, N <: Nat, Out0] = Take[T, N] { type Out = Out0 }

    implicit def tupleTake[T, L1 <: HList, N <: Nat, L2 <: HList]
      (implicit gen: Generic.Aux[T, L1], take: hl.Take.Aux[L1, N, L2], tp: hl.Tupler[L2]): Aux[T, N, tp.Out] =
        new Take[T, N] {
          type Out = tp.Out
          def apply(t: T): tp.Out = tp(take(gen.to(t)))
        }
  }

  /**
   * Type class supporting removal of the first ''n'' elements of this tuple. Available only if this tuple has at
   * least ''n'' elements.
   *
   * @author Miles Sabin
   */
  trait Drop[T, N <: Nat] extends DepFn1[T]

  object Drop {
    def apply[T, N <: Nat](implicit drop: Drop[T, N]): Aux[T, N, drop.Out] = drop

    type Aux[T, N <: Nat, Out0] = Drop[T, N] { type Out = Out0 }

    implicit def tupleDrop[T, L1 <: HList, N <: Nat, L2 <: HList]
      (implicit gen: Generic.Aux[T, L1], drop: hl.Drop.Aux[L1, N, L2], tp: hl.Tupler[L2]): Aux[T, N, tp.Out] =
        new Drop[T, N] {
          type Out = tp.Out
          def apply(t: T): tp.Out = tp(drop(gen.to(t)))
        }
  }

  /**
   * Type class supporting splitting this tuple at the ''nth'' element returning the prefix and suffix as a pair.
   * Available only if this tuple has at least ''n'' elements.
   *
   * @author Miles Sabin
   */
  trait Split[T, N <: Nat] extends DepFn1[T]

  object Split {
    def apply[T, N <: Nat](implicit split: Split[T, N]): Aux[T, N, split.Out] = split

    type Aux[T, N <: Nat, Out0] = Split[T, N] { type Out = Out0 }

    implicit def tupleSplit[T, L <: HList, N <: Nat, LP <: HList, LS <: HList]
      (implicit
        gen: Generic.Aux[T, L],
        split: hl.Split.Aux[L, N, LP, LS],
        tpp: hl.Tupler[LP],
        tps: hl.Tupler[LS]
      ): Aux[T, N, (tpp.Out, tps.Out)] =
        new Split[T, N] {
          type Out = (tpp.Out, tps.Out)
          def apply(t: T): Out = { val p :: s :: HNil = split.product(gen.to(t)) ; (tpp(p), tps(s)) }
        }
  }

  /**
   * Type class supporting splitting this tuple at the ''nth'' element returning the reverse prefix and suffix as a
   * pair. Available only if this tuple has at least ''n'' elements.
   *
   * @author Miles Sabin
   */
  trait ReverseSplit[T, N <: Nat] extends DepFn1[T]

  object ReverseSplit {
    def apply[T, N <: Nat](implicit split: ReverseSplit[T, N]): Aux[T, N, split.Out] = split

    type Aux[T, N <: Nat, Out0] = ReverseSplit[T, N] { type Out = Out0 }

    implicit def tupleReverseSplit[T, L <: HList, N <: Nat, LP <: HList, LS <: HList]
      (implicit
        gen: Generic.Aux[T, L],
        split: hl.ReverseSplit.Aux[L, N, LP, LS],
        tpp: hl.Tupler[LP],
        tps: hl.Tupler[LS]
      ): Aux[T, N, (tpp.Out, tps.Out)] =
        new ReverseSplit[T, N] {
          type Out = (tpp.Out, tps.Out)
          def apply(t: T): Out = { val p :: s :: HNil = split.product(gen.to(t)) ; (tpp(p), tps(s)) }
        }
  }

  /**
   * Type class supporting splitting this tuple at the first occurence of an element of type `U` returning the prefix
   * and suffix as a pair. Available only if this tuple contains an element of type `U`.
   *
   * @author Miles Sabin
   */
  trait SplitLeft[T, U] extends DepFn1[T]

  object SplitLeft {
    def apply[T, U](implicit split: SplitLeft[T, U]): Aux[T, U, split.Out] = split

    type Aux[T, U, Out0] = SplitLeft[T, U] { type Out = Out0 }

    implicit def tupleSplitLeft[T, L <: HList, U, LP <: HList, LS <: HList]
      (implicit
        gen: Generic.Aux[T, L],
        split: hl.SplitLeft.Aux[L, U, LP, LS],
        tpp: hl.Tupler[LP],
        tps: hl.Tupler[LS]
      ): Aux[T, U, (tpp.Out, tps.Out)] =
        new SplitLeft[T, U] {
          type Out = (tpp.Out, tps.Out)
          def apply(t: T): Out = { val p :: s :: HNil = split.product(gen.to(t)) ; (tpp(p), tps(s)) }
        }
  }

  /**
   * Type class supporting splitting this tuple at the first occurence of an element of type `U` returning the reverse
   * prefix and suffix as a pair. Available only if this tuple contains an element of type `U`.
   *
   * @author Miles Sabin
   */
  trait ReverseSplitLeft[T, U] extends DepFn1[T]

  object ReverseSplitLeft {
    def apply[T, U](implicit split: ReverseSplitLeft[T, U]): Aux[T, U, split.Out] = split

    type Aux[T, U, Out0] = ReverseSplitLeft[T, U] { type Out = Out0 }

    implicit def tupleReverseSplitLeft[T, L <: HList, U, LP <: HList, LS <: HList]
      (implicit
        gen: Generic.Aux[T, L],
        split: hl.ReverseSplitLeft.Aux[L, U, LP, LS],
        tpp: hl.Tupler[LP],
        tps: hl.Tupler[LS]
      ): Aux[T, U, (tpp.Out, tps.Out)] =
        new ReverseSplitLeft[T, U] {
          type Out = (tpp.Out, tps.Out)
          def apply(t: T): Out = { val p :: s :: HNil = split.product(gen.to(t)) ; (tpp(p), tps(s)) }
        }
  }

  /**
   * Type class supporting splitting this tuple at the last occurence of an element of type `U` returning the prefix
   * and suffix as a pair. Available only if this tuple contains an element of type `U`.
   *
   * @author Miles Sabin
   */
  trait SplitRight[T, U] extends DepFn1[T]

  object SplitRight {
    def apply[T, U](implicit split: SplitRight[T, U]): Aux[T, U, split.Out] = split

    type Aux[T, U, Out0] = SplitRight[T, U] { type Out = Out0 }

    implicit def tupleSplitRight[T, L <: HList, U, LP <: HList, LS <: HList]
      (implicit
        gen: Generic.Aux[T, L],
        split: hl.SplitRight.Aux[L, U, LP, LS],
        tpp: hl.Tupler[LP],
        tps: hl.Tupler[LS]
      ): Aux[T, U, (tpp.Out, tps.Out)] =
        new SplitRight[T, U] {
          type Out = (tpp.Out, tps.Out)
          def apply(t: T): Out = { val p :: s :: HNil = split.product(gen.to(t)) ; (tpp(p), tps(s)) }
        }
  }

  /**
   * Type class supporting splitting this tuple at the last occurence of an element of type `U` returning the reverse
   * prefix and suffix as a pair. Available only if this tuple contains an element of type `U`.
   *
   * @author Miles Sabin
   */
  trait ReverseSplitRight[T, U] extends DepFn1[T]

  object ReverseSplitRight {
    def apply[T, U](implicit split: ReverseSplitRight[T, U]): Aux[T, U, split.Out] = split

    type Aux[T, U, Out0] = ReverseSplitRight[T, U] { type Out = Out0 }

    implicit def tupleReverseSplitRight[T, L <: HList, U, LP <: HList, LS <: HList]
      (implicit
        gen: Generic.Aux[T, L],
        split: hl.ReverseSplitRight.Aux[L, U, LP, LS],
        tpp: hl.Tupler[LP],
        tps: hl.Tupler[LS]
      ): Aux[T, U, (tpp.Out, tps.Out)] =
        new ReverseSplitRight[T, U] {
          type Out = (tpp.Out, tps.Out)
          def apply(t: T): Out = { val p :: s :: HNil = split.product(gen.to(t)) ; (tpp(p), tps(s)) }
        }
  }

  /**
   * Type class supporting reversing this tuple.
   *
   * @author Miles Sabin
   */
  trait Reverse[T] extends DepFn1[T]

  object Reverse {
    def apply[T](implicit reverse: Reverse[T]): Aux[T, reverse.Out] = reverse

    type Aux[T, Out0] = Reverse[T] { type Out = Out0 }

    implicit def tupleReverseAux[T, L1 <: HList, L2 <: HList, Out]
      (implicit gen: Generic.Aux[T, L1], reverse: hl.Reverse.Aux[L1, L2], tp: hl.Tupler[L2]): Aux[T, tp.Out] =
        new Reverse[T] {
          type Out = tp.Out
          def apply(t: T): tp.Out = tp(reverse(gen.to(t)))
        }
  }

  /**
   * Type class supporting mapping a higher ranked function over this tuple.
   *
   * @author Miles Sabin
   */
  trait Mapper[T, P] extends DepFn1[T]

  object Mapper {
    def apply[T, P](implicit mapper: Mapper[T, P]): Aux[T, P, mapper.Out] = mapper

    type Aux[T, P, Out0] = Mapper[T, P] { type Out = Out0 }

    implicit def mapper[T, P, L1 <: HList, L2 <: HList]
      (implicit gen: Generic.Aux[T, L1], mapper: hl.Mapper.Aux[P, L1, L2], tp: hl.Tupler[L2]): Aux[T, P, tp.Out] =
        new Mapper[T, P] {
          type Out = tp.Out
          def apply(t: T): tp.Out = tp(mapper(gen.to(t)))
        }
  }

  /**
   * Type class supporting flatmapping a higher ranked function over this tuple.
   *
   * @author Miles Sabin
   */
  trait FlatMapper[T, P] extends DepFn1[T]

  object FlatMapper {
    def apply[T, P](implicit mapper: FlatMapper[T, P]): Aux[T, P, mapper.Out] = mapper

    import poly.Compose

    type Aux[T, P, Out0] = FlatMapper[T, P] { type Out = Out0 }

    implicit def mapper[T, P, L1 <: HList, L2 <: HList]
      (implicit gen: Generic.Aux[T, L1], mapper: hl.FlatMapper.Aux[Compose[productElements.type, P], L1, L2], tp: hl.Tupler[L2]): Aux[T, P, tp.Out] =
        new FlatMapper[T, P] {
          type Out = tp.Out
          def apply(t: T): tp.Out = tp(mapper(gen.to(t)))
        }
  }

  /**
   * Type class supporting mapping a constant valued function over this tuple.
   *
   * @author Miles Sabin
   */
  trait ConstMapper[T, C] extends DepFn2[T, C]

  object ConstMapper {
    def apply[T, C](implicit mapper: ConstMapper[T, C]): Aux[T, C, mapper.Out] = mapper

    type Aux[T, C, Out0] = ConstMapper[T, C] { type Out = Out0 }

    implicit def mapper[T, C, L1 <: HList, L2 <: HList]
      (implicit gen: Generic.Aux[T, L1], mapper: hl.ConstMapper.Aux[C, L1, L2], tp: hl.Tupler[L2]): Aux[T, C, tp.Out] =
        new ConstMapper[T, C] {
          type Out = tp.Out
          def apply(t: T, c: C): tp.Out = tp(mapper(c, gen.to(t)))
        }
  }

  /**
   * Type class supporting mapping a polymorphic function over this tuple and then folding the result using a
   * monomorphic function value.
   *
   * @author Miles Sabin
   */
  trait MapFolder[T, R, P] { // Nb. Not a dependent function signature
    def apply(t: T, in: R, op: (R, R) => R): R
  }

  object MapFolder {
    def apply[T, R, P](implicit folder: MapFolder[T, R, P]) = folder

    implicit def mapper[T, L <: HList, R, P]
      (implicit gen: Generic.Aux[T, L], mapper: hl.MapFolder[L, R, P]): MapFolder[T, R, P] =
        new MapFolder[T, R, P] {
          def apply(t: T, in: R, op: (R, R) => R): R = mapper(gen.to(t), in, op)
        }
  }

  /**
   * Type class supporting left-folding a polymorphic binary function over this tuple.
   *
   * @author Miles Sabin
   */
  trait LeftFolder[T, U, P] extends DepFn2[T, U]

  object LeftFolder {
    def apply[T, U, P](implicit folder: LeftFolder[T, U, P]): Aux[T, U, P, folder.Out] = folder

    type Aux[T, U, P, Out0] = LeftFolder[T, U, P] { type Out = Out0 }

    implicit def folder[T, L <: HList, U, P]
      (implicit gen: Generic.Aux[T, L], folder: hl.LeftFolder[L, U, P]): Aux[T, U, P, folder.Out] =
        new LeftFolder[T, U, P] {
          type Out = folder.Out
          def apply(t: T, u: U): Out = folder(gen.to(t), u)
        }
  }

  /**
   * Type class supporting right-folding a polymorphic binary function over this tuple.
   *
   * @author Miles Sabin
   */
  trait RightFolder[T, U, P] extends DepFn2[T, U]

  object RightFolder {
    def apply[T, U, P](implicit folder: RightFolder[T, U, P]): Aux[T, U, P, folder.Out] = folder

    type Aux[T, U, P, Out0] = RightFolder[T, U, P] { type Out = Out0 }

    implicit def folder[T, L <: HList, U, P]
      (implicit gen: Generic.Aux[T, L], folder: hl.RightFolder[L, U, P]): Aux[T, U, P, folder.Out] =
        new RightFolder[T, U, P] {
          type Out = folder.Out
          def apply(t: T, u: U): Out = folder(gen.to(t), u)
        }
  }

  /**
   * Type class supporting left-reducing a polymorphic binary function over this tuple.
   *
   * @author Miles Sabin
   */
  trait LeftReducer[T, P] extends DepFn1[T]

  object LeftReducer {
    def apply[T, P](implicit reducer: LeftReducer[T, P]): Aux[T, P, reducer.Out] = reducer

    type Aux[T, P, Out0] = LeftReducer[T, P] { type Out = Out0 }

    implicit def folder[T, L <: HList, P]
      (implicit gen: Generic.Aux[T, L], folder: hl.LeftReducer[L, P]): Aux[T, P, folder.Out] =
        new LeftReducer[T, P] {
          type Out = folder.Out
          def apply(t: T): Out = folder(gen.to(t))
        }
  }

  /**
   * Type class supporting right-reducing a polymorphic binary function over this tuple.
   *
   * @author Miles Sabin
   */
  trait RightReducer[T, P] extends DepFn1[T]

  object RightReducer {
    def apply[T, P](implicit reducer: RightReducer[T, P]): Aux[T, P, reducer.Out] = reducer

    type Aux[T, P, Out0] = RightReducer[T, P] { type Out = Out0 }

    implicit def folder[T, L <: HList, P]
      (implicit gen: Generic.Aux[T, L], folder: hl.RightReducer[L, P]): Aux[T, P, folder.Out] =
        new RightReducer[T, P] {
          type Out = folder.Out
          def apply(t: T): Out = folder(gen.to(t))
        }
  }

  /**
   * Type class supporting transposing this tuple.
   *
   * @author Miles Sabin
   */
  trait Transposer[T] extends DepFn1[T]

  object Transposer {
    def apply[T](implicit transposer: Transposer[T]): Aux[T, transposer.Out] = transposer

    type Aux[T, Out0] = Transposer[T] { type Out = Out0 }

    implicit def transpose[T, L1 <: HList, L2 <: HList, L3 <: HList, L4 <: HList]
      (implicit
        gen: Generic.Aux[T, L1],
        mpe: hl.Mapper.Aux[productElements.type, L1, L2],
        tps: hl.Transposer.Aux[L2, L3],
        mtp: hl.Mapper.Aux[tupled.type, L3, L4],
        tp:  hl.Tupler[L4]
      ): Aux[T, tp.Out] =
      new Transposer[T] {
        type Out = tp.Out
        def apply(t: T): Out = ((gen.to(t) map productElements).transpose map tupled).tupled
      }
  }

  /**
   * Type class supporting zipping this this tuple of monomorphic function values with its argument tuple of
   * correspondingly typed function arguments returning the result of each application as a tuple. Available only if
   * there is evidence that the corresponding function and argument elements have compatible types.
   *
   * @author Miles Sabin
   */
  trait ZipApply[FT, AT] extends DepFn2[FT, AT]

  object ZipApply {
    def apply[FT, AT](implicit zip: ZipApply[FT, AT]): Aux[FT, AT, zip.Out] = zip

    type Aux[FT, AT, Out0] = ZipApply[FT, AT] { type Out = Out0 }

    implicit def zipApply[FT, FL <: HList, AT, AL <: HList, RL <: HList]
      (implicit
        genf: Generic.Aux[FT, FL],
        gena: Generic.Aux[AT, AL],
        zip:  hl.ZipApply.Aux[FL, AL, RL],
        tp:   hl.Tupler[RL]
      ): Aux[FT, AT, tp.Out] =
      new ZipApply[FT, AT] {
        type Out = tp.Out
        def apply(ft: FT, at: AT): Out = (genf.to(ft) zipApply gena.to(at)).tupled
      }
  }

  /**
   * Type class supporting zipping this tuple with a tuple of tuples returning a tuple of tuples with each
   * element of this tuple prepended to the corresponding tuple element of the argument tuple.
   *
   * @author Miles Sabin
   */
  trait ZipOne[H, T] extends DepFn2[H, T]

  object ZipOne {
    def apply[H, T](implicit zip: ZipOne[H, T]): Aux[H, T, zip.Out] = zip

    type Aux[H, T, Out0] = ZipOne[H, T] { type Out = Out0 }

    implicit def zipOne[HT, HL <: HList, TT, TL <: HList, TLL <: HList, RLL <: HList, RL <: HList]
      (implicit
        genh: Generic.Aux[HT, HL],
        gent: Generic.Aux[TT, TL],
        mpet: hl.Mapper.Aux[productElements.type, TL, TLL],
        zone: hl.ZipOne.Aux[HL, TLL, RLL],
        mtp:  hl.Mapper.Aux[tupled.type, RLL, RL],
        tp:   hl.Tupler[RL]
      ): Aux[HT, TT, tp.Out] =
      new ZipOne[HT, TT] {
        type Out = tp.Out
        def apply(h: HT, t: TT): Out = ((genh.to(h) zipOne (gent.to(t) map productElements)) map tupled).tupled
      }
  }

  /**
   * Type class supporting zipping a tuple with a constant, resulting in a tuple of tuples of the form
   * ({element from input tuple}, {supplied constant})
   *
   * @author Miles Sabin
   */
  trait ZipConst[T, C] extends DepFn2[T, C]

  object ZipConst {
    def apply[T, C](implicit zip: ZipConst[T, C]): Aux[T, C, zip.Out] = zip

    type Aux[T, C, Out0] = ZipConst[T, C] { type Out = Out0 }

    implicit def zipConst[T, C, L1 <: HList, L2 <: HList]
    (implicit gen: Generic.Aux[T, L1], zipper: hl.ZipConst.Aux[C, L1, L2], tp: hl.Tupler[L2]): Aux[T, C, tp.Out] =
      new ZipConst[T, C] {
        type Out = tp.Out
        def apply(t: T, c: C): tp.Out = tp(zipper(c, gen.to(t)))
      }
  }

  /**
   * Type class supporting unification of this tuple.
   *
   * @author Miles Sabin
   */
  trait Unifier[T] extends DepFn1[T]

  object Unifier {
    def apply[T](implicit unifier: Unifier[T]): Aux[T, unifier.Out] = unifier

    type Aux[T, Out0] = Unifier[T] { type Out = Out0 }

    implicit def unifier[T, L1 <: HList, L2 <: HList]
      (implicit gen: Generic.Aux[T, L1], unifier: hl.Unifier.Aux[L1, L2], tp: hl.Tupler[L2]): Aux[T, tp.Out] =
        new Unifier[T] {
          type Out = tp.Out
          def apply(t: T): Out = unifier(gen.to(t)).tupled
        }
  }

  /**
   * Type class supporting unification of all elements that are subtypes of `B` in this tuple to `B`, with all other
   * elements left unchanged.
   *
   * @author Miles Sabin
   */
  trait SubtypeUnifier[T, B] extends DepFn1[T]

  object SubtypeUnifier {
    def apply[T, B](implicit unifier: SubtypeUnifier[T, B]): Aux[T, B, unifier.Out] = unifier

    type Aux[T, B, Out0] = SubtypeUnifier[T, B] { type Out = Out0 }

    implicit def subtypeUnifier[T, B, L1 <: HList, L2 <: HList]
      (implicit gen: Generic.Aux[T, L1], unifier: hl.SubtypeUnifier.Aux[L1, B, L2], tp: hl.Tupler[L2]): Aux[T, B, tp.Out] =
        new SubtypeUnifier[T, B] {
          type Out = tp.Out
          def apply(t: T): Out = unifier(gen.to(t)).tupled
        }
  }

  /**
   * Type class supporting computing the type-level Nat corresponding to the length of this tuple.
   *
   * @author Miles Sabin
   */
  trait Length[T] extends DepFn1[T]

  object Length {
    def apply[T](implicit length: Length[T]): Aux[T, length.Out] = length

    type Aux[T, Out0] = Length[T] { type Out = Out0 }

    implicit def length[T, L <: HList]
      (implicit gen: Generic.Aux[T, L], length: hl.Length[L]): Aux[T, length.Out] =
        new Length[T] {
          type Out = length.Out
          def apply(t: T): Out = length()
        }
  }

  /**
   * Type class supporting conversion of this tuple to a `M` with elements typed as the least upper bound
   * of the types of the elements of this tuple.
   *
   * @author Alexandre Archambault
   */
  trait ToTraversable[T, M[_]] extends DepFn1[T] {
    type Lub
    type Out = M[Lub]
  }

  object ToTraversable {
    def apply[T, M[_]]
      (implicit toTraversable: ToTraversable[T, M]): Aux[T, M, toTraversable.Lub] = toTraversable

    type Aux[T, M[_], Lub0] = ToTraversable[T, M] { type Lub = Lub0 }

    implicit def toTraversable[T, L <: HList, M[_], Lub]
      (implicit gen: Generic.Aux[T, L], toTraversable: hl.ToTraversable.Aux[L, M, Lub]): Aux[T, M, Lub] =
        new ToTraversable[T, M] {
          type Lub = toTraversable.Lub
          def apply(t: T) = gen.to(t).to[M]
        }
  }

  /**
   * Type class supporting conversion of this tuple to a `List` with elements typed as the least upper bound
   * of the types of the elements of this tuple.
   *
   * Provided for backward compatibility.
   *
   * @author Miles Sabin
   */
  trait ToList[T, Lub] extends DepFn1[T]

  object ToList {
    type Aux[T, Lub, Out0] = ToList[T, Lub] { type Out = Out0 }

    implicit def toList[T, L <: HList, Lub]
      (implicit toTraversable: ToTraversable.Aux[T, List, Lub]): Aux[T, Lub, List[Lub]] =
        new ToList[T, Lub] {
          type Out = List[Lub]
          def apply(t: T) = toTraversable(t)
        }
  }

  /**
   * Type class supporting conversion of this tuple to an `Array` with elements typed as the least upper bound
   * of the types of the elements of this tuple.
   *
   * Provided for backward compatibility.
   *
   * @author Miles Sabin
   */
  trait ToArray[T, Lub] extends DepFn1[T]

  object ToArray {
    type Aux[T, Lub, Out0] = ToArray[T, Lub] { type Out = Out0 }

    implicit def toArray[T, L <: HList, Lub]
      (implicit toTraversable: ToTraversable.Aux[T, Array, Lub]): Aux[T, Lub, Array[Lub]] =
        new ToArray[T, Lub] {
          type Out = Array[Lub]
          def apply(t: T) = toTraversable(t)
        }
  }

  /**
   * Type class supporting conversion of this tuple to a `Sized[M[Lub], N]` with elements typed as
   * the least upper bound Lub of the types of the elements of this tuple.
   *
   * @author Alexandre Archambault
   */
  trait ToSized[T, M[_]] extends DepFn1[T]

  object ToSized {
    def apply[T, M[_]](implicit toSized: ToSized[T, M]): Aux[T, M, toSized.Out] = toSized

    type Aux[T, M[_], Out0] = ToSized[T, M] { type Out = Out0 }

    implicit def toSized[T, L <: HList, M[_]]
      (implicit gen: Generic.Aux[T, L], toSized: hl.ToSized[L, M]): Aux[T, M, toSized.Out] =
        new ToSized[T, M] {
          type Out = toSized.Out
          def apply(t: T) = gen.to(t).toSized[M]
        }
  }

  /**
   * Type Class witnessing that this tuple can be collected with a 'Poly' to produce a new tuple
   *
   * @author Stacy Curl
   */
  trait Collect[T, P <: Poly] extends DepFn1[T]

  object Collect {
    def apply[T, P <: Poly](implicit collect: Collect[T, P]): Aux[T, P, collect.Out] = collect

    type Aux[T, P <: Poly, Out0] = Collect[T, P] { type Out = Out0 }

    implicit def collect[T, L <: HList, L2 <: HList, P <: Poly]
      (implicit gen: Generic.Aux[T, L], collect: hl.Collect.Aux[L, P, L2], tp: hl.Tupler[L2])
        : Aux[T, P, tp.Out] = new Collect[T, P] {
          type Out = tp.Out

          def apply(t: T): tp.Out = tp(collect(gen.to(t)))
        }
  }

  /**
   * Typer class supporting the calculation of every permutation of this tuple
   *
   * @author Stacy Curl
   */
  trait Permutations[T] extends DepFn1[T]

  object Permutations {
    def apply[T](implicit permutations: Permutations[T]): Aux[T, permutations.Out] = permutations

    type Aux[T, Out0] = Permutations[T] { type Out = Out0 }

    implicit def permutations[T, L <: HList, L2 <: HList, L3 <: HList]
      (implicit gen: Generic.Aux[T, L], collect: hl.Permutations.Aux[L, L2],
        mapper: hl.Mapper.Aux[tupled.type, L2, L3], tp: hl.Tupler[L3]
      ): Aux[T, tp.Out] = new Permutations[T] {
        type Out = tp.Out

        def apply(t: T): Out = tp(collect(gen.to(t)).map(tupled))
      }
  }

  /**
   * Type class supporting rotating a tuple left
   *
   * @author Stacy Curl
   */
  trait RotateLeft[T, N <: Nat] extends DepFn1[T]

  object RotateLeft {
    def apply[T, N <: Nat](implicit rotateLeft: RotateLeft[T, N]): Aux[T, N, rotateLeft.Out] = rotateLeft

    type Aux[T, N <: Nat, Out0] = RotateLeft[T, N] { type Out = Out0 }

    implicit def tupleRotateLeft[T, N <: Nat, L <: HList, L2 <: HList]
      (implicit gen: Generic.Aux[T, L], rotateLeft: hl.RotateLeft.Aux[L, N, L2], tp: hl.Tupler[L2])
        : Aux[T, N, tp.Out] = new RotateLeft[T, N] {
          type Out = tp.Out

          def apply(t: T): Out = tp(rotateLeft(gen.to(t)))
        }
  }

  /**
   * Type class supporting rotating a tuple right
   *
   * @author Stacy Curl
   */
  trait RotateRight[T, N <: Nat] extends DepFn1[T]

  object RotateRight {
    def apply[T, N <: Nat](implicit rotateRight: RotateRight[T, N]): Aux[T, N, rotateRight.Out] = rotateRight

    type Aux[T, N <: Nat, Out0] = RotateRight[T, N] { type Out = Out0 }

    implicit def tupleRotateRight[T, N <: Nat, L <: HList, L2 <: HList]
      (implicit gen: Generic.Aux[T, L], rotateRight: hl.RotateRight.Aux[L, N, L2], tp: hl.Tupler[L2])
        : Aux[T, N, tp.Out] = new RotateRight[T, N] {
          type Out = tp.Out

          def apply(t: T): Out = tp(rotateRight(gen.to(t)))
        }
  }

  /**
   * Type class supporting left-scanning a binary polymorphic function over this tuple.
   *
   * @author Owein Reese
   */
  trait LeftScanner[T, In, P <: Poly] extends DepFn2[T, In]

  object LeftScanner{
    def apply[T, In, P <: Poly](implicit scanL: LeftScanner[T, In, P]): Aux[T, In, P, scanL.Out] = scanL

    type Aux[T, In, P <: Poly, Out0] = LeftScanner[T, In, P] { type Out = Out0 }

    implicit def scanner[T, L <: HList, In, P <: Poly, R <: HList]
      (implicit gen: Generic.Aux[T, L],
        scanL: hl.LeftScanner.Aux[L, In, P, R],
        tp: hl.Tupler[R]
      ): Aux[T, In, P, tp.Out] =
        new LeftScanner[T, In, P] {
          type Out = tp.Out

          def apply(t: T, in: In): Out = tp(scanL(gen.to(t), in))
        }
  }

  /**
   * Type class supporting right-scanning a binary polymorphic function over this tuple.
   *
   * @author Owein Reese
   */
  trait RightScanner[T, In, P <: Poly] extends DepFn2[T, In]

  object RightScanner{
    def apply[T, In, P <: Poly](implicit scanR: RightScanner[T, In, P]): Aux[T, In, P, scanR.Out] = scanR

    type Aux[T, In, P <: Poly, Out0] = RightScanner[T, In, P] { type Out = Out0 }

    implicit def scanner[T, L <: HList, In, P <: Poly, R <: HList]
      (implicit gen: Generic.Aux[T, L],
        scanR: hl.RightScanner.Aux[L, In, P, R],
        tp: hl.Tupler[R]
      ): Aux[T, In, P, tp.Out] =
        new RightScanner[T, In, P] {
          type Out = tp.Out

          def apply(t: T, in: In): Out = tp(scanR(gen.to(t), in))
        }
  }

  /**
   * Type class supporting producing a tuple of shape `N` filled with elements of type `A`.
   *
   * @author Alexandre Archambault
   */
  trait Fill[N, A] extends DepFn1[A]

  object Fill {
    def apply[N, A](implicit fill: Fill[N, A]) = fill

    type Aux[N, A, Out0] = Fill[N, A] { type Out = Out0 }

    implicit def fill1[N <: Nat, A, L <: HList, P]
      (implicit fill: hlist.Fill.Aux[N, A, L], tupler: hlist.Tupler[L]): Aux[N, A, tupler.Out] =
        new Fill[N, A] {
          type Out = tupler.Out
          def apply(elem: A) = tupler(fill(elem))
        }

    implicit def fill2[A, N1 <: Nat, N2 <: Nat, SubOut]
      (implicit subFill: Fill.Aux[N2, A, SubOut], fill: Fill[N1, SubOut]): Aux[(N1, N2), A, fill.Out] =
        new Fill[(N1, N2), A] {
          type Out = fill.Out
          def apply(elem: A) = fill(subFill(elem))
        }

  }

  /**
   * Type class supporting the patching of a tuple.
   *
   * @author Owein Reese
   */
  trait Patcher[N <: Nat, M <: Nat, T, InT] extends DepFn2[T, InT]

  object Patcher{
    def apply[N <: Nat, M <: Nat, T, InT](implicit patch: Patcher[N, M, T, InT]) = patch

    implicit def tuplePatch[N <: Nat, M <: Nat, T, L <: HList, InT, InL <: HList, OutL <: HList]
      (implicit gen: Generic.Aux[T, L],
        genIn: Generic.Aux[InT, InL],
        patch: hl.Patcher.Aux[N, M, L, InL, OutL],
        tp: hl.Tupler[OutL]) =
        new Patcher[N, M, T, InT]{
          type Out = tp.Out

          def apply(t: T, in: InT) = tp(patch(gen.to(t), genIn.to(in)))
        }
  }
}
