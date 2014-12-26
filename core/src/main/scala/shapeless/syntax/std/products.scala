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
package syntax
package std

object product {
  implicit def unitProductOps(u: Unit): ProductOps[Unit] = new ProductOps(u)
  implicit def productOps[P <: Product](p: P): ProductOps[P] = new ProductOps[P](p)
}

final class ProductOps[P](p: P) {
  import ops.product._

  /**
   * Returns an `HList` containing the elements of this tuple.
   */
  def productElements(implicit gen: Generic[P]): gen.Repr = gen.to(p)

  /**
   * Compute the length of this product.
   */
  def length(implicit length : Length[P]) : length.Out = length()

  /**
   * Returns a tuple containing the values of this product.
   */
  def toTuple[T](implicit toTuple: ToTuple.Aux[P, T]): T = toTuple(p)

  /**
   * Returns an `HList` containing the elements of this product.
   */
  def toHList[L <: HList](implicit toHList: ToHList.Aux[P, L]): L = toHList(p)

  /**
   * Returns a record containing the elements of this labelled product.
   */
  def toRecord[R <: HList](implicit toRecord: ToRecord.Aux[P, R]): R = toRecord(p)

  /**
   * Returns a collection `M` whose elements are typed as the Lub of the elements of this product.
   */
  def to[M[_]](implicit toTraversable: ToTraversable[P, M]): toTraversable.Out = toTraversable(p)

  /**
   * Converts this product to a `List` of elements typed as the least upper bound of the types of the elements
   * of this product.
   */
  def toList[Lub](implicit toTraversable : ToTraversable.Aux[P, List, Lub]) : toTraversable.Out = toTraversable(p)

  /**
   * Converts this product to an `Array` of elements typed as the least upper bound of the types of the elements
   * of this product.
   *
   * It is advisable to specify the type parameter explicitly, because for many reference types, case classes in
   * particular, the inferred type will be too precise (ie. `Product with Serializable with CC` for a typical case class
   * `CC`) which interacts badly with the invariance of `Array`s.
   */
  def toArray[Lub](implicit toTraversable : ToTraversable.Aux[P, Array, Lub]) : toTraversable.Out = toTraversable(p)

  /**
   * Returns a `Map` whose values are typed as the Lub of the values of this product.
   */
  def toMap[K, V](implicit toMap: ToMap.Aux[P, K, V]): Map[K, V] = toMap(p)

  /**
   * Returns a sized collection `M` whose elements are typed as the Lub of the elements of this product.
   */
  def toSized[M[_]](implicit toSized: ToSized[P, M]): toSized.Out = toSized(p)

  /**
   * Displays all elements of this tuple in a string using start, end, and separator strings.
   */
  def mkString(start: String, sep: String, end: String)
              (implicit toTraversable: ToTraversable.Aux[P, List, Any]): String =
    toList.mkString(start, sep, end)
}
