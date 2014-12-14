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
  implicit def productOps[P <: Product](p: P): ProductOps[P] = new ProductOps[P](p)
}

final class ProductOps[P](p: P) {
  import ops.product._

  /**
   * Returns an `HList` containing the elements of this product.
   */
  def toHList[L <: HList](implicit gen: IsGeneric.Aux[P, L]): L = gen.to(p)

  /**
   * Returns an record containing the elements of this labelled product.
   */
  def toRecord[R <: HList](implicit gen: IsLabelledGeneric.Aux[P, R]): R = gen.to(p)

  /**
   * Compute the length of this product.
   */
  def length(implicit length : ProductLength[P]) : length.Out = length(p)
}
