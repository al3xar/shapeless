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
package syntax
package std

import scala.collection.GenTraversable

/**
 * Conversions between `Traversables` and `HLists` or generic products.
 * 
 * The implicit defined by this object enhances `Traversables` with methods which construct an equivalently
 * typed [[shapeless.HList]] or generic product if possible. 
 * 
 * @author Miles Sabin, Alexandre Archambault
 */
object traversable {
  implicit def traversableOps[T <% GenTraversable[_]](t : T): TraversableOps[T] = new TraversableOps(t)
}

final class TraversableOps[T <% GenTraversable[_]](t : T) {
  import ops.traversable._

  def toHList[L <: HList](implicit fromTraversable: FromTraversable[L]): Option[L] = fromTraversable(t)
  def toGeneric[P](implicit fromTraversable: FromTraversable[P]): Option[P] = fromTraversable(t)
}
