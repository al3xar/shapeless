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
package ops

import scala.collection.GenTraversable

object traversable {
  /**
   * Type class supporting type safe conversion of `Traversables` to `HLists` or generic products. 
   * 
   * @author Miles Sabin, Alexandre Archambault
   */
  trait FromTraversable[T] {
    def apply(l: GenTraversable[Any]): Option[T]
  }

  trait LowPriorityFromTraversable {
    implicit def projectGenericFromTraversable[F, G](implicit
      gen: IsGeneric.Aux[F, G],
      fromTraversable: FromTraversable[G]
    ): FromTraversable[F] =
      new FromTraversable[F] {
        def apply(l: GenTraversable[Any]) = fromTraversable(l).map(gen.from)
      }
  }

  object FromTraversable extends LowPriorityFromTraversable {
    implicit val hnilFromTraversable: FromTraversable[HNil] =
      new FromTraversable[HNil] {
        def apply(l: GenTraversable[Any]) =
          if (l.isEmpty)
            Some(HNil)
          else
            None
      }

    implicit def hconsFromTraversable[H, T <: HList](implicit
      typeable: Typeable[H],
      tailFromTraversable: FromTraversable[T]
    ): FromTraversable[H :: T] =
      new FromTraversable[H :: T] {
        def apply(l: GenTraversable[Any]) =
          if (l.isEmpty)
            None
          else
            for {
              h <- typeable.cast(l.head)
              t <- tailFromTraversable(l.tail)
            } yield h :: t
      }
  }
}
