/*
 * Copyright (c) 2014 Miles Sabin
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

import org.junit.Test
import org.junit.Assert._

import labelled.FieldType

class UnionTests {
  import union._
  import syntax.singleton._
  import test._
  import testutil._

  val wI = Witness('i)
  type i = wI.T

  val wS = Witness('s)
  type s = wS.T

  val sB = Witness('b)
  type b = sB.T

  type U = Union.`'i -> Int, 's -> String, 'b -> Boolean`.T

  @Test
  def testGetLiterals {
    val u1 = Coproduct[U]('i ->> 23)
    val u2 = Coproduct[U]('s ->> "foo")
    val u3 = Coproduct[U]('b ->> true)

    val v1 = u1.get('i)
    typed[Option[Int]](v1)
    assertEquals(Some(23), v1)

    val v2 = u2.get('s)
    typed[Option[String]](v2)
    assertEquals(Some("foo"), v2)

    val v3 = u3.get('b)
    typed[Option[Boolean]](v3)
    assertEquals(Some(true), v3)

    illTyped("""
      u1.get('foo)
    """)
  }

  @Test
  def testSelectDynamic {
    val u1 = Coproduct[U]('i ->> 23).union
    val u2 = Coproduct[U]('s ->> "foo").union
    val u3 = Coproduct[U]('b ->> true).union

    val v1 = u1.i
    typed[Option[Int]](v1)
    assertEquals(Some(23), v1)

    val n1 = u1.s
    typed[Option[String]](n1)
    assertEquals(None, n1)

    val v2 = u2.s
    typed[Option[String]](v2)
    assertEquals(Some("foo"), v2)

    val n2 = u2.b
    typed[Option[Boolean]](n2)
    assertEquals(None, n2)

    val v3 = u3.b
    typed[Option[Boolean]](v3)
    assertEquals(Some(true), v3)

    /*
     * illTyped gives a false positive here, but `u1.foo` does in fact fail to compile
     * however, it fails in a weird way:
     *   Unknown type: <error>, <error> [class scala.reflect.internal.Types$ErrorType$,
     *   class scala.reflect.internal.Types$ErrorType$] TypeRef? false
     */
    //illTyped("u1.foo")
  }

  @Test
  def testUnionTypeSelector {
    type ii = FieldType[i, Int] :+: CNil
    typed[ii](Coproduct[Union.`'i -> Int`.T]('i ->> 23))

    type iiss = FieldType[i, Int] :+: FieldType[s, String] :+: CNil
    typed[iiss](Coproduct[Union.`'i -> Int, 's -> String`.T]('s ->> "foo"))

    type iissbb = FieldType[i, Int] :+: FieldType[s, String] :+: FieldType[b, Boolean] :+: CNil
    typed[iissbb](Coproduct[Union.`'i -> Int, 's -> String, 'b -> Boolean`.T]('b ->> true))
  }

  @Test
  def testNamedArgsInject {
    val u1 = Union[U](i = 23)
    val u2 = Union[U](s = "foo")
    val u3 = Union[U](b = true)

    val v1 = u1.get('i)
    typed[Option[Int]](v1)
    assertEquals(Some(23), v1)

    val v2 = u2.get('s)
    typed[Option[String]](v2)
    assertEquals(Some("foo"), v2)

    val v3 = u3.get('b)
    typed[Option[Boolean]](v3)
    assertEquals(Some(true), v3)

    illTyped("""
      u1.get('foo)
    """)
  }

  val iSymbolWitness = Witness('i)
  val sSymbolWitness = Witness('s)
  val bSymbolWitness = Witness('b)

  object filter extends FieldPoly {
    implicit def i = at[iSymbolWitness.T](_ => True)
    implicit def s = at[sSymbolWitness.T](_ => False)
    implicit def b = at[bSymbolWitness.T](_ => True)
  }

  @Test
  def testFilterKeys {
    type U = Union.`'i -> Int, 's -> String, 'b -> Boolean`.T
    type R = Union.`'i -> Int, 'b -> Boolean`.T

    val u1 = Union[U](i = 23)
    val u2 = Union[U](s = "foo")
    val u3 = Union[U](b = true)
    
    val r1 = u1.filterKeys(filter)
    val r2 = u2.filterKeys(filter)
    val r3 = u3.filterKeys(filter)
    
    assertTypedEquals[Option[R]](Some(Union[R](i = 23)), r1)
    assertTypedEquals[Option[R]](None, r2)
    assertTypedEquals[Option[R]](Some(Union[R](b = true)), r3)
  }
}
