package shapeless

import org.junit.Test
import testutil._

import syntax.std.product._

class ProductTests {

  case object Empty
  case class EmptyCC()
  case class Foo(i: Int, s: String)
  case class Bar(b: Boolean, f: Foo)

  def equalInferredTypes[A,B](a: A, b: B)(implicit eq: A =:= B) {}

  @Test
  def testToHList = {
    // FIXME: should work (needs changes in GenericMacros?)
    // Empty.toHList
    
    val e = EmptyCC()
    val el = e.toHList
    equalInferredTypes(HNil, el)
    
    val foo = Foo(1, "b")
    val fooL = foo.toHList
    val expectedFooL = 1 :: "b" :: HNil
    equalInferredTypes(expectedFooL, fooL)
    assertTypedEquals(expectedFooL, fooL)
    
    val bar = Bar(true, foo)
    val barL = bar.toHList
    val expectedBarL = true :: foo :: HNil
    equalInferredTypes(expectedBarL, barL)
    assertTypedEquals(expectedBarL, barL)
  }
  
}
