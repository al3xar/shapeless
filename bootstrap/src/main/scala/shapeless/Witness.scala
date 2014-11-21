package shapeless

import language.experimental.macros
import language.dynamics

trait Witness extends Serializable {
  type T
  val value: T {}
}

object Witness extends Dynamic {
  type Aux[T0] = Witness { type T = T0 }
  type Lt[Lub] = Witness { type T <: Lub }

  implicit def apply[T]: Aux[T] = macro bootstrap.SingletonTypeMacros.materializeImpl[T]

  implicit def apply[T](t: T): Lt[T] = macro bootstrap.SingletonTypeMacros.convertImpl[T]

  def selectDynamic(tpeSelector: String): Any = macro bootstrap.SingletonTypeMacros.witnessTypeImpl
}
