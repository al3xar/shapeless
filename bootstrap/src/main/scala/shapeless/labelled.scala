package shapeless

object labelled {
  /**
   * The type of fields with keys of singleton type `K` and value type `V`.
   */
  type FieldType[K, +V] = V with KeyTag[K, V]
  trait KeyTag[K, +V]

  /**
   * Yields a result encoding the supplied value with the singleton type `K' of its key.
   */
  def field[K] = new FieldBuilder[K]

  class FieldBuilder[K] {
    def apply[V](v : V): FieldType[K, V] = v.asInstanceOf[FieldType[K, V]]
  }
}
