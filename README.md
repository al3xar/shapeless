My shapeless *sandbox*, where I put my latest developments/attempts,
prior to submitting PRs to the main shapeless repo if I think they
are worth it.

## Extra features
### Straighforward literal value types
```scala
val i: Literal.`2`.T = 2
val s: Literal.`"a"`.T = "a"
illTyped(""" val i: Literal.`2`.T = 3 """)
```

### Straightforward HList/Coproduct types made of literal value types
```scala
val l: Literals.`2, "a", true`.T = 2.narrow :: "a".narrow :: true.narrow :: HNil
val c1 = Coproduct[Literals.`2, "a", true`.S](2.narrow)
val c2 = Coproduct[Literals.`2, "a", true`.S]("a".narrow)
val c3 = Coproduct[Literals.`2, "a", true`.S](true.narrow)
illTyped(""" val i: Literals.`2, "a", true`.T = 2.narrow :: "b".narrow :: true.narrow :: HNil """)
```

### Enhanced zipWithKeys method on records/unions
```scala
val l = "a" :: 2 :: false :: HNil
val rec = l.zipWithKeys[Literals.`'s, 'i, 'b`.T]
val c1 = Coproduct[String :+: Int :+: Boolean :+: CNil]("a")
val u1 = c1.zipWithKeys[Literals.`'s, 'i, 'b`.T]
```

### Bootstrapped build
Some macros (the ones related to literals mainly)
are built in a bootstrap project, so that they can be used in the library
itself.

### filterType methods instead of filter
The `filter` and `filterNot` methods, for `HList`s and `Coproduct`s,
that filter types, are renamed to `filterType` / `filterNotType`:
```scala
val l = 2 :: "a" :: 0.0 :: HNil
val filtered = l.filterType[Int]
val filteredNot = l.filterNotType[String]
```

### Boolean-literal based filtering
```scala
object atLeastTwo extends Poly1 {
  implicit def upToOne[N <: Nat](implicit ev: N  < nat._2) = at[N](_ => False)
  implicit def fromTwo[N <: Nat](implicit ev: nat._2 <= N) = at[N](_ => True)
}

val l = Nat(1) :: Nat(2) :: HNil
val filtered = l.filter(atLeastTwo)
```
Features `filter` and `filterNot` for both `HList`s and `Coproduct`s.

### filterKeys methods on record/union
```scala
object filter extends Poly1 {
  implicit def i = at[Literal.`'i`.T](_ => True)
  implicit def s = at[Literal.`'s`.T](_ => False)
  implicit def b = at[Literal.`'b`.T](_ => True)
}

val r = Record(i = 23, s = "foo", b = true)
val filtered = r.filterKeys(filter)
```

### Simplified HNil definition
No `HNil` trait, no distinction between `HNil` and `HNil.type`. Allows to simplify
 `HNil` implicits definitions, typically:
```scala
implicit def hnilImpl: Aux[HNil, HNil] = new Impl[HNil] { ... }
```
instead of
```scala
implicit def hnilImpl[L <: HNil]: Aux[L, HNil] = new Impl[L] { ... }
```

### toHList and toRecord methods on case classes
```scala
import syntax.std.product._
case class Bar(i: Int, s: String)
val bar = Bar(2, "b")

val l = bar.toHList // HList(2, "b")
val rec = bar.toRecord // Record(i=2, s="b")
```

### Case-class like syntax for record and union types
```scala
type R = Record.`i: Int, s: String`.T
// Instead of type R = Record.`'i -> Int, 's -> String`.T
type U = Union.`i: Int, s: String`.T
// Instead of type R = Union.`'i -> Int, 's -> String`.T
```

### Empty record/union types allowed
```scala
type R = Record.` `.T
type U = Union.` `.T
```

### More precise Generic-like type classes
- `LabelledGeneric` only accepts labelled types (no bare HList or tuples)

- Added `NonLabelledGeneric` that only accepts *non* labelled types (bare HLists and tuples)

- The former `LabelledGeneric` is now `LooseLabelledGeneric`

- Added `IsTuple` and `IsCaseClass` generic-like type classes

- Prefixed the `*Generic` type classes with `Is`

```scala
type L = Int :: String :: HNil
IsGeneric[L]
illTyped(" IsLabelledGeneric[L] ")
IsNonLabelledGeneric[L]
IsLooseLabelledGeneric[L]
illTyped(" IsTuple[L] ")
illTyped(" IsCaseClass[L] ")

type T = (Int, String)
IsGeneric[T]
illTyped(" IsLabelledGeneric[T] ")
IsNonLabelledGeneric[T]
IsLooseLabelledGeneric[T]
IsTuple[T]
illTyped(" IsCaseClass[T] ")

type R = Record.`i: Int, s: String`.T
IsGeneric[R]
IsLabelledGeneric[R]
illTyped(" IsNonLabelledGeneric[R] ")
IsLooseLabelledGeneric[R]
illTyped(" IsTuple[R] ")
illTyped(" IsCaseClass[R] ")

type U = Union.`i: Int, s: String`.T
IsGeneric[U]
IsLabelledGeneric[U]
illTyped(" IsNonLabelledGeneric[U] ")
IsLooseLabelledGeneric[U]
illTyped(" IsTuple[U] ")
illTyped(" IsCaseClass[U] ")

case class CC(i: Int, s: String)
IsGeneric[CC]
IsLabelledGeneric[CC]
illTyped(" IsNonLabelledGeneric[CC] ")
IsLooseLabelledGeneric[CC]
illTyped(" IsTuple[CC] ")
IsCaseClass[CC]

sealed trait CP
case class CPInt(i: Int) extends CP
case class CPString(s: String) extends CP
IsGeneric[CP]
IsLabelledGeneric[CP]
illTyped(" IsNonLabelledGeneric[CP] ")
IsLooseLabelledGeneric[CP]
illTyped(" IsTuple[CP] ")
illTyped(" IsCaseClass[CP] ")
```

### Removed scarcely used or not recommended features
- Removed the unused logical types `¬`, `∧`, `∨`, & others

- Removed the unnecessary `<:!<` and `=:!=` types

### Straightforward access to record/union fields
Enabled direct `selectDynamic` access to record and union fields:
```scala
val r = Record(i=1, s="b")
r.i
r.s

type U = Union.`i: Int, s: String`.T
val u = Union[U](i=1)
u.i
u.s
```

### List-like toString method on HLists
```scala
scala> HList(1, "a").toString
res0: String = HList(1, a)
```

### Conversion from  traversables to HLists/records/case classes
```scala
type R = Record.`i: Int, s: String`.T
case class Bar(i: Int, s: String)

val l = List(1, "b")
l.toHList[Int :: String :: HNil] // Some(1 :: "b" :: HNil): Option[Int :: String :: HNil]
l.toGeneric[Bar] // Some(Bar(1, "b")): Option[Bar]
l.toGeneric[R] // Some(Record(i=1, s="b")): Option[R]
```

## TODO

### Tuple-like access to HList and Coproduct elements
```scala
val l = 1 :: "b" :: HNil
l._1
l._2

type C = Int :+: String :+: CNil
val c = Coproduct[C]("b")
c._1
c._2
```

### Simpler syntax for HList and Coproduct types
```scala
type L = HList.`Int, String`.T
val l: L = HList(1, "b")

type C = Coproduct.`Int, String`.T
val c = Coproduct[C](2)
```

### Straightforward conversion of case classes to standard collections
```scala
import syntax.std.product._
case class Bar(i: Int, s: Double)
val bar = Bar(2, 1.0)

bar.to[IndexedSeq]     // IndexedSeq[AnyVal](1, 1.0)
bar.toList             // List[AnyVal](2, 1.0)         
bar.toList[Any]        // List[Any](2, 1.0)
bar.toArray            // Array[AnyVal](2, 1.0)
bar.toArray[Any]       // Array[Any](2, 1.0)
bar.toMap              // Map[Symbol, AnyVal]('i -> 2, 's -> 1.0)
bar.toMap[Symbol, Any] // Map[Symbol, Any]('i -> 2, 's -> 1.0)
```

### Conversion of maps to records/unions/case classes/sealed hierarchy
```scala
type R = Record.`i: Int, s: String`.T
type U = Union.`i: Int, s: String`.T
case class Bar(i: Int, s: String)

val m = Map("i" -> 1, "s" -> "a")
m.toRecord[R] // Some(Record(i=1, s="a")): Option[R]
m.toLabelledGeneric[Bar] // Some(Bar(1, "a")): Option[Bar]

Map("i" -> 1).toUnion[U] // Some(Union[U](i=1)): Option[U]
Map("s" -> "a").toUnion[U] // Some(Union[U](s="a")): Option[U]
```

### IsSealedHierarchy generic-like type class