package com.avsystem.commons
package mirror

import com.avsystem.commons.serialization.whenAbsent
import org.scalatest.funsuite.AnyFunSuite

class DerMirrorDefaultsTest extends AnyFunSuite {
  test("case class with defaults") {
    val m: DerMirror.Product {
      type MirroredType = WithDefaults
      type MirroredLabel = "WithDefaults"
      type Metadata = Meta
      type MirroredElems = DerElem {
        type MirroredType = Int
        type MirroredLabel = "x"
        type Metadata = Meta
      } *: DerElem {
        type MirroredType = String
        type MirroredLabel = "y"
        type Metadata = Meta
      } *: DerElem {
        type MirroredType = Boolean
        type MirroredLabel = "z"
        type Metadata = Meta
      } *: EmptyTuple
    } = DerMirror.derived[WithDefaults]

    val (x, y, z) = m.mirroredElems

    assert(x.default.isEmpty)
    assert(y.default.contains("hello"))
    assert(z.default.contains(true))
  }

  test("case class with all defaults") {
    val m: DerMirror.Product {
      type MirroredType = AllDefaults
      type MirroredLabel = "AllDefaults"
      type Metadata = Meta
      type MirroredElems = DerElem {
        type MirroredType = Int
        type MirroredLabel = "a"
        type Metadata = Meta
      } *: DerElem {
        type MirroredType = String
        type MirroredLabel = "b"
        type Metadata = Meta
      } *: EmptyTuple
    } = DerMirror.derived[AllDefaults]

    val (a, b) = m.mirroredElems

    assert(a.default.contains(1))
    assert(b.default.contains("test"))
  }

  test("case class with mixed defaults") {
    val m: DerMirror.Product {
      type MirroredType = MixedDefaults
      type MirroredLabel = "MixedDefaults"
      type Metadata = Meta
      type MirroredElems = DerElem {
        type MirroredType = Int
        type MirroredLabel = "required"
        type Metadata = Meta
      } *: DerElem {
        type MirroredType = String
        type MirroredLabel = "optional"
        type Metadata = Meta
      } *: EmptyTuple
    } = DerMirror.derived[MixedDefaults]

    val (a, b) = m.mirroredElems

    assert(a.default.isEmpty)
    assert(b.default.contains("default"))
  }

  test("GeneratedDerElem.default returns None") {
    val m = DerMirror.derived[WithDefaultGenerated]

    val y *: EmptyTuple = m.generatedElems
    assert(y.default.isEmpty)
  }

  test("@whenAbsent provides default") {
    val m = DerMirror.derived[WithWhenAbsent]

    val (x, y) = m.mirroredElems

    assert(x.default.isEmpty)
    assert(y.default.contains("absent"))
  }

  test("@whenAbsent takes priority over Scala default value") {
    val m = DerMirror.derived[WhenAbsentOverridesDefault]

    val (a, b) = m.mirroredElems

    assert(a.default.contains(42))
    assert(b.default.contains("fromAnnotation"))
  }

  test("mixing @whenAbsent, Scala defaults, and no defaults") {
    val m = DerMirror.derived[MixedWhenAbsent]

    val (a, b, c) = m.mirroredElems

    assert(a.default.isEmpty)
    assert(b.default.contains(99))
    assert(c.default.contains("scalaDefault"))
  }
}

case class WithDefaults(x: Int, y: String = "hello", z: Boolean = true)
case class AllDefaults(a: Int = 1, b: String = "test")
case class MixedDefaults(required: Int, optional: String = "default")
case class WithDefaultGenerated(x: Int, y: String = "hello") {
  @generated def gen: Int = x + y.length
}
case class WithWhenAbsent(x: Int, @whenAbsent("absent") y: String)
case class WhenAbsentOverridesDefault(@whenAbsent(42) a: Int = 0, @whenAbsent("fromAnnotation") b: String = "fromDefault")
case class MixedWhenAbsent(a: Int, @whenAbsent(99) b: Int, c: String = "scalaDefault")