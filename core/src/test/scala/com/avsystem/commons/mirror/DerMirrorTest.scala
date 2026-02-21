package com.avsystem.commons
package mirror

import org.scalatest.funsuite.AnyFunSuite

import scala.deriving.Mirror.Sum

class DerMirrorTest extends AnyFunSuite {
  test("DerMirror for case class") {
    val _: DerMirror {
      type MirroredType = SimpleCaseClass
      type MirroredLabel = "SimpleCaseClass"
      type MirroredElems = DerElem {
        type MirroredType = Long
        type MirroredLabel = "id"
        type Metadata = Meta
      } *: DerElem {
        type MirroredType = String
        type MirroredLabel = "name"
        type Metadata = Meta
      } *: EmptyTuple
      type Metadata = Meta
    } = DerMirror.derived[SimpleCaseClass]
  }

  test("DerMirror for case class with no fields") {
    val _: DerMirror.Product {
      type MirroredType = NoFields
      type MirroredLabel = "NoFields"
      type Metadata = Meta
      type MirroredElems = EmptyTuple
    } = DerMirror.derived[NoFields]
  }

  test("DerMirror for generic case class") {
    val _: DerMirror.Product {
      type MirroredType = Box[Int]
      type MirroredLabel = "Box"
      type Metadata = Meta
      type MirroredElems = DerElem {
        type MirroredType = Int
        type MirroredLabel = "a"
        type Metadata = Meta
      } *: EmptyTuple
    } = DerMirror.derived[Box[Int]]
  }

  test("DerMirror for enum") {
    val _: DerMirror.Sum {
      type MirroredType = SimpleEnum
      type MirroredLabel = "SimpleEnum"
      type Metadata = Meta
      type MirroredElems = DerElem {
        type MirroredType = SimpleEnum.Case1.type
        type MirroredLabel = "Case1"
        type Metadata = Meta
      } *: DerElem {
        type MirroredType = SimpleEnum.Case2
        type MirroredLabel = "Case2"
        type Metadata = Meta
      } *: EmptyTuple
    } = DerMirror.derived[SimpleEnum]
  }

  test("DerMirror for object") {
    val mirror: DerMirror.Singleton {
      type MirroredType = SimpleObject.type
      type MirroredLabel = "SimpleObject"
      type Metadata = Meta
      type MirroredElems = EmptyTuple
    } = DerMirror.derived[SimpleObject.type]

    assert(mirror.value == SimpleObject)
  }

  test("DerMirror for Unit") {
    val mirror: DerMirror.Singleton {
      type MirroredType = Unit
      type MirroredLabel = "Unit"
      type Metadata = Meta
    } = DerMirror.derived[Unit]
    assert(mirror.value == ())
  }

  test("DerMirror for value class") {
    val mirror: DerMirror.Product {
      type MirroredType = ValueClass
      type MirroredLabel = "ValueClass"
      type Metadata = Meta
    } = DerMirror.derived[ValueClass]
    assert(mirror.fromUnsafeArray(Array("test")) == ValueClass("test"))
  }

  test("DerMirror for @transparent case class") {
    val mirror: DerMirror.Transparent {
      type MirroredType = TransparentClass
      type MirroredLabel = "TransparentClass"
      type Metadata = Meta @transparent
      type MirroredElemType = Int
      type MirroredElems = DerElem {
        type MirroredType = Int
        type MirroredLabel = "int"
        type Metadata = Meta
      } *: EmptyTuple
    } = DerMirror.derived[TransparentClass]

    val tc = TransparentClass(42)
    assert(mirror.unwrap(tc) == 42)
    assert(mirror.wrap(42) == tc)
  }

  test("getAnnotation and hasAnnotation") {
    val mirror = DerMirror.derived[AnnotatedCaseClass]
    summon[mirror.Metadata =:= (Meta @Annotation2 @Annotation1)]

    assert(mirror.hasAnnotation[Annotation1])
    assert(mirror.hasAnnotation[Annotation2])
    assert(!mirror.hasAnnotation[Annotation3])

    assert(mirror.getAnnotation[Annotation1].isDefined)
    assert(mirror.getAnnotation[Annotation2].isDefined)
    assert(mirror.getAnnotation[Annotation3].isEmpty)
  }

  test("parametrized annotation") {
    val mirror = DerMirror.derived[ParamAnnotated]
    val annot = mirror.getAnnotation[ParamAnnotation].get
    assert(annot.value == "foo")
  }

  test("DerMirror with annotations") {
    val _: DerMirror {
      type Metadata = Meta @Annotation2 @Annotation1
    } = DerMirror.derived[AnnotatedCaseClass]
  }

  test("DerMirror with many annotations") {
    val _: DerMirror {
      type Metadata = Meta @Annotation3 @Annotation2 @Annotation1
    } = DerMirror.derived[ManyAnnotated]
  }

  test("DerMirror for enum with @name") {
    val _: DerMirror.Sum {
      type MirroredType = NamedEnum
      type MirroredLabel = "NamedEnum"
      type Metadata = Meta
      type MirroredElems <: DerElem {
        type MirroredType = NamedEnum.Case1.type
        type MirroredLabel = "C1"
//        type Metadata = Meta @name("C1")
      } *: DerElem {
        type MirroredType = NamedEnum.Case2.type
        type MirroredLabel = "Case2"
        type Metadata = Meta
      } *: EmptyTuple
    } = DerMirror.derived[NamedEnum]
  }

  test("DerMirror for recursive ADT") {
    val _: DerMirror.Sum {
      type MirroredType = Recursive
      type MirroredLabel = "Recursive"
      type Metadata = Meta
      type MirroredElems = DerElem {
        type MirroredType = Recursive.End.type
        type MirroredLabel = "End"
        type Metadata = Meta
      } *: DerElem {
        type MirroredType = Recursive.Next
        type MirroredLabel = "Next"
        type Metadata = Meta
      } *: EmptyTuple
    } = DerMirror.derived[Recursive]
  }

  test("DerMirror for ADT with mixed cases") {
    val _: DerMirror.Sum {
      type MirroredType = MixedADT
      type MirroredLabel = "MixedADT"
      type Metadata = Meta
      type MirroredElems = DerElem {
        type MirroredType = MixedADT.CaseClass
        type MirroredLabel = "CaseClass"
        type Metadata = Meta
      } *: DerElem {
        type MirroredType = MixedADT.CaseObj.type
        type MirroredLabel = "CaseObj"
        type Metadata = Meta
      } *: EmptyTuple
    } = DerMirror.derived[MixedADT]
  }

  test("DerMirror should include @generated members") {
    val m: DerMirror {
      type MirroredType = HasGenerated
      type MirroredLabel = "HasGenerated"
      type Metadata = Meta
      type MirroredElems = DerElem {
        type MirroredType = String
        type MirroredLabel = "str"
        type Metadata = Meta
      } *: EmptyTuple
      type GeneratedElems = GeneratedDerElem {
        type OuterMirroredType = HasGenerated
        type MirroredType = Int
        type MirroredLabel = "gen"
        type Metadata = Meta @generated
      } *: EmptyTuple
    } = DerMirror.derived[HasGenerated]

    val instance = HasGenerated("test")
    assert(m.generatedElems(0).apply(instance) == 4)
  }

  test("DerMirror for HK case class") {
    val _: DerMirror.Product {
      type MirroredType = HKBox[List]
      type MirroredLabel = "HKBox"
      type Metadata = Meta
      type MirroredElems = DerElem {
        type MirroredType = List[Int]
        type MirroredLabel = "fa"
        type Metadata = Meta
      } *: EmptyTuple
    } = DerMirror.derived[HKBox[List]]
  }

  test("DerMirror for HK sum") {
    val _: DerMirror.Sum {
      type MirroredType = HKADT[List, Int]
      type MirroredLabel = "HKADT"
      type Metadata = Meta
      type MirroredElems = DerElem {
        type MirroredType = HKADT.Case1[List, Int]
        type MirroredLabel = "Case1"
        type Metadata = Meta
      } *: DerElem {
        type MirroredType = HKADT.Case2[List, Int]
        type MirroredLabel = "Case2"
        type Metadata = Meta
      } *: EmptyTuple
    } = DerMirror.derived[HKADT[List, Int]]
  }

  test("DerMirror for case class with defaults") {
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

  test("DerMirror for case class with all defaults") {
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

  test("DerMirror for case class with mixed defaults") {
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

  test("DerMirror for case class with wildcard") {
    val _: DerMirror.Product {
      type MirroredType = Box[?]
      type MirroredLabel = "Box"
      type Metadata = Meta;
      type MirroredElems <: DerElem {
        type MirroredLabel = "a"
        type Metadata = Meta
      } *: EmptyTuple
    } = DerMirror.derived[Box[?]]
  }

  test("fromUnsafeArray for simple case class") {
    val mirror = DerMirror.derived[SimpleCaseClass]
    val result = mirror.fromUnsafeArray(Array(42L, "test"))
    assert(result == SimpleCaseClass(42L, "test"))
  }

  test("fromUnsafeArray for case class with no fields") {
    val mirror = DerMirror.derived[NoFields]
    val result = mirror.fromUnsafeArray(Array.empty)
    assert(result == NoFields())
  }

  test("fromUnsafeArray for generic case class") {
    val mirror = DerMirror.derived[Box[String]]
    val result = mirror.fromUnsafeArray(Array("content"))
    assert(result == Box("content"))
  }

  test("fromUnsafeArray for case class with defaults") {
    val mirror = DerMirror.derived[WithDefaults]
    val result = mirror.fromUnsafeArray(Array(10, "world", false))
    assert(result == WithDefaults(10, "world", false))
  }
}

sealed trait MixedADT
sealed trait HKADT[F[_], T]
case class SimpleCaseClass(id: Long, name: String)
case class NoFields()
enum SimpleEnum {
  case Case1
  case Case2(data: String)
}
case class ValueClass(str: String) extends AnyVal
@transparent
case class TransparentClass(int: Int)
case class ParamAnnotation(value: String) extends MetaAnnotation
@ParamAnnotation("foo")
case class ParamAnnotated(id: Int)
case class Box[T](a: T)
enum NamedEnum {
  @name("C1") case Case1
  case Case2
}
class Annotation1 extends MetaAnnotation
class Annotation2 extends MetaAnnotation
class Annotation3 extends MetaAnnotation
@Annotation1 @Annotation2
case class AnnotatedCaseClass()
@Annotation1 @Annotation2 @Annotation3
case class ManyAnnotated()
enum Recursive {
  case End
  case Next(r: Recursive)
}
case class HasGenerated(str: String) {
  @generated def gen: Int = str.length
}
case class HKBox[F[_]](fa: F[Int])
object HKADT {
  case class Case1[F[_], T](a: T) extends HKADT[F, T]
  case class Case2[F[_], T](fa: F[T]) extends HKADT[F, T]
}
case class WithDefaults(x: Int, y: String = "hello", z: Boolean = true)
case class AllDefaults(a: Int = 1, b: String = "test")
case class MixedDefaults(required: Int, optional: String = "default")
case class WithDefaultGenerated(x: Int, y: String = "hello") {
  @generated def gen: Int = x + y.length
}
case object SimpleObject
object MixedADT {
  case class CaseClass(v: Int) extends MixedADT
  case object CaseObj extends MixedADT
}
