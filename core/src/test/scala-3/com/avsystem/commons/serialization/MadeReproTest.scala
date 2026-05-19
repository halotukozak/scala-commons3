package com.avsystem.commons.serialization

import made.*
import made.annotation.{optionalParam, whenAbsent}
import org.scalatest.funsuite.AnyFunSuite

// Top-level case classes — nested-in-class causes scala-3 "missing outer accessor" crash
case class WithTransient(
  str: String,
  @transientDefault int: Int = 42,
  @transientDefault @whenAbsent("dafuq") s2: String = "kek",
)

case class WithOptionals(
  str: String,
  @optionalParam int: com.avsystem.commons.misc.Opt[Int],
  @optionalParam bul: Option[Boolean],
)

case class WithTransientRT(str: String, @transientDefault int: Int = 42)
object WithTransientRT extends HasGenCodec[WithTransientRT]

class MadeReproTest extends AnyFunSuite:

  inline def fetchTransient[T: Made.Of as made]: Array[Boolean] =
    inline made match
      case made: Made.ProductOf[T] =>
        made.elems
          .hasAnnotations[transientDefault]
          .toArrayOf[Boolean](using containsOnly.refl)
      case _ => Array.empty[Boolean]

  inline def fetchOptional[T: Made.Of as made]: Array[Boolean] =
    inline made match
      case made: Made.ProductOf[T] =>
        made.elems
          .hasAnnotations[optionalParam]
          .toArrayOf[Boolean](using containsOnly.refl)
      case _ => Array.empty[Boolean]

  test("transient annotation detected on WithTransient fields") {
    val arr = fetchTransient[WithTransient]
    println(s"[REPRO] transient=${arr.toList}")
    assert(arr.toList == List(false, true, true))
  }

  test("optional annotation detected on WithOptionals fields") {
    val arr = fetchOptional[WithOptionals]
    println(s"[REPRO] optional=${arr.toList}")
    assert(arr.toList == List(false, true, true))
  }

  // --- Mirror commons GenCodec.derived path: round-trip write ---

  test("HasGenCodec.write strips default-value transient field") {
    import com.avsystem.commons.serialization.SimpleValueOutput
    var captured: Any = null
    GenCodec.write(new SimpleValueOutput((v: Any) => captured = v), WithTransientRT("lol"))
    println(s"[REPRO] HasGenCodec output: $captured")
    assert(captured == Map("str" -> "lol"), s"int=42 (default) should be stripped, got: $captured")
  }

  // --- Direct probe of MadeFieldElem.default ---

  inline def probeDefaults[T: Made.Of as made]: List[Option[Any]] =
    inline made match
      case made: Made.ProductOf[T] =>
        made.elems.toArrayOf[MadeFieldElem](using containsOnly.refl).map(_.default).toList
      case _ => Nil

  test("MadeFieldElem.default returns Some(42) for @transientDefault int = 42") {
    val defaults = probeDefaults[WithTransient]
    println(s"[REPRO] WithTransient defaults: $defaults")
    assertResult(List(None, Some(42), Some("dafuq")))(defaults)
  }

  test("WithTransientRT — direct view of skip components") {
    val defaults = probeDefaults[WithTransientRT]
    val transient = fetchTransient[WithTransientRT]
    println(s"[REPRO] RT defaults=$defaults, transient=${transient.toList}")
    assertResult(List(None, Some(42)))(defaults)
    assertResult(List(false, true))(transient.toList)
  }

  // --- Test through multi-level inline call (like HasGenCodec → MacroInstances → summonInline → derived → unsafeDerived) ---

  inline def innerFetch[T: Made.Of as made]: Array[Boolean] =
    inline made match
      case made: Made.ProductOf[T] =>
        made.elems
          .hasAnnotations[transientDefault]
          .toArrayOf[Boolean](using containsOnly.refl)
      case _ => Array.empty[Boolean]

  inline def outerLevel1[T]: Array[Boolean] = innerFetch[T](using compiletime.summonInline[Made.Of[T]])
  inline def outerLevel2[T]: Array[Boolean] = outerLevel1[T]
  inline def outerLevel3[T]: Array[Boolean] = outerLevel2[T]

  test("multi-level inline call preserves hasAnnotations precision") {
    val arr1 = outerLevel1[WithTransientRT]
    val arr2 = outerLevel2[WithTransientRT]
    val arr3 = outerLevel3[WithTransientRT]
    println(s"[REPRO] L1=${arr1.toList} L2=${arr2.toList} L3=${arr3.toList}")
    assertResult(List(false, true))(arr3.toList)
  }

  // --- Same multi-level but without summonInline ---

  inline def outerCB[T: Made.Of]: Array[Boolean] = innerFetch[T]
  inline def outerCB2[T: Made.Of]: Array[Boolean] = outerCB[T]

  test("multi-level via context bound (no summonInline) preserves precision?") {
    val arr = outerCB2[WithTransientRT]
    println(s"[REPRO] outerCB2=${arr.toList}")
    assertResult(List(false, true))(arr.toList)
  }

  // --- Direct Made.derived[T] passed to using parameter ---

  inline def outerDirect[T]: Array[Boolean] = innerFetch[T](using Made.derived[T])
  inline def outerDirect2[T]: Array[Boolean] = outerDirect[T]

  test("multi-level via direct Made.derived[T] (no summonInline) preserves precision?") {
    val arr = outerDirect2[WithTransientRT]
    println(s"[REPRO] outerDirect2=${arr.toList}")
    assertResult(List(false, true))(arr.toList)
  }
