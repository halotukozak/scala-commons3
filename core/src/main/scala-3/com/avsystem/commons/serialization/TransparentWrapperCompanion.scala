package com.avsystem.commons
package serialization

import com.avsystem.commons.meta.{AllowDerivation, MacroInstances}
import made.*

import scala.util.NotGiven

inline given [R, T] => (AllowDerivation[TransparentWrapping[R, T]]) => TransparentWrapping[R, T] =
  TransparentWrapping.derived[R, T]

/**
 * Base class for companion objects of case classes which are transparent wrappers ("newtypes") over their only field.
 * This is the usual way of providing [[TransparentWrapping]] for some type and is intended as a replacement for
 * [[transparent]] annotation where possible.
 */
abstract class TransparentWrapperCompanion[R, T](
  using macroInstances: MacroInstances[Unit, (tw: TransparentWrapping[R, T])],
) extends TransparentWrapping[R, T]
    with (R => T) {
  given NotGiven[AllowDerivation[TransparentWrapping[R, T]]] => TransparentWrapping[R, T] = macroInstances((), this).tw

  final def apply(x: R): T = wrap(x)
  final def unapply(x: T): Option[R] = Some(unwrap(x))

  final def wrap(r: R): T = summon[TransparentWrapping[R, T]].wrap(r)
  final def unwrap(t: T): R = summon[TransparentWrapping[R, T]].unwrap(t)

  given Ordering[R] => Ordering[T] = Ordering.by(unwrap)
}

abstract class StringWrapperCompanion[T](using MacroInstances[Unit, (tw: TransparentWrapping[String, T])])
  extends TransparentWrapperCompanion[String, T]
abstract class IntWrapperCompanion[T](using MacroInstances[Unit, (tw: TransparentWrapping[Int, T])])
  extends TransparentWrapperCompanion[Int, T]
abstract class LongWrapperCompanion[T](using MacroInstances[Unit, (tw: TransparentWrapping[Long, T])])
  extends TransparentWrapperCompanion[Long, T]
abstract class FloatWrapperCompanion[T](using MacroInstances[Unit, (tw: TransparentWrapping[Float, T])])
  extends TransparentWrapperCompanion[Float, T]
abstract class DoubleWrapperCompanion[T](using MacroInstances[Unit, (tw: TransparentWrapping[Double, T])])
  extends TransparentWrapperCompanion[Double, T]
abstract class BooleanWrapperCompanion[T](using MacroInstances[Unit, (tw: TransparentWrapping[Boolean, T])])
  extends TransparentWrapperCompanion[Boolean, T]
