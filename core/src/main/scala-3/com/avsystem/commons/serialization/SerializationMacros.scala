package com.avsystem.commons.serialization

import com.avsystem.commons.annotation.positioned
import com.avsystem.commons.derivation.AllowImplicitMacro
import com.avsystem.commons.misc.{Opt, OptArg, OptRef}
import com.avsystem.commons.serialization

import scala.language.implicitConversions
import scala.quoted.*

trait RawRefCreatorMacros[S] {
  inline def ref[T](inline fun: S => T): RawRef = ${ SerializationMacros.rawRefImpl[S, T]('fun) }
}

trait GenRefCreatorMacros[S] {
  inline def ref[T](inline fun: S => T): GenRef[S, T] = ${ SerializationMacros.genRefImpl[S, T]('fun) }
}

trait GenRefImplicitsMacros {
  // Kept as `inline implicit def` (not a `given Conversion`): the body is a macro splice over the
  // `inline fun` argument, which a `Conversion[S => T, GenRef[S, T]]`'s non-inline `apply` cannot carry.
  inline implicit def fun2GenRef[S, T](inline fun: S => T): GenRef[S, T] =
    ${ SerializationMacros.genRefImpl[S, T]('fun) }
}

object SerializationMacros {

  def rawRefImpl[S: Type, T: Type](fun: Expr[S => T])(using Quotes): Expr[RawRef] =
    GenRefBuilder.buildRawRef[S, T](fun)

  def genRefImpl[S: Type, T: Type](fun: Expr[S => T])(using Quotes): Expr[GenRef[S, T]] = {
    val raw = GenRefBuilder.buildRawRef[S, T](fun)
    '{ GenRef[S, T]($fun, $raw) }
  }

  inline def validateTransientDefaults[T]: Unit = ${ validateTransientDefaultsImpl[T] }

  def validateTransientDefaultsImpl[T: Type](using Quotes): Expr[Unit] = {
    import quotes.reflect.*
    val tSym = TypeRepr.of[T].typeSymbol
    if (tSym.flags.is(Flags.Case) && !tSym.flags.is(Flags.Module)) {
      val transientDefaultSym = TypeRepr.of[transientDefault].typeSymbol
      val whenAbsentSym = TypeRepr.of[made.annotation.whenAbsent[Any]].typeSymbol
      val optionalParamSym = TypeRepr.of[made.annotation.optionalParam].typeSymbol
      tSym.primaryConstructor.paramSymss.flatten.foreach { param =>
        if (param.hasAnnotation(transientDefaultSym)) {
          val hasWhenAbsent = param.hasAnnotation(whenAbsentSym)
          val hasOptionalParam = param.hasAnnotation(optionalParamSym)
          val hasScalaDefault = param.flags.is(Flags.HasDefault)
          if (!hasWhenAbsent && !hasOptionalParam && !hasScalaDefault) {
            report.error(
              s"@transientDefault has no effect on parameter ${param.name} because it has no default value",
              param.pos.getOrElse(Position.ofMacroExpansion),
            )
          }
        }
      }
    }
    '{ () }
  }
}
