package com.avsystem.commons.serialization

import com.avsystem.commons.derivation.AllowImplicitMacro
import com.avsystem.commons.serialization

import scala.quoted.*

trait RawRefCreatorMacros[S] {
  inline def ref[T](fun: S => T): RawRef = ${ SerializationMacros.refImpl[S, T]('fun) }
}

trait GenRefCreatorMacros[S] {
  inline def ref[T](fun: S => T): GenRef[S, T] = ${ SerializationMacros.refImpl[S, T]('fun) }
}

trait GenRefImplicitsMacros {
  given [S, T] => Conversion[S => T, GenRef[S, T]] = ???
}

object SerializationMacros {

  def refImpl[S: Type, T: Type](fun: Expr[S => T])(using Quotes): Expr[Nothing] = ???

  inline def validateTransientDefaults[T]: Unit = ${ validateTransientDefaultsImpl[T] }

  def validateTransientDefaultsImpl[T: Type](using Quotes): Expr[Unit] = {
    import quotes.reflect.*
    val tSym = TypeRepr.of[T].typeSymbol
    if (tSym.flags.is(Flags.Case) && !tSym.flags.is(Flags.Module)) {
      val transientDefaultSym = TypeRepr.of[transientDefault].typeSymbol
      val whenAbsentSym = TypeRepr.of[made.annotation.whenAbsent[Any]].typeSymbol
      val optionalParamSym = TypeRepr.of[made.annotation.optionalParam].typeSymbol
      val companion = tSym.companionModule
      val ctor = tSym.primaryConstructor
      val defaultMethodNames =
        if (companion.exists) companion.methodMembers.iterator.map(_.name).toSet
        else Set.empty[String]
      ctor.paramSymss.flatten.zipWithIndex.foreach { case (param, i) =>
        if (param.hasAnnotation(transientDefaultSym)) {
          val hasWhenAbsent = param.hasAnnotation(whenAbsentSym)
          val hasOptionalParam = param.hasAnnotation(optionalParamSym)
          val hasScalaDefault = defaultMethodNames.contains(s"$$lessinit$$greater$$default$$${i + 1}")
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
