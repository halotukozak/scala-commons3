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
}
