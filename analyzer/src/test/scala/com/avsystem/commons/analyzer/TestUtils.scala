package com.avsystem.commons
package analyzer

import com.avsystem.commons.annotation.{atLeast, explicitGenerics, macroPrivate}

object TestUtils {
  def need3Params(@atLeast(3) args: Any*) = ()

  @macroPrivate
  def macroPrivateMethod = 42

  inline def invokeMacroPrivateMethod: Int = ${ invokeMacroPrivateMethodImpl }

  import scala.quoted.*
  def invokeMacroPrivateMethodImpl(using Quotes): Expr[Int] = {
    import quotes.reflect.*
    Select.unique(This(Symbol.requiredModule("com.avsystem.commons.analyzer.TestUtils")).asExpr.asTerm, "macroPrivateMethod").asExprOf[Int]
  }

  object Extractor {
    @macroPrivate def unapply(any: Any): Option[Any] = None
  }

  @explicitGenerics
  def genericMethod[T](arg: T): T = arg
  
  @explicitGenerics
  inline def genericMacro[T](arg: T): T = ${ genericMacroImpl[T]('arg) }
  
  def genericMacroImpl[T: Type](arg: Expr[T])(using Quotes): Expr[T] = arg
}
