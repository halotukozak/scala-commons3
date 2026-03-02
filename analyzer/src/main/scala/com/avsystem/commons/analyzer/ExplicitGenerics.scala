package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.SymDenotations.NoDenotation.hasAnnotation
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Symbols.{NoSymbol, Symbol}

class ExplicitGenerics(using Context) extends AnalyzerRule("explicitGenerics") {
  private lazy val explicitGenericsAnnotClass =
    Symbols.getClassIfDefined("com.avsystem.commons.annotation.explicitGenerics")
  override def requiredSymbols: List[Symbol] = explicitGenericsAnnotClass :: Nil

  override def verifyTypeApply(tree: tpd.TypeApply)(using Context): Unit = if (tree.fun.symbol != NoSymbol) {
    val sym = tree.fun.symbol

    if (sym.hasOrInheritsAnnotation(explicitGenericsAnnotClass)) {
      // Inferred type args in Scala 3 have their span set to the fun's span
      // (identical start/end), while explicit type args have distinct spans
      // that come after the fun span (inside the [T1, T2] brackets).
      val funSpan = tree.fun.span
      val allInferred = tree.args.forall { arg =>
        val s = arg.span
        !s.exists || (s.start == funSpan.start && s.end == funSpan.end)
      }
      if (allInferred) {
        report(tree, s"${sym.name} requires explicit type arguments")
      }
    }
  }
}
