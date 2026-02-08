package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Symbols.*

class ExplicitGenerics(using Context) extends AnalyzerRuleOnTyped("explicitGenerics") {
  private lazy val extractExplicitGenericsAnnotation =
    resolveClassType("com.avsystem.commons.annotation.explicitGenerics")

  def analyze(unitTree: Tree)(using Context): Unit = extractExplicitGenericsAnnotation.foreach { explicitGenAnnotType =>
    def requiresExplicitGenerics(sym: Symbol): Boolean = (sym :: sym.allOverriddenSymbols.toList).exists { s =>
      s.annotations.exists(_.symbol.typeRef <:< explicitGenAnnotType)
    }

    checkChildren(unitTree) {
      case app @ TypeApply(fn, typeArgs) =>
        val fnSym = fn.symbol
        if (fnSym.exists) {
          if (requiresExplicitGenerics(fnSym)) {
            val allInferred = typeArgs.forall {
              case tt: TypeTree => !tt.span.exists || tt.span.isZeroExtent
              case _ => false
            }

            if (allInferred) {
              emitReport(
                app.srcPos,
                s"$fnSym requires that its type arguments are explicit (not inferred)",
              )
            }
          }
        }
      case _ =>
    }
  }
}
