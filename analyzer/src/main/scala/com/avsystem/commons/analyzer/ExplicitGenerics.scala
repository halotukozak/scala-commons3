package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import Contexts.*
import Symbols.*
import Types.*

class ExplicitGenerics(using Context) extends AnalyzerRuleOnTyped("explicitGenerics") {
  private lazy val extractExplicitGenericsAnnotation = resolveClassType("com.avsystem.commons.annotation.explicitGenerics")

  def performCheck(unitTree: Tree)(using Context): Unit = extractExplicitGenericsAnnotation.foreach { explicitGenAnnotType =>
      checkChildren(unitTree) {
        case app @ TypeApply(fn, typeArgs) =>
          val fnSym = fn.symbol
          if (fnSym.exists) {
            val requiresExplicit = (fnSym :: fnSym.allOverriddenSymbols.toList).exists { s =>
              s.annotations.exists(_.symbol.typeRef <:< explicitGenAnnotType)
            }

            if (requiresExplicit) {
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
