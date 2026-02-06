package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd
import core.*
import Contexts.*
import Symbols.*
import Types.*

class ExplicitGenerics() extends CheckingRule("explicitGenerics") {
  private def extractExplicitGenericsAnnotation(using Context): Type =
    resolveClassType("com.avsystem.commons.annotation.explicitGenerics")

  def performCheck(unitTree: tpd.Tree)(using Context): Unit = {
    val explicitGenAnnotType = extractExplicitGenericsAnnotation
    if (explicitGenAnnotType == NoType) return

    object ExplicitGenericsChecker extends tpd.TreeTraverser {
      override def traverse(tree: tpd.Tree)(using Context): Unit =
        tree match {
          case app @ tpd.TypeApply(fn, typeArgs) =>
            val fnSym = fn.symbol
            if (fnSym.exists) {
              val requiresExplicit = (fnSym :: fnSym.allOverriddenSymbols.toList).exists { s =>
                s.annotations.exists(_.symbol.typeRef <:< explicitGenAnnotType)
              }

              if (requiresExplicit) {
                val allInferred = typeArgs.forall {
                  case tt: tpd.TypeTree => !tt.span.exists || tt.span.isZeroExtent
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
            traverseChildren(tree)
          case _ => traverseChildren(tree)
        }
    }
    ExplicitGenericsChecker.traverse(unitTree)
  }
}
