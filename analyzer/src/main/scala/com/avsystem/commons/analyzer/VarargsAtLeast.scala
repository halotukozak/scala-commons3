package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import Contexts.*
import Symbols.*
import Types.*
import Constants.*
import Decorators.*

object VarargsAtLeast extends AnalyzerRule("varargsAtLeast") {
  private def extractAtLeastAnnotation(using Context): Type =
    resolveClassType("com.avsystem.commons.annotation.atLeast")

  def performCheck(unitTree: Tree)(using Context): Unit = {
    val atLeastAnnotType = extractAtLeastAnnotation
    if (atLeastAnnotType != NoType) {
      checkChildren(unitTree) {
        case app @ Apply(fn, args) =>
          val methodSym = fn.symbol
          if (methodSym.exists && methodSym.is(Flags.Method)) {
            val params = methodSym.info.paramInfoss.flatten
            if (params.nonEmpty && params.last.isRepeatedParam) {
              val lastIsVararg = args.lastOption.exists {
                case Typed(_, tpt) =>
                  tpt match {
                    case Ident(name) => name.toString.contains("*")
                    case _ => false
                  }
                case _ => false
              }

              if (!lastIsVararg) {
                val lastParamSym = methodSym.paramSymss.flatten.last
                val requiredCount = lastParamSym.annotations
                  .find(ann => ann.symbol.typeRef <:< atLeastAnnotType)
                  .map { annot =>
                    annot.tree match {
                      case Apply(_, List(Literal(Constant(n: Int)))) => n
                      case _ => 0
                    }
                  }
                  .getOrElse(0)

                val providedCount = args.length - params.length + 1

                if (providedCount < requiredCount)
                  emitReport(
                    app.srcPos,
                    s"This method requires at least $requiredCount arguments for its repeated parameter, $providedCount passed.",
                  )
              }
            }
          }
        case _ =>
      }
    }
  }
}
