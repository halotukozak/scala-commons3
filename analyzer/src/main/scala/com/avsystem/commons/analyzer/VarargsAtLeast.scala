package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Constants.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Symbols.*

class VarargsAtLeast(using Context) extends AnalyzerRuleOnTyped("varargsAtLeast") {
  private lazy val extractAtLeastAnnotation = resolveClassType("com.avsystem.commons.annotation.atLeast")

  def analyze(unitTree: Tree)(using Context): Unit = extractAtLeastAnnotation.foreach { atLeastAnnotType =>
    checkChildren(unitTree) {
      case app @ Apply(fn, args) =>
        val methodSym = fn.symbol
        if (methodSym.exists && methodSym.is(Flags.Method)) {
          val params = methodSym.info.paramInfoss.flatten
          if (params.nonEmpty && params.last.isRepeatedParam) {
            val lastIsVararg = args.lastOption.exists {
              case Typed(_, Ident(name)) => name.toString.contains("*")
              case _ => false
            }

            if (!lastIsVararg) {
              val lastParamSym = methodSym.paramSymss.flatten.last
              val requiredCount = lastParamSym.annotations
                .find(ann => ann.symbol.typeRef <:< atLeastAnnotType)
                .map(_.tree)
                .map {
                  case Apply(_, List(Literal(Constant(n: Int)))) => n
                  case _ => 0
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
