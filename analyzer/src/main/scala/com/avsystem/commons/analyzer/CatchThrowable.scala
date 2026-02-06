package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Symbols.*

class CatchThrowable(using Context) extends AnalyzerRuleOnTyped("catchThrowable", Level.Warn) {
  def analyze(unitTree: Tree)(using Context): Unit = {
    val throwableType = defn.ThrowableType

    def isCustomExtractor(pattern: Tree): Boolean = pattern match {
      case UnApply(Apply(Select(_, name), _), _, _) if name.toString == "unapply" => true
      case _ => false
    }

    def examinePattern(pattern: Tree): Unit =
      if (pattern.tpe != null && pattern.tpe =:= throwableType && !isCustomExtractor(pattern))
        emitReport(pattern.srcPos, "Catching Throwable is discouraged, catch specific exceptions instead")

    checkChildren(unitTree) {
      case tryTree: Try =>
        tryTree.cases.foreach {
          case caseDef @ CaseDef(Alternative(patterns), _, _) => patterns.foreach(examinePattern)
          case caseDef @ CaseDef(Bind(_, Alternative(patterns)), _, _) => patterns.foreach(examinePattern)
          case caseDef @ CaseDef(pattern, _, _) if caseDef.span.exists => examinePattern(pattern)
          case _ =>
        }
      case _ =>
    }
  }
}
