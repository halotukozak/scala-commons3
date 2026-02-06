package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Symbols.*

class CatchThrowable(using Context) extends AnalyzerRuleOnTyped("catchThrowable", Level.Warn) {
  def analyze(unitTree: Tree)(using Context): Unit =
    checkChildren(unitTree) {
      case tryTree: Try =>
        tryTree.cases.foreach {
          case caseDef @ CaseDef(Alternative(patterns), _, _) => patterns.foreach(checkTree)
          case caseDef @ CaseDef(Bind(_, Alternative(patterns)), _, _) => patterns.foreach(checkTree)
          case caseDef @ CaseDef(pattern, _, _) if caseDef.span.exists => checkTree(pattern)
          case _ =>
        }
      case _ =>
    }
  private def isCustomExtractor(pattern: Tree): Boolean = pattern match {
    case UnApply(Apply(Select(_, name), _), _, _) if name.toString == "unapply" => true
    case _ => false
  }
  private def checkTree(pat: Tree): Unit =
    if (pat.tpe != null && pat.tpe =:= defn.ThrowableType && !isCustomExtractor(pat))
      emitReport(pat.srcPos, "Catching Throwable is discouraged, catch specific exceptions instead")
}
