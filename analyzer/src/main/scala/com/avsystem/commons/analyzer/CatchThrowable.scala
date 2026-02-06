package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd
import core.*
import Contexts.*
import Symbols.*
import Types.*

class CatchThrowable() extends CheckingRule("catchThrowable", SeverityLevel.Warning) {
  def performCheck(unitTree: tpd.Tree)(using Context): Unit = {
    val throwableType = defn.ThrowableType

    def isCustomUnapply(pattern: tpd.Tree): Boolean = pattern match {
      case tpd.UnApply(tpd.Apply(tpd.Select(_, name), _), _, _) if name.toString == "unapply" => true
      case _ => false
    }

    def examinePattern(pattern: tpd.Tree): Unit =
      if (pattern.tpe != null && pattern.tpe =:= throwableType && !isCustomUnapply(pattern))
        emitReport(pattern.srcPos, "Catching Throwable is discouraged, catch specific exceptions instead")

    object ThrowableCatcher extends tpd.TreeTraverser {
      override def traverse(tree: tpd.Tree)(using Context): Unit =
        tree match {
          case tryTree: tpd.Try =>
            tryTree.cases.foreach {
              case caseDef @ tpd.CaseDef(tpd.Alternative(patterns), _, _) => patterns.foreach(examinePattern)
              case caseDef @ tpd.CaseDef(tpd.Bind(_, tpd.Alternative(patterns)), _, _) =>
                patterns.foreach(examinePattern)
              case caseDef @ tpd.CaseDef(pattern, _, _) if caseDef.span.exists => examinePattern(pattern)
              case _ =>
            }
            traverseChildren(tree)
          case _ => traverseChildren(tree)
        }
    }
    ThrowableCatcher.traverse(unitTree)
  }
}
