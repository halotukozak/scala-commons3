package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd
import core.*
import Contexts.*
import Symbols.*

class FinalValueClasses() extends CheckingRule("finalValueClasses", SeverityLevel.Warning) {
  def performCheck(unitTree: tpd.Tree)(using Context): Unit = {
    val anyValClass = defn.AnyValClass

    object ValueClassChecker extends tpd.TreeTraverser {
      override def traverse(tree: tpd.Tree)(using Context): Unit =
        tree match {
          case classDef: tpd.TypeDef if classDef.symbol.isClass && !classDef.symbol.is(Flags.Final) =>
            val classType = classDef.symbol.info
            if (classType.baseClasses.contains(anyValClass))
              emitReport(classDef.srcPos, "Value classes should be marked as final")
            traverseChildren(tree)
          case _ => traverseChildren(tree)
        }
    }
    ValueClassChecker.traverse(unitTree)
  }
}
