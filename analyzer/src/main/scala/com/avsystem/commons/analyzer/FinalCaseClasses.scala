package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd
import core.*
import Contexts.*
import Symbols.*

class FinalCaseClasses() extends CheckingRule("finalCaseClasses", SeverityLevel.Warning):
  def performCheck(unitTree: tpd.Tree)(using Context): Unit =
    object CaseClassChecker extends tpd.TreeTraverser:
      override def traverse(tree: tpd.Tree)(using Context): Unit =
        tree match
          case classDef: tpd.TypeDef 
            if classDef.symbol.isClass && classDef.symbol.is(Flags.Case) &&
               !classDef.symbol.is(Flags.Final) && !classDef.symbol.is(Flags.Sealed) =>
            
            val isTopLevelOrStatic = classDef.symbol.isStatic
            
            if isTopLevelOrStatic then
              emitReport(classDef.srcPos, "Case classes should be marked as final")
            else
              emitReport(
                classDef.srcPos,
                "Case classes should be marked as final. Due to the SI-4440 bug, it cannot be done here. Consider moving the case class to the companion object",
                severity = SeverityLevel.Information
              )
            traverseChildren(tree)
          case _ => traverseChildren(tree)
    CaseClassChecker.traverse(unitTree)
