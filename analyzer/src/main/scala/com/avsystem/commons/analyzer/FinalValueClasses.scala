package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import Contexts.*
import Symbols.*

class FinalValueClasses() extends CheckingRule("finalValueClasses", SeverityLevel.Warning) {
  def performCheck(unitTree: Tree)(using Context): Unit = {
    val anyValClass = defn.AnyValClass

    checkChildren(unitTree) { tree =>
      tree match {
        case classDef: TypeDef if classDef.symbol.isClass && !classDef.symbol.is(Flags.Final) =>
          val classType = classDef.symbol.info
          if (classType.baseClasses.contains(anyValClass))
            emitReport(classDef.srcPos, "Value classes should be marked as final")
        case _ =>
      }
    }
  }
}
