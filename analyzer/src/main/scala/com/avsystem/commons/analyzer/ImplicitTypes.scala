package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import Contexts.*
import Symbols.*

class ImplicitTypes() extends CheckingRule("implicitTypes") {
  def performCheck(unitTree: Tree)(using Context): Unit = {
    checkChildren(unitTree) { tree =>
      tree match {
        case valDef: ValDef if valDef.symbol.is(Flags.Implicit) && !valDef.symbol.is(Flags.Synthetic) =>
          valDef.tpt match {
            case typeTree: TypeTree if !typeTree.span.exists || typeTree.span.isZeroExtent =>
              emitReport(valDef.srcPos, "Implicit definitions must have type annotated explicitly")
            case _ =>
          }
        case defDef: DefDef if defDef.symbol.is(Flags.Implicit) && !defDef.symbol.is(Flags.Synthetic) =>
          defDef.tpt match {
            case typeTree: TypeTree if !typeTree.span.exists || typeTree.span.isZeroExtent =>
              emitReport(defDef.srcPos, "Implicit definitions must have type annotated explicitly")
            case _ =>
          }
        case _ =>
      }
    }
  }
}
