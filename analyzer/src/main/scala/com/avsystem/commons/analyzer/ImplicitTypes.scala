package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd
import core.*
import Contexts.*
import Symbols.*

class ImplicitTypes() extends CheckingRule("implicitTypes") {
  def performCheck(unitTree: tpd.Tree)(using Context): Unit = {
    object ImplicitTypeChecker extends tpd.TreeTraverser {
      override def traverse(tree: tpd.Tree)(using Context): Unit =
        tree match {
          case valDef: tpd.ValDef if valDef.symbol.is(Flags.Implicit) && !valDef.symbol.is(Flags.Synthetic) =>
            valDef.tpt match {
              case typeTree: tpd.TypeTree if !typeTree.span.exists || typeTree.span.isZeroExtent =>
                emitReport(valDef.srcPos, "Implicit definitions must have type annotated explicitly")
              case _ =>
            }
            traverseChildren(tree)
          case defDef: tpd.DefDef if defDef.symbol.is(Flags.Implicit) && !defDef.symbol.is(Flags.Synthetic) =>
            defDef.tpt match {
              case typeTree: tpd.TypeTree if !typeTree.span.exists || typeTree.span.isZeroExtent =>
                emitReport(defDef.srcPos, "Implicit definitions must have type annotated explicitly")
              case _ =>
            }
            traverseChildren(tree)
          case _ => traverseChildren(tree)
        }
    }
    ImplicitTypeChecker.traverse(unitTree)
  }
}
