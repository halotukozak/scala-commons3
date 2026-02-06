package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd
import core.*
import Contexts.*
import Symbols.*
import Types.*

class ImplicitFunctionParams() extends CheckingRule("implicitFunctionParams", SeverityLevel.Warning) {
  def performCheck(unitTree: tpd.Tree)(using Context): Unit = {
    object FunctionParamChecker extends tpd.TreeTraverser {
      override def traverse(tree: tpd.Tree)(using Context): Unit =
        tree match {
          case defDef: tpd.DefDef if defDef.symbol.is(Flags.Method) =>
            defDef.termParamss.foreach { paramClause =>
              if (paramClause.nonEmpty && paramClause.head.symbol.is(Flags.Implicit))
                paramClause.foreach { param =>
                  val paramType = param.tpt.tpe
                  if (
                    paramType != null &&
                    (defn.isFunctionType(paramType) || paramType.typeSymbol == defn.PartialFunctionClass)
                  )
                    emitReport(param.srcPos, "Implicit parameters should not have any function type")
                }
            }
            traverseChildren(tree)
          case _ => traverseChildren(tree)
        }
    }
    FunctionParamChecker.traverse(unitTree)
  }
}
