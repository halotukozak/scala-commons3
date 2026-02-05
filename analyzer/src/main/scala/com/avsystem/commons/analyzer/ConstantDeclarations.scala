package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd
import core.*
import Contexts.*
import Symbols.*
import Types.*
import Constants.*

class ConstantDeclarations() extends CheckingRule("constantDeclarations", SeverityLevel.Disabled):
  def performCheck(unitTree: tpd.Tree)(using Context): Unit =
    object ConstantChecker extends tpd.TreeTraverser:
      override def traverse(tree: tpd.Tree)(using Context): Unit =
        tree match
          case valDef: tpd.ValDef if valDef.symbol.exists && valDef.symbol.owner.isEffectivelyFinal =>
            val valSym = valDef.symbol
            if valSym.isPublic && !valSym.allOverriddenSymbols.iterator.hasNext then
              val isLiteralConstant = valDef.rhs.tpe match
                case ConstantType(_) => true
                case _ => false
              
              val valName = valDef.name.toString
              val startsUpper = valName.nonEmpty && valName.charAt(0).isUpper
              val isFinalVal = valSym.is(Flags.Final)
              
              def emitDiagnostic(msg: String): Unit =
                emitReport(valDef.srcPos, msg)
              
              if isLiteralConstant && (!startsUpper || !isFinalVal) then
                emitDiagnostic("a literal-valued constant should be declared as a `final val` with an UpperCamelCase name")
              
              if !isLiteralConstant && startsUpper && !isFinalVal then
                emitDiagnostic("a constant with UpperCamelCase name should be declared as a `final val`")
              
              if isFinalVal && isLiteralConstant && valDef.tpt.tpe != null && !(valDef.tpt.tpe =:= valDef.rhs.tpe) then
                emitDiagnostic("a constant with a literal value should not have an explicit type annotation")
            traverseChildren(tree)
          case _ => traverseChildren(tree)
    ConstantChecker.traverse(unitTree)
