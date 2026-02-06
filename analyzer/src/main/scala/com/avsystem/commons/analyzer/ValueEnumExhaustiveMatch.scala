package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import Contexts.*
import Symbols.*
import Types.*
import scala.collection.mutable

class ValueEnumExhaustiveMatch() extends AnalyzerRule("valueEnumExhaustiveMatch") {
  private def extractValueEnumTypes(using Context): (Type, Symbol) = {
    val valueEnumType = resolveClassType("com.avsystem.commons.misc.ValueEnum")
    val companionType = resolveClassType("com.avsystem.commons.misc.ValueEnumCompanion")
    val companionSym = if (companionType != NoType) companionType.typeSymbol else NoSymbol
    (valueEnumType, companionSym)
  }

  def performCheck(unitTree: Tree)(using Context): Unit = {
    val (valueEnumType, companionSym) = extractValueEnumTypes
    if (valueEnumType != NoType && companionSym.exists) {
      checkChildren(unitTree) { tree =>
        tree match {
          case matchTree @ Match(selector, cases) if selector.tpe <:< valueEnumType =>
            val selectorType = selector.tpe
            val companionObj = selectorType.typeSymbol.companionModule

            if (companionObj.exists) {
              val uncovered = mutable.LinkedHashSet.empty[Symbol]

              companionObj.info.decls.foreach { member =>
                if (
                  member.is(Flags.Final) && member.is(Flags.Method) && !member.is(Flags.Lazy) &&
                  member.info.resultType <:< selectorType
                ) {
                  if (member.isPublic) uncovered += member
                }
              }

              def recordMatchedEnums(pattern: Tree): Unit = pattern match {
                case Bind(_, body) => recordMatchedEnums(body)
                case Alternative(patterns) => patterns.foreach(recordMatchedEnums)
                case Ident(name) if name.toString == "_" => uncovered.clear()
                case ref: RefTree if ref.symbol.exists => uncovered -= ref.symbol
                case Literal(_) =>
                case _ => uncovered.clear()
              }

              cases.foreach {
                case CaseDef(pattern, EmptyTree, _) => recordMatchedEnums(pattern)
                case _ => uncovered.clear()
              }

              if (uncovered.nonEmpty) {
                val enumList =
                  if (uncovered.size > 1)
                    "inputs: " + uncovered.map(_.name.toString).mkString(", ")
                  else
                    "input: " + uncovered.head.name.toString
                emitReport(
                  matchTree.srcPos,
                  s"match may not be exhaustive.\nIt would fail on the following $enumList",
                )
              }
            }
          case _ =>
        }
      }
    }
  }
}
