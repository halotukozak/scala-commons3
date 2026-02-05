package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd
import core.*
import Contexts.*
import Symbols.*
import Types.*
import scala.collection.mutable

class ValueEnumExhaustiveMatch() extends CheckingRule("valueEnumExhaustiveMatch"):
  private def extractValueEnumTypes(using Context): (Type, Symbol) =
    val valueEnumType = resolveClassType("com.avsystem.commons.misc.ValueEnum")
    val companionType = resolveClassType("com.avsystem.commons.misc.ValueEnumCompanion")
    val companionSym = if companionType != NoType then companionType.typeSymbol else NoSymbol
    (valueEnumType, companionSym)

  def performCheck(unitTree: tpd.Tree)(using Context): Unit =
    val (valueEnumType, companionSym) = extractValueEnumTypes
    if valueEnumType == NoType || !companionSym.exists then return

    object MatchChecker extends tpd.TreeTraverser:
      override def traverse(tree: tpd.Tree)(using Context): Unit =
        tree match
          case matchTree @ tpd.Match(selector, cases) if selector.tpe <:< valueEnumType =>
            val selectorType = selector.tpe
            val companionObj = selectorType.typeSymbol.companionModule
            
            if companionObj.exists then
              val uncovered = mutable.LinkedHashSet.empty[Symbol]
              
              companionObj.info.decls.foreach: member =>
                if member.is(Flags.Final) && member.is(Flags.Method) && 
                   !member.is(Flags.Lazy) && member.info.resultType <:< selectorType then
                  if member.isPublic then uncovered += member
              
              def recordMatchedEnums(pattern: tpd.Tree): Unit = pattern match
                case tpd.Bind(_, body) => recordMatchedEnums(body)
                case tpd.Alternative(patterns) => patterns.foreach(recordMatchedEnums)
                case tpd.Ident(name) if name.toString == "_" => uncovered.clear()
                case ref: tpd.RefTree if ref.symbol.exists => uncovered -= ref.symbol
                case tpd.Literal(_) =>
                case _ => uncovered.clear()
              
              cases.foreach:
                case tpd.CaseDef(pattern, tpd.EmptyTree, _) => recordMatchedEnums(pattern)
                case _ => uncovered.clear()
              
              if uncovered.nonEmpty then
                val enumList = if uncovered.size > 1 then
                  "inputs: " + uncovered.map(_.name.toString).mkString(", ")
                else
                  "input: " + uncovered.head.name.toString
                emitReport(
                  matchTree.srcPos,
                  s"match may not be exhaustive.\nIt would fail on the following $enumList"
                )
            traverseChildren(tree)
          case _ => traverseChildren(tree)
    MatchChecker.traverse(unitTree)
