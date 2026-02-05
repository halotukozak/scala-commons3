package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd
import core.*
import Contexts.*
import Symbols.*
import Types.*
import Constants.*
import Decorators.*

class VarargsAtLeast() extends CheckingRule("varargsAtLeast"):
  private def extractAtLeastAnnotation(using Context): Type =
    resolveClassType("com.avsystem.commons.annotation.atLeast")

  def performCheck(unitTree: tpd.Tree)(using Context): Unit =
    val atLeastAnnotType = extractAtLeastAnnotation
    if atLeastAnnotType == NoType then return

    object VarargsChecker extends tpd.TreeTraverser:
      override def traverse(tree: tpd.Tree)(using Context): Unit =
        tree match
          case app @ tpd.Apply(fn, args) =>
            val methodSym = fn.symbol
            if methodSym.exists && methodSym.is(Flags.Method) then
              val params = methodSym.info.paramInfoss.flatten
              if params.nonEmpty && params.last.isRepeatedParam then
                val lastIsVararg = args.lastOption.exists:
                  case tpd.Typed(_, tpt) => 
                    tpt match
                      case tpd.Ident(name) => name.toString.contains("*")
                      case _ => false
                  case _ => false
                
                if !lastIsVararg then
                  val lastParamSym = methodSym.paramSymss.flatten.last
                  val requiredCount = lastParamSym.annotations.find(ann => ann.symbol.typeRef <:< atLeastAnnotType).map: annot =>
                    annot.tree match
                      case tpd.Apply(_, List(tpd.Literal(Constant(n: Int)))) => n
                      case _ => 0
                  .getOrElse(0)
                  
                  val providedCount = args.length - params.length + 1
                  
                  if providedCount < requiredCount then
                    emitReport(
                      app.srcPos,
                      s"This method requires at least $requiredCount arguments for its repeated parameter, $providedCount passed."
                    )
            traverseChildren(tree)
          case _ => traverseChildren(tree)
    VarargsChecker.traverse(unitTree)
