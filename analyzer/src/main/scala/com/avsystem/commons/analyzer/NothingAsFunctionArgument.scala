package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd
import core.*
import Contexts.*
import Symbols.*
import Types.*

class NothingAsFunctionArgument() extends CheckingRule("nothingAsFunctionArgument"):
  def performCheck(unitTree: tpd.Tree)(using Context): Unit =
    object NothingChecker extends tpd.TreeTraverser:
      override def traverse(tree: tpd.Tree)(using Context): Unit =
        tree match
          case app @ tpd.Apply(fn, args) =>
            val methodSym = fn.symbol
            if methodSym.exists && methodSym.is(Flags.Method) then
              val params = methodSym.info.paramInfoss.flatten
              args.zip(params).foreach:
                case (arg, paramInfo) if defn.isFunctionType(paramInfo) && arg.tpe <:< defn.NothingType =>
                  emitReport(
                    arg.srcPos,
                    s"""
                       |A value of type `Nothing` was passed where a function is expected.
                       |If you intended to throw an exception, wrap it in a function literal (e.g. `_ => throw ex` instead of `throw ex`).
                       |If you are using a mocking framework, provide a mock function with the correct type (e.g. `any[${paramInfo.show}]`).
                       |""".stripMargin
                  )
                case _ =>
            traverseChildren(tree)
          case _ => traverseChildren(tree)
    NothingChecker.traverse(unitTree)
