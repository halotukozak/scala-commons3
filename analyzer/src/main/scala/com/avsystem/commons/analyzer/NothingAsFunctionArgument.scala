package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import Contexts.*
import Symbols.*
import Types.*

class NothingAsFunctionArgument(using Context) extends AnalyzerRuleOnTyped("nothingAsFunctionArgument") {
  def analyze(unitTree: Tree)(using Context): Unit = checkChildren(unitTree) {
    case app @ Apply(fn, args) =>
      val methodSym = fn.symbol
      if (methodSym.exists && methodSym.is(Flags.Method)) {
        val params = methodSym.info.paramInfoss.flatten
        args.zip(params).foreach {
          case (arg, param) if defn.isFunctionType(param) && arg.tpe <:< defn.NothingType =>
            emitReport(
              arg.srcPos,
              s"""
                 |A value of type `Nothing` was passed where a function is expected.
                 |If you intended to throw an exception, wrap it in a function literal (e.g. `_ => throw ex` instead of `throw ex`).
                 |If you are using a mocking framework, provide a mock function with the correct type (e.g. `any[${param.show}]`).
                 |""".stripMargin,
            )
          case _ =>
        }
      }
    case _ =>
  }
}
