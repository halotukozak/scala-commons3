package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Symbols.{defn, NoSymbol}

import scala.annotation.tailrec

class NothingAsFunctionArgument extends AnalyzerRule("nothingAsFunctionArgument") {
  override def requiredSymbols: Seq[Symbols.Symbol] = Nil

  override def verifyApply(tree: tpd.Apply)(using Context): Unit = if (tree.fun.symbol != NoSymbol) {
    val paramInfoss = tree.fun.symbol.info.paramInfoss

    // Determine which parameter list this Apply corresponds to by counting
    // how many Apply layers are nested in `fun` (depth 0 = first param list).
    @tailrec
    def applyLayers(t: tpd.Tree, applyDepth: Int): Int = t match {
      case tpd.Apply(fun, args) => applyLayers(fun, applyDepth + 1)
      case _ => applyDepth
    }

    val applyDepth = applyLayers(tree.fun, 0)

    if (paramInfoss.length > applyDepth) {
      for {
        (arg, paramTpe) <- tree.args.zip(paramInfoss(applyDepth))
        if defn.isFunctionType(paramTpe) && arg.tpe <:< defn.NothingType
      } {
        report(
          arg,
          s"""|A value of type `Nothing` was passed where a function is expected.
              |If you intended to throw an exception, wrap it in a function literal (e.g. `_ => throw ex` instead of `throw ex`).
              |If you are using a mocking framework, provide a mock function with the correct type (e.g. `any[${paramTpe.show}]`).
              |""".stripMargin,
        )
      }
    }
  }
}
