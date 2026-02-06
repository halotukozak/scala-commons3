package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import Contexts.*
import Symbols.*
import Types.*
import Names.*

object ThrowableObjects extends AnalyzerRule("throwableObjects", Level.Warn) {
  def performCheck(unitTree: Tree)(using Context): Unit = checkChildren(unitTree) {
    case valDef: ValDef if valDef.symbol.is(Flags.Module) =>
      val moduleType = valDef.symbol.info
      if (moduleType <:< defn.ThrowableType) {
        val fillInStackTraceMethods = moduleType.member(termName("fillInStackTrace"))
        val hasOverride = fillInStackTraceMethods.alternatives.exists { alt =>
          alt.symbol.owner != defn.ThrowableClass && alt.symbol.is(Flags.Override)
        }

        if (!hasOverride)
          emitReport(
            valDef.srcPos,
            "objects should never extend Throwable unless they have no stack trace",
          )
      }
    case _ =>
  }
}
