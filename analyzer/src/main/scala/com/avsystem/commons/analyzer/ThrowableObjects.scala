package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import Contexts.*
import Symbols.*
import Types.*
import Names.*

class ThrowableObjects() extends AnalyzerRule("throwableObjects", Level.Warn) {
  def performCheck(unitTree: Tree)(using Context): Unit = {
    val throwableType = defn.ThrowableType
    val throwableSym = defn.ThrowableClass

    checkChildren(unitTree) { tree =>
      tree match {
        case valDef: ValDef if valDef.symbol.is(Flags.Module) =>
          val moduleType = valDef.symbol.info
          if (moduleType <:< throwableType) {
            val fillInStackTraceMethods = moduleType.member(termName("fillInStackTrace"))
            val hasOverride = fillInStackTraceMethods.alternatives.exists { alt =>
              alt.symbol.owner != throwableSym && alt.symbol.is(Flags.Override)
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
  }
}
