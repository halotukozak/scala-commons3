package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.{Symbols, Types}
import dotty.tools.dotc.core.Symbols.{NoSymbol, Symbol}
import dotty.tools.dotc.core.Types.Type

class DiscardedMonixTask(using Context) extends AnalyzerRule("discardedMonixTask") {
  private lazy val monixTaskClass: Symbol = Symbols.getClassIfDefined("monix.eval.Task")
  private lazy val monixTaskTpe: Type = monixTaskClass.info
  override def requiredSymbols: List[Symbol] = monixTaskClass :: Nil

  override def verifyBlock(tree: tpd.Block)(using Context): Unit =
    tree.stats.foreach(reportIfTask)
  override def verifyTemplate(tree: tpd.Template)(using Context): Unit = tree.body.foreach {
    case _: tpd.DefTree => ()
    case stat => reportIfTask(stat)
  }
  override def verifyWhileDo(tree: tpd.WhileDo)(using Context): Unit = reportIfTask(tree.body)
  override def verifyTry(tree: tpd.Try)(using Context): Unit = if (!tree.finalizer.isEmpty) {
    reportIfTask(tree.finalizer)
  }
  private def reportIfTask(tree: tpd.Tree)(using Context): Unit =
    if (tree.tpe <:< monixTaskTpe) {
      report(tree, "discarded monix.eval.Task value - this Task will not execute its side effects")
    }
}
