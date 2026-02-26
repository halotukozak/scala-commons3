package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.{Flags, Symbols}
import dotty.tools.dotc.core.Symbols.{NoSymbol, Symbol}

class CheckBincompat(using Context) extends AnalyzerRule("bincompat") {

  private lazy val bincompatAnnotClass: Symbol =
    Symbols.getClassIfDefined("com.avsystem.commons.annotation.bincompat")

  override def requiredSymbols: List[Symbol] = bincompatAnnotClass :: Nil

  override def verifyIdent(tree: tpd.Ident)(using Context): Unit = checkTree(tree)

  override def verifySelect(tree: tpd.Select)(using Context): Unit = checkTree(tree)

  override def verifyNew(tree: tpd.New)(using Context): Unit = checkTree(tree)

  private def checkTree(tree: tpd.Tree)(using Context): Unit = if (tree.symbol != NoSymbol) {
    val sym = tree.symbol
    if (sym.hasAnnotation(bincompatAnnotClass) && !isDefinitionSite(sym, tree)) {
      report(
        tree,
        "Symbols annotated as @bincompat exist only for binary compatibility and should not be used directly",
      )
    }
  }

  /**
   * Check if the tree is at the definition site of the symbol, not a usage site.
   * In Scala 3, the MiniPhase may see internal references within a TypeDef/ValDef
   * that correspond to the definition itself (e.g., module class references inside
   * an object definition). We detect this by checking whether:
   * 1. The symbol's definition span contains the tree's span, OR
   * 2. The companion module/class definition span contains the tree's span
   *    (handles `object X` where the module class Ident appears inside the ValDef).
   */
  private def isDefinitionSite(sym: Symbol, tree: tpd.Tree)(using Context): Boolean = {
    val treeSpan = tree.span

    treeSpan.exists && {
      def spanContains(s: Symbol): Boolean = {
        val sp = s.span
        sp.exists && sp.contains(treeSpan)
      }

      spanContains(sym) || {
        // For module classes, also check the module val (companion module)
        // For module vals, also check the module class
        val companion =
          if (sym.is(Flags.ModuleClass)) sym.companionModule
          else if (sym.is(Flags.Module)) sym.companionClass
          else NoSymbol
        companion != NoSymbol && spanContains(companion)
      }
    }
  }
}
