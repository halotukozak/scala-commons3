package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Symbols.{NoSymbol, Symbol}

class CheckMacroPrivate(using Context) extends AnalyzerRule("macroPrivate") {
  private lazy val macroPrivateAnnotClass: Symbol =
    Symbols.getClassIfDefined("com.avsystem.commons.annotation.macroPrivate")

  override def requiredSymbols: List[Symbol] = macroPrivateAnnotClass :: Nil

  override def verifyIdent(tree: tpd.Ident)(using Context): Unit =
    checkMacroPrivate(tree)

  override def verifySelect(tree: tpd.Select)(using Context): Unit =
    checkMacroPrivate(tree)

  private def checkMacroPrivate(tree: tpd.Tree)(using ctx: Context): Unit = if (tree.symbol != NoSymbol) {
    // Check if the referenced symbol (or any overridden version) has @macroPrivate
    val hasMacroPrivate = tree.symbol.hasOrInheritsAnnotation(macroPrivateAnnotClass)

    // Filter out definition sites: if the tree span is within the symbol's own definition span,
    // this is the definition itself, not a usage.
    if (hasMacroPrivate && !isDefinitionSite(tree.symbol, tree)) {
      // Check if the usage is inside an inline method body (Scala 3 equivalent of macroExpandee)
      val insideInline = ctx.owner.ownersIterator.exists(_.isInlineMethod)
      if (!insideInline) {
        report(tree, s"${tree.symbol.name} is @macroPrivate and can only be used in inline (macro) code")
      }
    }
  }

  /**
   * Check if the tree is at the definition site of the symbol, not a usage site.
   * Prevents false positives when the MiniPhase visits internal references within
   * the definition itself.
   */
  private def isDefinitionSite(sym: Symbol, tree: tpd.Tree)(using Context): Boolean = tree.span.exists && {
    val symSpan = sym.span
    symSpan.exists && symSpan.contains(tree.span)
  }
}
