package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Symbols.{NoSymbol, Symbol}

import scala.collection.mutable

//imo it can be removed
class ValueEnumExhaustiveMatch(using Context) extends AnalyzerRule("valueEnumExhaustiveMatch") {
  private lazy val valueEnumClass: Symbol =
    Symbols.getClassIfDefined("com.avsystem.commons.misc.ValueEnum")

  private lazy val valueEnumCompanionClass: Symbol =
    Symbols.getClassIfDefined("com.avsystem.commons.misc.ValueEnumCompanion")

  override def requiredSymbols: Seq[Symbol] = valueEnumClass :: valueEnumCompanionClass :: Nil

  override def verifyMatch(tree: tpd.Match)(using Context): Unit =
    if (tree.selector.tpe.widenDealias.classSymbol.derivesFrom(valueEnumClass)) checkExhaustiveness(tree)

  private def checkExhaustiveness(tree: tpd.Match)(using Context): Unit = {
    val selectorTpe = tree.selector.tpe.widenDealias
    val classSym = selectorTpe.classSymbol
    val companion = classSym.companionModule

    if (companion != NoSymbol && companion.info.derivesFrom(valueEnumCompanionClass)) {
      // Collect expected enum values: final, non-lazy, public vals of the selector type
      val all = companion.info.decls.iterator.filter { s =>
        s.isTerm && s.is(Flags.Final, butNot = Flags.Lazy) && s.isPublic && s.info.finalResultType <:< selectorTpe
      }.toSet

      // Analyze each case to remove matched values
      val unmatched = tree.cases.foldLeft(all) {
        case (acc, cd: tpd.CaseDef) if cd.guard.isEmpty => findMatchedEnums(cd.pat, acc)
        case _ => Set.empty // Guard present or unusual case -- assume covered
      }

      if (unmatched.nonEmpty) {
        val what =
          if (unmatched.size > 1) "inputs: " + unmatched.iterator.map(_.name.toString).mkString(", ")
          else "input: " + unmatched.head.name.toString
        report(tree, "match may not be exhaustive.\nIt would fail on the following " + what)
      }
    }
  }

  private def findMatchedEnums(
    pattern: tpd.Tree,
    unmatched: Set[Symbol],
  )(using Context,
  ): Set[Symbol] = pattern match {
    case tpd.Bind(_, body) => findMatchedEnums(body, unmatched)
    case tpd.Alternative(pats) => pats.foldLeft(unmatched)((pat, acc) => findMatchedEnums(acc, pat))
    case tpd.Ident(nme.WILDCARD) => Set.empty
    case _: tpd.Ident | _: tpd.Select => unmatched - pattern.symbol
    case _: tpd.Literal => unmatched // ignore literal patterns (e.g. null)
    case _ => Set.empty // unknown pattern, assume exhaustive
  }
}
