package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Symbols.Symbol

import scala.annotation.tailrec

class BasePackage(using Context) extends AnalyzerRule("basePackage") {

  private lazy val requiredBasePackage = argument.map(Symbols.requiredPackage)

  override def requiredSymbols: List[Symbol] = requiredBasePackage.toList

  override def verifyUnit(tree: tpd.Tree)(using Context): Unit = {
    requiredBasePackage.foreach(validate(tree, _))
  }

  @tailrec
  private def validate(tree: tpd.Tree, required: Symbol)(using Context): Unit = tree match {
    case pkg: tpd.PackageDef if pkg.pid.symbol == required =>
    // Found the required base package -- validation passes
    case pkg: tpd.PackageDef =>

      // For dotted package names like `package com.avsystem.commons`, check if required
      // is a direct qualifier in the pid tree (e.g. `avsystem` in `com.avsystem.commons`).
      pkg.pid match {
        case sel: tpd.Select if sel.qualifier.symbol == required =>
          // Skip imports, recurse into the single remaining stat (if exactly one)
          val nonImports = pkg.stats.filterNot(_.isInstanceOf[tpd.Import])
          nonImports match {
            case stat :: Nil => validate(stat, required)
            case _ => report(tree, s"`$required` must be one of the base packages in this file")
          }
        case _ =>
      }
    case _ =>
      report(tree, s"`$required` must be one of the base packages in this file")
  }

}
