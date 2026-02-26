package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Symbols.Symbol

import scala.annotation.tailrec

class BasePackage(
  level: Level = Level.Warn,
  argument: Option[String] = None,
)(using Context) extends AnalyzerRule("basePackage", level, argument) {

  def withConfig(level: Level, argument: Option[String]): AnalyzerRule =
    new BasePackage(level, argument)
  private lazy val requiredBasePackage = argument.map(Symbols.requiredPackage)

  override def transformUnit(tree: tpd.Tree)(using Context): tpd.Tree = {
    println(s"=== transformUnit START ===")
    dumpTree(tree)
    println(s"=== transformUnit END ===")
    requiredBasePackage.foreach(validate(tree, _))
    tree
  }

  private def dumpTree(tree: tpd.Tree, indent: String = "")(using Context): Unit = tree match {
    case pkg: tpd.PackageDef =>
      println(s"${indent}PackageDef(pid=${pkg.pid.show}, pid.sym=${pkg.pid.symbol}, stats.size=${pkg.stats.size})")
      pkg.stats.foreach(s => dumpTree(s, indent + "  "))
    case _ =>
      println(s"${indent}${tree.getClass.getSimpleName}(sym=${tree.symbol})")
  }

  @tailrec
  private def validate(tree: tpd.Tree, required: Symbol)(using Context): Unit = tree match {
    case pkg: tpd.PackageDef if pkg.pid.symbol == required =>
      // Found the required base package -- validation passes
      println(s"DEBUG MATCHED: pid.symbol=${pkg.pid.symbol} == required=$required")
      ()
    case pkg: tpd.PackageDef =>
      // DEBUG
      val pidSym = pkg.pid.symbol
      println(s"DEBUG NOMATCH: pid.symbol=$pidSym pid.show=${pkg.pid.show} required=$required")
      val mc = pidSym.moduleClass
      println(s"  moduleClass.info.decls=${mc.info.decls.toList}")
      // Check if the pid's qualifier contains required
      pkg.pid match {
        case sel: tpd.Select =>
          val qSym = sel.qualifier.symbol
          println(s"  qualifier.symbol=$qSym (== required? ${qSym == required})")
          // Check if pidSym existed as a known member of required (pre-existing package)
          val memberInRequired = required.info.member(pidSym.name)
          println(s"  required.info.member(${pidSym.name})=$memberInRequired exists=${memberInRequired.exists}")
        case _ =>
      }
      // For dotted package names like `package com.avsystem.commons`, check if required
      // is a direct qualifier in the pid tree (e.g. `avsystem` in `com.avsystem.commons`).
      if (pidHasDirectQualifier(pkg.pid, required)) {
        () // Required base found as direct qualifier
      } else {
        // Skip imports, recurse into the single remaining stat (if exactly one)
        val nonImports = pkg.stats.filterNot(_.isInstanceOf[tpd.Import])
        nonImports match {
          case stat :: Nil => validate(stat, required)
          case _ =>
            report(tree, s"`$required` must be one of the base packages in this file")
        }
      }
    case _ =>
      report(tree, s"`$required` must be one of the base packages in this file")
  }

  /** Check if the DIRECT qualifier (parent) of the pid matches the required package. */
  private def pidHasDirectQualifier(pid: tpd.Tree, required: Symbol)(using Context): Boolean = pid match {
    case sel: tpd.Select => sel.qualifier.symbol == required
    case _ => false
  }
}
