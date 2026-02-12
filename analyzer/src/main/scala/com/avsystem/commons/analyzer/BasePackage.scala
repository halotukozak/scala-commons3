package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd.{PackageDef, *}
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags.Package
import dotty.tools.dotc.core.Symbols.*

import scala.annotation.tailrec

class BasePackage(using Context) extends AnalyzerRuleOnTyped("basePackage") {

  private object SkipImports {
    @tailrec def unapply(trees: List[Tree]): Some[List[Tree]] = trees match {
      case Import(_, _) :: tail => SkipImports.unapply(tail)
      case stats => Some(stats)
    }
  }

  def analyze(unitTree: Tree)(using Context): Unit = Option(argument).foreach { requiredPkg =>
    @tailrec def validate(tree: Tree): Unit = tree match {
      case PackageDef(pid, _) if pid.symbol.is(Package) || pid.symbol.fullName.toString == requiredPkg =>
      case PackageDef(_, SkipImports(List(stat))) => validate(stat)
      case other => emitReport(other.srcPos, s"`$requiredPkg` must be one of the base packages in this file")
    }

    validate(unitTree)
  }
}
