package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Symbols.*

import scala.annotation.tailrec

class BasePackage(using Context) extends AnalyzerRuleOnTyped("basePackage") {
  
  object SkipImports {
    @tailrec def unapply(trees: List[Tree]): Option[List[Tree]] = trees match {
      case (_: Import) :: tail => SkipImports.unapply(tail)
      case other => Some(other)
    }
  }
  
  def analyze(unitTree: Tree)(using Context): Unit = Option(ruleArgument).foreach { requiredPkg =>
    @tailrec def validate(tree: Tree): Unit = tree match {
      case pkgDef: PackageDef if pkgDef.symbol.isPackageObject || pkgDef.pid.symbol.fullName.toString == requiredPkg =>
      // Valid
      case pkgDef @ PackageDef(_, SkipImports(nonImports)) =>
        if (nonImports.length == 1) validate(nonImports.head)
        else emitReport(pkgDef.srcPos, s"`$requiredPkg` must be one of the base packages in this file")
      case other =>
        emitReport(other.srcPos, s"`$requiredPkg` must be one of the base packages in this file")
    }

    validate(unitTree)
  }
}
