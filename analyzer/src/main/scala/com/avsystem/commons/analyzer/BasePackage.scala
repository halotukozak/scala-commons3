package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import Contexts.*
import Symbols.*
import Types.*
import scala.annotation.tailrec

object BasePackage extends AnalyzerRule("basePackage") {
  def performCheck(unitTree: Tree)(using Context): Unit = Option(ruleArgument).foreach { requiredPkg =>

    object ImportSkipper {
      @tailrec
      def unapply(trees: List[Tree]): Option[List[Tree]] = trees match {
        case (_: Import) :: ImportSkipper(tail) => Some(tail)
        case other => Some(other)
      }
    }

    @tailrec
    def validatePackage(tree: Tree): Unit = tree match {
      case pkgDef: PackageDef if pkgDef.symbol.isPackageObject || pkgDef.pid.symbol.fullName.toString == requiredPkg =>
      // Valid
      case pkgDef @ PackageDef(_, ImportSkipper(nonImports)) =>
        if (nonImports.length == 1) validatePackage(nonImports.head)
        else emitReport(pkgDef.srcPos, s"`$requiredPkg` must be one of the base packages in this file")
      case other =>
        emitReport(other.srcPos, s"`$requiredPkg` must be one of the base packages in this file")
    }

    validatePackage(unitTree)
  }
}
