package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import Contexts.*
import Symbols.*
import Types.*
import scala.annotation.tailrec

class BasePackage() extends CheckingRule("basePackage") {
  def performCheck(unitTree: Tree)(using Context): Unit = {
    if (ruleArgument != null) {
      val requiredPkg = ruleArgument

      object ImportSkipper {
        @tailrec
        def skipImports(trees: List[Tree]): List[Tree] = trees match {
          case (_: Import) :: tail => skipImports(tail)
          case other => other
        }
      }

      @tailrec
      def validatePackage(tree: Tree): Unit = tree match {
        case pkgDef: PackageDef
            if pkgDef.symbol.isPackageObject || pkgDef.pid.symbol.fullName.toString == requiredPkg =>
        // Valid
        case pkgDef: PackageDef =>
          val nonImports = ImportSkipper.skipImports(pkgDef.stats)
          if (nonImports.length == 1) validatePackage(nonImports.head)
          else
            emitReport(pkgDef.srcPos, s"`$requiredPkg` must be one of the base packages in this file")
        case other =>
          emitReport(other.srcPos, s"`$requiredPkg` must be one of the base packages in this file")
      }

      validatePackage(unitTree)
    }
  }
}
