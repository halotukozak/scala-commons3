package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.{tpd, untpd}
import core.*
import Contexts.*

class ImportJavaUtil() extends CheckingRule("importJavaUtil"):
  def performCheck(unitTree: tpd.Tree)(using Context): Unit =
    object ImportChecker extends tpd.TreeTraverser:
      override def traverse(tree: tpd.Tree)(using Context): Unit =
        tree match
          case tpd.Import(expr, selectors) =>
            expr match
              case ref: tpd.RefTree if ref.name.toString == "util" =>
                ref.qualifier match
                  case qual: tpd.RefTree if qual.name.toString == "java" =>
                    selectors.foreach: selector =>
                      if selector.imported.isEmpty || selector.imported.name.toString == "util" then
                        emitReport(
                          tree.srcPos,
                          "Don't import java.util: either import with rename (e.g. import java.{util => ju}) " +
                            "or use type aliases from JavaInterop (e.g. JList, JSet, etc)"
                        )
                  case _ =>
              case _ =>
            traverseChildren(tree)
          case _ => traverseChildren(tree)
    ImportChecker.traverse(unitTree)
