package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.{tpd, untpd}
import core.*
import Contexts.*

class ImportJavaUtil() extends CheckingRule("importJavaUtil"):
  def performCheck(unitTree: tpd.Tree)(using Context): Unit =
    // This method is not used; use performCheckOnUntpd instead
    ()
  
  def performCheckOnUntpd(unitTree: untpd.Tree)(using Context): Unit =
    def checkImport(tree: untpd.Tree): Unit = tree match
      case untpd.Import(expr, selectors) =>
        expr match
          // Match: import java.util (expr is just "java", selector imports "util")
          case untpd.Ident(name) if name.toString == "java" =>
            selectors.foreach: selector =>
              selector match
                case untpd.ImportSelector(imported: untpd.Ident, renamed, _) =>
                  if imported.name.toString == "util" && renamed.isEmpty then
                    emitReport(
                      tree.srcPos,
                      "Don't import java.util: either import with rename (e.g. import java.{util => ju}) " +
                        "or use type aliases from JavaInterop (e.g. JList, JSet, etc)"
                    )
          // Match: import java.util.xyz (expr is "java.util", selecting from it)
          case untpd.Select(qual: untpd.Ident, name) if name.toString == "util" && qual.name.toString == "java" =>
            selectors.foreach: selector =>
              selector match
                case untpd.ImportSelector(imported, renamed, _) =>
                  if renamed.isEmpty then
                    emitReport(
                      tree.srcPos,
                      "Don't import java.util: either import with rename (e.g. import java.{util => ju}) " +
                        "or use type aliases from JavaInterop (e.g. JList, JSet, etc)"
                    )
          case _ =>
      case _ =>
    
    def traverse(tree: untpd.Tree): Unit =
      checkImport(tree)
      tree match
        case untpd.PackageDef(_, stats) => stats.foreach(traverse)
        case untpd.ModuleDef(_, impl) => traverse(impl)
        case untpd.TypeDef(_, impl) => traverse(impl)
        case templ: untpd.Template => 
          var i = 0
          while i < templ.body.length do
            traverse(templ.body(i))
            i += 1
        case _ => // Don't traverse other nodes
    
    traverse(unitTree)
