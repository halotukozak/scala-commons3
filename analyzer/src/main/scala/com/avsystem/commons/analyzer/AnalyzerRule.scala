package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd
import core.*
import Contexts.*
import Symbols.*
import Types.*
import Constants.*
import util.{SourcePosition, SrcPos}
import reporting.Message

import java.io.{PrintWriter, StringWriter}
import scala.util.control.NonFatal
import dotty.tools.dotc.ast.untpd

enum Level {
  case Off, Info, Warn, Error
}

sealed trait AnalyzerRule(using Context)(val ruleName: String, defaultLevel: Level = Level.Warn) {
  var level: Level = defaultLevel
  var argument: String | Null = null

  protected def resolveClassType(fqn: String): Option[TypeRef] = try {
    val classSym = requiredClass(fqn)
    Some(classSym.typeRef).filter(_ != NoType)
  } catch {
    case NonFatal(_) => None
  }

  private def prefixMessage(msg: String): String = s"[AVS] $msg"

  protected final def emitReport(
    position: SrcPos,
    message: String,
    severity: Level = level,
  )(using Context,
  ): Unit =
    severity match {
      case Level.Off =>
      case Level.Info => report.inform(prefixMessage(message), position)
      case Level.Warn => report.warning(prefixMessage(message), position)
      case Level.Error => report.error(prefixMessage(message), position)
    }

}

abstract class AnalyzerRuleOnUntyped(using Context)(ruleName: String, initialSeverity: Level = Level.Warn)
  extends AnalyzerRule(ruleName, initialSeverity) {
  def analyze(unitTree: untpd.Tree)(using Context): Unit
}

abstract class AnalyzerRuleOnTyped(using Context)(ruleName: String, initialSeverity: Level = Level.Warn)
  extends AnalyzerRule(ruleName, initialSeverity) {
  def analyze(unitTree: tpd.Tree)(using Context): Unit

  protected def traverseAndCheck(checkFn: PartialFunction[tpd.Tree, Unit])(tree: tpd.Tree): Unit =
    try
      checkFn.applyOrElse(tree, (_: tpd.Tree) => ())
    catch {
      case NonFatal(ex) =>
        val stringWriter = new StringWriter
        ex.printStackTrace(new PrintWriter(stringWriter))
        report.error(s"Analyzer rule $this failed: ${stringWriter.toString}", tree.srcPos)
    }

  protected def checkChildren(tree: tpd.Tree)(checkFn: tpd.Tree => Unit)(using Context): Unit = {
    object Checker extends tpd.TreeTraverser {
      override def traverse(t: tpd.Tree)(using Context): Unit = {
        checkFn(t)
        traverseChildren(t)
      }
    }
    Checker.traverse(tree)
  }
}
