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

enum Level {
  case Off, Info, Warn, Error
}

abstract class CheckingRule(val ruleName: String, initialSeverity: Level = Level.Warn) {
  var currentSeverity: Level = initialSeverity
  var ruleArgument: String | Null = null

  def updateSeverity(newLevel: Level): Unit =
    currentSeverity = newLevel

  def updateArgument(arg: String): Unit =
    ruleArgument = arg

  protected def resolveClassType(fqn: String)(using Context): Type =
    try {
      val classSym = requiredClass(fqn)
      classSym.typeRef
    } catch {
      case _: Exception => NoType
    }

  protected def traverseAndCheck(checkFn: PartialFunction[tpd.Tree, Unit])(tree: tpd.Tree)(using Context): Unit =
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

  private def prefixMessage(msg: String): String = s"[AVS] $msg"

  protected final def emitReport(
    position: SrcPos,
    message: String,
    severity: Level = currentSeverity,
  )(using Context): Unit =
    severity match {
      case Level.Off =>
      case Level.Info => report.inform(prefixMessage(message), position)
      case Level.Warn => report.warning(prefixMessage(message), position)
      case Level.Error => report.error(prefixMessage(message), position)
    }

  def performCheck(unitTree: tpd.Tree)(using Context): Unit
}
