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

enum SeverityLevel {
  case Disabled, Information, Warning, Fatal
}

abstract class CheckingRule(val ruleName: String, initialSeverity: SeverityLevel = SeverityLevel.Warning) {
  var currentSeverity: SeverityLevel = initialSeverity
  var ruleArgument: String | Null = null

  def updateSeverity(newLevel: SeverityLevel): Unit =
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

  private def prefixMessage(msg: String): String = s"[AVS] $msg"

  protected final def emitReport(
    position: SrcPos,
    message: String,
    severity: SeverityLevel = currentSeverity,
  )(using Context,
  ): Unit =
    severity match {
      case SeverityLevel.Disabled =>
      case SeverityLevel.Information => report.inform(prefixMessage(message), position)
      case SeverityLevel.Warning => report.warning(prefixMessage(message), position)
      case SeverityLevel.Fatal => report.error(prefixMessage(message), position)
    }

  def performCheck(unitTree: tpd.Tree)(using Context): Unit
}
