package com.avsystem.commons
package analyzer

import dotty.tools.dotc.Compiler
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.plugins.Plugin
import dotty.tools.dotc.reporting.Reporter
import org.scalactic.source.Position
import org.scalatest.Assertions

import scala.collection.mutable

trait AnalyzerTest { this: Assertions =>

  protected def pluginOptions: List[String] = List("AVSystemAnalyzer:+_")

  protected def compile(source: String): Reporter = {
    val ctxBase = new ContextBase {
      override protected def loadRoughPluginsList(using Context): List[Plugin] =
        new AnalyzerPlugin :: Nil
    }
    given ctx: FreshContext = ctxBase.initialCtx.fresh
    ctx.settings.Yusejavacp.update(true)
    ctx.settings.experimental.update(true)
    ctx.settings.pluginOptions.update(pluginOptions)

    val compiler = new Compiler
    val run = compiler.newRun
    run.compileFromStrings(List(source))

    ctx.reporter
  }

  def assertErrors(errors: Int, source: String)(using Position): Unit = {
    val result = compile(source)
    assert(
      result.errorCount == errors,
      s"Expected $errors errors, got ${result.errorCount}: ${result.allErrors.mkString("; ")}",
    )
  }

  def assertNoErrors(source: String)(using Position): Unit = {
    val result = compile(source)
    assert(
      !result.hasErrors,
      s"Expected no errors, got: ${result.allErrors.mkString("; ")}",
    )
  }

  def assertWarnings(warnings: Int, source: String)(using Position): Unit = {
    val result = compile(source)
    assert(
      result.warningCount == warnings,
      s"Expected $warnings warnings, got ${result.warningCount}: ${result.allWarnings.mkString("; ")}",
    )
  }

  extension (sc: StringContext) def scala(args: Any*): String = s"object TopLevel {${sc.s(args*)}}"
}
