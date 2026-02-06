package com.avsystem.commons
package analyzer

import dotty.tools.dotc.{Main, Compiler, Driver}
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.reporting.*

import java.io.{File, PrintWriter}
import java.nio.file.Files
import scala.collection.mutable.ListBuffer
import org.scalactic.source.Position
import org.scalatest.Assertions

trait AnalyzerTest extends Assertions {
  
  private val diagnostics = ListBuffer.empty[(Int, String)]
  
  private def compileSource(sourceCode: String, pluginOpts: List[String] = List("-_")): Unit = {
    diagnostics.clear()
    
    val tempDir = Files.createTempDirectory("analyzer-test").toFile
    val srcFile = new File(tempDir, "test.scala")
    val pw = new PrintWriter(srcFile)
    pw.write(sourceCode)
    pw.close()
    
    val classPathValue = System.getProperty("java.class.path")
    
    class CustomDriver extends Driver {
      override def newCompiler(using Context): Compiler =
        new Compiler {
          override lazy val phases: List[List[Phase]] = {
            val analyzerPlugin = new AnalyzerPlugin
            val basePhases = super.phases
            analyzerPlugin.init(pluginOpts, basePhases)
          }
        }
    }
    
    val customDriver = new CustomDriver
    
    val reporterInstance = new Reporter {
      def doReport(dia: Diagnostic)(using Context): Unit =
        diagnostics += ((dia.level, dia.msg.message))
    }

    val opts = Array(
      "-classpath", classPathValue,
      "-d", tempDir.getAbsolutePath,
      srcFile.getAbsolutePath
    )
    
    try
      customDriver.process(opts, reporterInstance)
    finally {
      srcFile.delete()
      tempDir.delete()
    }
  }

  def compile(source: String): Unit =
    compileSource(source)

  def assertErrors(count: Int, source: String, pluginOpts: List[String] = List("-_"))(using Position): Unit = {
    compileSource(source, pluginOpts)
    val errCount = diagnostics.count(_._1 == 2)
    assert(errCount == count, s"Expected $count errors but got $errCount. Diagnostics: ${diagnostics.map(_._2).mkString(", ")}")
  }

  def assertNoErrors(source: String, pluginOpts: List[String] = List("-_"))(using Position): Unit = {
    compileSource(source, pluginOpts)
    val errCount = diagnostics.count(_._1 == 2)
    assert(errCount == 0, s"Expected no errors but got $errCount. Diagnostics: ${diagnostics.map(_._2).mkString(", ")}")
  }

  extension (sc: StringContext) {
    def scala(args: Any*): String = s"object TopLevel {${sc.s(args*)}}"
  }

  def onlyRule(name: String): List[String] = List("-_", s"+$name")
  def onlyRuleWithArg(name: String, arg: String): List[String] = List("-_", s"+$name:$arg")
}
