package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import plugins.*
import core.*
import Contexts.*
import Phases.Phase
import Decorators.*

class AnalyzerPlugin extends StandardPlugin {
  override val name = "AVSystemAnalyzer"
  override val description = "AVSystem custom Scala static analyzer"

  def init(options: List[String], phases: List[List[Phase]])(using Context): List[List[Phase]] = {
    val ruleInstances = createAllRuleInstances()
    val ruleMapping = ruleInstances.map(r => r.ruleName -> r).toMap

    options.foreach { option =>
      if (option.startsWith("requireJDK=")) {
        val jdkPattern = option.drop(11)
        val currentJavaVer = System.getProperty("java.version", "")
        if (!currentJavaVer.matches(jdkPattern)) {
          report.error(s"This project must be compiled on JDK version that matches $jdkPattern but got $currentJavaVer")
        }
      } else {
        val severityLevel = option.charAt(0) match {
          case '-' => Level.Off
          case '*' => Level.Info
          case '+' => Level.Error
          case _ => Level.Warn
        }

        val nameWithArg = if (severityLevel != Level.Warn) option.drop(1) else option

        if (nameWithArg == "_") {
          ruleInstances.foreach(_.updateSeverity(severityLevel))
        } else {
          val parts = nameWithArg.split(":", 2)
          val ruleName = parts(0)
          val ruleArg = if (parts.length > 1) parts(1) else null

          ruleMapping.get(ruleName) match {
            case Some(ruleInstance) =>
              ruleInstance.updateSeverity(severityLevel)
              ruleInstance.updateArgument(ruleArg)
            case None =>
              report.error(s"Unrecognized AVS analyzer rule: $ruleName")
          }
        }
      }
    }

    val analyzerPhase = AnalyzerPhaseImpl(ruleInstances)
    // Insert the analyzer phase as a separate phase group
    // The runsAfter/runsBefore constraints will handle the ordering
    phases :+ List(analyzerPhase)
  }

  private def createAllRuleInstances(): List[AnalyzerRule] = List(
    ImportJavaUtil(),
    VarargsAtLeast(),
    CheckMacroPrivate(),
    ExplicitGenerics(),
    ValueEnumExhaustiveMatch(),
    ShowAst(),
    FindUsages(),
    CheckBincompat(),
    ThrowableObjects(),
    DiscardedMonixTask(),
    NothingAsFunctionArgument(),
    ConstantDeclarations(),
    BasePackage(),
    ImplicitValueClasses(),
    FinalValueClasses(),
    FinalCaseClasses(),
    ImplicitParamDefaults(),
    CatchThrowable(),
    ImplicitFunctionParams(),
  )
}

class AnalyzerPhaseImpl(rulesList: List[AnalyzerRule]) extends PluginPhase {
  val phaseName = "avsAnalyze"
  override val runsAfter = Set("typer")
  override val runsBefore = Set("patmat")

  override def transformUnit(unitTree: ast.tpd.Tree)(using ctx: Context): ast.tpd.Tree = {
    // Access the untyped tree for rules that need it (like ImportJavaUtil)
    val untpdTree = ctx.compilationUnit.untpdTree

    rulesList.foreach { currentRule =>
      if (currentRule.currentSeverity != Level.Off) {
        // For ImportJavaUtil, pass the untyped tree
        if (currentRule.ruleName == "importJavaUtil") {
          currentRule.asInstanceOf[ImportJavaUtil].performCheckOnUntpd(untpdTree)
        } else
          currentRule.performCheck(unitTree)
      }
    }
    unitTree
  }
}
