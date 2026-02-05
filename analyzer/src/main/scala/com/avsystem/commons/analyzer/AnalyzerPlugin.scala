package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import plugins.*
import core.*
import Contexts.*
import Phases.Phase
import Decorators.*

class AnalyzerPlugin extends StandardPlugin:
  override val name = "AVSystemAnalyzer"
  override val description = "AVSystem custom Scala static analyzer"

  def init(options: List[String], phases: List[List[Phase]])(using Context): List[List[Phase]] =
    val ruleInstances = createAllRuleInstances()
    val ruleMapping = ruleInstances.map(r => r.ruleName -> r).toMap
    
    options.foreach: optionString =>
      if optionString.startsWith("requireJDK=") then
        val jdkPattern = optionString.drop(11)
        val currentJavaVer = System.getProperty("java.version", "")
        if !currentJavaVer.matches(jdkPattern) then
          report.error(s"This project must be compiled on JDK version that matches $jdkPattern but got $currentJavaVer")
      else
        val severityLevel = optionString.charAt(0) match
          case '-' => SeverityLevel.Disabled
          case '*' => SeverityLevel.Information
          case '+' => SeverityLevel.Fatal
          case _ => SeverityLevel.Warning
        
        val nameWithArg = if severityLevel != SeverityLevel.Warning then optionString.drop(1) else optionString
        
        if nameWithArg == "_" then
          ruleInstances.foreach(_.updateSeverity(severityLevel))
        else
          val parts = nameWithArg.split(":", 2)
          val ruleName = parts(0)
          val ruleArg = if parts.length > 1 then parts(1) else null
          
          ruleMapping.get(ruleName) match
            case Some(ruleInstance) =>
              ruleInstance.updateSeverity(severityLevel)
              ruleInstance.updateArgument(ruleArg)
            case None =>
              report.error(s"Unrecognized AVS analyzer rule: $ruleName")
    
    val analyzerPhase = AnalyzerPhaseImpl(ruleInstances)
    phases.map: phaseList =>
      phaseList :+ analyzerPhase

  private def createAllRuleInstances(): List[CheckingRule] = List(
    ImportJavaUtil(),
    VarargsAtLeast(),
    CheckMacroPrivate(),
    ExplicitGenerics(),
    ValueEnumExhaustiveMatch(),
    ShowAst(),
    FindUsages(),
    CheckBincompat(),
    Any2StringAdd(),
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
    ImplicitFunctionParams()
  )

class AnalyzerPhaseImpl(rulesList: List[CheckingRule]) extends PluginPhase:
  val phaseName = "avsAnalyze"
  override val runsAfter = Set("typer")
  override val runsBefore = Set("patmat")

  override def transformUnit(unitTree: ast.tpd.Tree)(using ctx: Context): ast.tpd.Tree =
    rulesList.foreach: currentRule =>
      if currentRule.currentSeverity != SeverityLevel.Disabled then
        currentRule.performCheck(unitTree)
    unitTree
