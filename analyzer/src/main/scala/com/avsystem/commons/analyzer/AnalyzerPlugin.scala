package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.plugins.*

class AnalyzerPlugin extends StandardPlugin {
  override val name = "AVSystemAnalyzer"
  override val description = "AVSystem custom Scala static analyzer"

  override def initialize(options: List[String])(using Context): List[PluginPhase] = {
    val rules = createRules
    val rulesByName = rules.map(r => r.ruleName -> r).toMap

    options.foreach { option =>
      if (option.startsWith("requireJDK=")) {
        val jdkVersionRegex = option.substring(option.indexOf('=') + 1)
        val javaVersion = System.getProperty("java.version", "")
        if (!javaVersion.matches(jdkVersionRegex)) {
          report.error(
            s"This project must be compiled on JDK version that matches $jdkVersionRegex but got $javaVersion",
          )
        }
      } else {
        val level = option.charAt(0) match {
          case '-' => Level.Off
          case '*' => Level.Info
          case '+' => Level.Error
          case _ => Level.Warn
        }

        val nameArg = if (level != Level.Warn) option.drop(1) else option

        if (nameArg == "_") {
          rules.foreach(_.level = level)
        } else {
          val parts = nameArg.split(":", 2)
          val ruleName = parts(0)
          val ruleArg = if (parts.length > 1) parts(1) else null

          rulesByName.get(ruleName) match {
            case Some(ruleInstance) =>
              ruleInstance.level = level
              ruleInstance.ruleArgument = ruleArg
            case None =>
              report.error(s"Unrecognized AVS analyzer rule: $ruleName")
          }
        }
      }
    }

    List(AnalyzerPhaseImpl(rules))
  }

  private def createRules(using Context): List[AnalyzerRule] = List(
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
  override val runsAfter: Set[String] = Set("typer")
  override val runsBefore: Set[String] = Set("patmat")
  val phaseName = "avsAnalyze"
  override def transformUnit(unitTree: tpd.Tree)(using ctx: Context): tpd.Tree = {
    rulesList.filter(_.level != Level.Off).foreach {
      case rule: AnalyzerRuleOnUntyped => rule.analyze(ctx.compilationUnit.untpdTree)
      case rule: AnalyzerRuleOnTyped => rule.analyze(unitTree)
    }
    unitTree
  }
}
