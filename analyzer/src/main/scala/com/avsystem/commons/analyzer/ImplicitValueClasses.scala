package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import Contexts.*
import Symbols.*
import Types.*

class ImplicitValueClasses() extends CheckingRule("implicitValueClasses", SeverityLevel.Warning) {
  private def shouldReportNested: Boolean =
    ruleArgument match {
      case "all" => true
      case "top-level-only" | null => false
      case other => throw IllegalArgumentException(s"Unknown ImplicitValueClasses option: $other")
    }

  def performCheck(unitTree: Tree)(using Context): Unit = {
    val anyValClass = defn.AnyValClass
    val objectClass = defn.ObjectClass
    val anyClass = defn.AnyClass
    val defaultBaseClasses = Set(anyClass, anyValClass, objectClass)

    checkChildren(unitTree) { tree =>
      tree match {
        case classDef: TypeDef if classDef.symbol.is(Flags.Implicit) && classDef.symbol.isClass =>
          val classType = classDef.symbol.info
          val baseClassSet = classType.baseClasses.toSet

          val hasAnyValParent = baseClassSet.contains(anyValClass)

          val hasNonDefaultBase = baseClassSet.exists { base =>
            def isUniversalTrait = base.is(Flags.Trait) && base.asClass.superClass == anyClass
            base != classDef.symbol && !defaultBaseClasses.contains(base) && !isUniversalTrait
          }

          val primaryConstructor = classDef.symbol.primaryConstructor
          val constructorParamLists = if (primaryConstructor.exists) primaryConstructor.info.paramInfoss else Nil
          val hasExactlyOneParam = constructorParamLists match {
            case List(List(param)) => true
            case _ => false
          }

          val paramIsValueClass = primaryConstructor.exists &&
            primaryConstructor.info.paramInfoss.flatten.headOption.exists { paramType =>
              paramType.typeSymbol.is(Flags.Trait) || paramType.typeSymbol.isDerivedValueClass
            }

          if (!hasAnyValParent && !hasNonDefaultBase && hasExactlyOneParam && !paramIsValueClass) {
            val nestedInNonStaticContext = !classDef.symbol.isStatic

            val message = "Implicit classes should always extend AnyVal to become value classes" +
              (if (nestedInNonStaticContext) ". Nested classes should be extracted to top-level objects" else "")

            if (shouldReportNested || !nestedInNonStaticContext) {
              emitReport(classDef.srcPos, message)
            } else {
              emitReport(classDef.srcPos, message, severity = SeverityLevel.Information)
            }
          }
        case _ =>
      }
    }
  }
}
