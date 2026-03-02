package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.{defn, Symbol}

class ImplicitValueClasses extends AnalyzerRule("implicitValueClasses") {

  override def verifyTypeDef(tree: tpd.TypeDef)(using Context): Unit = {
    val sym = tree.symbol
    if (
      sym.isClass && sym.is(Flags.Implicit, butNot = Flags.Synthetic | Flags.Module) &&
      !sym.derivesFrom(defn.AnyValClass) && couldBeValueClass(sym)
    ) {
      report(tree, "implicit classes should extend AnyVal to avoid runtime overhead")
    }
  }

  private def couldBeValueClass(sym: Symbol)(using Context): Boolean = {
    // Check parents: only AnyVal, Any, Object, or universal traits allowed
    def hasValidParents = sym.info.parents.forall { parent =>
      val cls = parent.classSymbol
      cls == defn.AnyValClass || cls == defn.AnyClass || cls == defn.ObjectClass ||
      (cls.is(Flags.Trait) && !cls.derivesFrom(defn.ObjectClass))
    }

    def hasImplicitParams = {
      // Check constructor: no implicit parameter lists, and must have val parameter
      val ctor = sym.primaryConstructor
      val allParams = ctor.paramSymss.flatten.filterNot(_.isType)
      allParams.exists(_.isOneOf(Flags.GivenOrImplicit))
    }
    // A val parameter creates a public (non-private) field accessor on the class
    def hasValParam =
      sym.info.decls.exists(d => d.is(Flags.ParamAccessor, butNot = Flags.Method | Flags.Private) && !d.isType)

    hasValidParents && !hasImplicitParams && hasValParam
  }
}
