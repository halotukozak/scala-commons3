package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.{Symbol, defn}

class ImplicitValueClasses extends AnalyzerRule {
  val name: String = "implicitValueClasses"

  override def transformTypeDef(tree: tpd.TypeDef)(using Context): tpd.Tree = {
    val sym = tree.symbol
    if (
      sym.isClass && sym.is(Flags.Implicit) && !sym.is(Flags.Synthetic) && !sym.is(Flags.Module) &&
      !sym.derivesFrom(defn.AnyValClass) && couldBeValueClass(sym)
    ) {
      report(tree, "implicit classes should extend AnyVal to avoid runtime overhead")
    }
    tree
  }

  private def couldBeValueClass(sym: Symbol)(using Context): Boolean = {
    // Check parents: only AnyVal, Any, Object, or universal traits allowed
    val hasValidParents = sym.info.parents.forall { parent =>
      val cls = parent.classSymbol
      cls == defn.AnyValClass || cls == defn.AnyClass || cls == defn.ObjectClass ||
      (cls.is(Flags.Trait) && !cls.derivesFrom(defn.ObjectClass))
    }
    // Check constructor: no implicit parameter lists, and must have val parameter
    val ctor = sym.primaryConstructor
    val allParams = ctor.paramSymss.flatten.filterNot(_.isType)
    val hasImplicitParams = allParams.exists(_.isOneOf(Flags.GivenOrImplicit))
    // A val parameter creates a public (non-private) field accessor on the class
    val hasValParam = sym.info.decls.exists(d =>
      d.is(Flags.ParamAccessor) && !d.is(Flags.Method) && !d.isType && !d.is(Flags.Private)
    )
    hasValidParents && !hasImplicitParams && hasValParam
  }
}
