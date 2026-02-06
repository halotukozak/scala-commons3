package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*

class DiscardedMonixTask(using Context) extends AnalyzerRuleOnTyped("discardedMonixTask") {
  private lazy val extractMonixTaskType = resolveClassType("monix.eval.Task").map(_.appliedTo(defn.AnyType))

  def analyze(unitTree: Tree)(using Context): Unit = extractMonixTaskType.foreach { monixTaskType =>
    def checkDiscardedTask(tree: Tree, isDiscarded: Boolean): Unit = tree match {
      case t if !isDiscarded && t.tpe != null && t.tpe =:= defn.UnitType =>
        checkDiscardedTask(t, isDiscarded = true)

      case Block(statements, expr) =>
        statements.foreach(checkDiscardedTask(_, isDiscarded = true))
        checkDiscardedTask(expr, isDiscarded)

      case Template(_, _, _, body) =>
        body match {
          case trees: List[Tree @unchecked] => trees.foreach(checkDiscardedTask(_, isDiscarded = true))
          case _ => ()
        }

      case If(_, thenBranch, elseBranch) =>
        checkDiscardedTask(thenBranch, isDiscarded)
        checkDiscardedTask(elseBranch, isDiscarded)

      case Try(body, handlers, finalizer) =>
        checkDiscardedTask(body, isDiscarded)
        handlers.foreach(checkDiscardedTask(_, isDiscarded))
        if (!finalizer.isEmpty) checkDiscardedTask(finalizer, isDiscarded = true)

      case CaseDef(_, _, body) =>
        checkDiscardedTask(body, isDiscarded)

      case Match(_, cases) =>
        cases.foreach(checkDiscardedTask(_, isDiscarded))

      case Annotated(arg, _) =>
        checkDiscardedTask(arg, isDiscarded)

      case Typed(expr, _) =>
        checkDiscardedTask(expr, isDiscarded)

      case app @ Apply(TypeApply(Select(prefix, name), _), List(Block(List(defDef: DefDef), _)))
          if name.toString == "foreach" =>
        checkDiscardedTask(prefix, isDiscarded = false)
        checkDiscardedTask(defDef.rhs, isDiscarded)

      case ref: RefTree
          if isDiscarded && ref.tpe != null && ref.tpe <:< monixTaskType && !(ref.tpe <:< defn.NullType) =>
        emitReport(
          ref.srcPos,
          "discarded Monix Task - this is probably a mistake because the Task must be run for its side effects",
        )

      case other =>
        object SubTreeTraverser extends TreeTraverser {
          override def traverse(tree: Tree)(using Context): Unit = {
            checkDiscardedTask(tree, isDiscarded = false)
            traverseChildren(tree)
          }
        }
        SubTreeTraverser.traverse(other)
    }

    checkDiscardedTask(unitTree, isDiscarded = false)
  }
}
