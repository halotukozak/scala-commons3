package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import Contexts.*
import Symbols.*
import Types.*

class DiscardedMonixTask() extends AnalyzerRule("discardedMonixTask") {
  private def extractMonixTaskType(using Context): Type = {
    val taskType = resolveClassType("monix.eval.Task")
    if (taskType == NoType) NoType
    else taskType.appliedTo(defn.AnyType)
  }

  def performCheck(unitTree: Tree)(using Context): Unit = {
    val monixTaskType = extractMonixTaskType
    if (monixTaskType == NoType) return

    object DiscardChecker {
      def checkForDiscard(tree: Tree, isDiscarded: Boolean): Unit = tree match {
        case t if !isDiscarded && t.tpe != null && t.tpe =:= defn.UnitType =>
          checkForDiscard(t, isDiscarded = true)

        case Block(statements, expr) =>
          statements.foreach(checkForDiscard(_, isDiscarded = true))
          checkForDiscard(expr, isDiscarded)

        case Template(_, _, _, body) =>
          body match {
            case trees: List[Tree @unchecked] => trees.foreach(checkForDiscard(_, isDiscarded = true))
            case _ => ()
          }

        case If(_, thenBranch, elseBranch) =>
          checkForDiscard(thenBranch, isDiscarded)
          checkForDiscard(elseBranch, isDiscarded)

        case Try(body, handlers, finalizer) =>
          checkForDiscard(body, isDiscarded)
          handlers.foreach(checkForDiscard(_, isDiscarded))
          if (!finalizer.isEmpty) checkForDiscard(finalizer, isDiscarded = true)

        case CaseDef(_, _, body) =>
          checkForDiscard(body, isDiscarded)

        case Match(_, cases) =>
          cases.foreach(checkForDiscard(_, isDiscarded))

        case Annotated(arg, _) =>
          checkForDiscard(arg, isDiscarded)

        case Typed(expr, _) =>
          checkForDiscard(expr, isDiscarded)

        case app @ Apply(TypeApply(Select(prefix, name), _), List(Block(List(defDef: DefDef), _)))
            if name.toString == "foreach" =>
          checkForDiscard(prefix, isDiscarded = false)
          checkForDiscard(defDef.rhs, isDiscarded)

        case ref: RefTree
            if isDiscarded && ref.tpe != null && ref.tpe <:< monixTaskType && !(ref.tpe <:< defn.NullType) =>
          emitReport(
            ref.srcPos,
            "discarded Monix Task - this is probably a mistake because the Task must be run for its side effects",
          )

        case other =>
          object SubTreeTraverser extends TreeTraverser {
            override def traverse(tree: Tree)(using Context): Unit = {
              checkForDiscard(tree, isDiscarded = false)
              traverseChildren(tree)
            }
          }
          SubTreeTraverser.traverse(other)
      }
    }

    DiscardChecker.checkForDiscard(unitTree, isDiscarded = false)
  }
}
