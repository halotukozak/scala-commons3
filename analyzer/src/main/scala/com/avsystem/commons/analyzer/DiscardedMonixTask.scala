package com.avsystem.commons
package analyzer

import dotty.tools.dotc.*
import ast.tpd
import core.*
import Contexts.*
import Symbols.*
import Types.*

class DiscardedMonixTask() extends CheckingRule("discardedMonixTask"):
  private def extractMonixTaskType(using Context): Type =
    val taskType = resolveClassType("monix.eval.Task")
    if taskType == NoType then NoType
    else taskType.appliedTo(defn.AnyType)

  def performCheck(unitTree: tpd.Tree)(using Context): Unit =
    val monixTaskType = extractMonixTaskType
    if monixTaskType == NoType then return

    object DiscardChecker:
      def checkForDiscard(tree: tpd.Tree, isDiscarded: Boolean): Unit = tree match
        case t if !isDiscarded && t.tpe != null && t.tpe =:= defn.UnitType =>
          checkForDiscard(t, isDiscarded = true)

        case tpd.Block(statements, expr) =>
          statements.foreach(checkForDiscard(_, isDiscarded = true))
          checkForDiscard(expr, isDiscarded)

        case tpd.Template(_, _, _, body) =>
          body match
            case trees: List[tpd.Tree @unchecked] => trees.foreach(checkForDiscard(_, isDiscarded = true))
            case _ => ()

        case tpd.If(_, thenBranch, elseBranch) =>
          checkForDiscard(thenBranch, isDiscarded)
          checkForDiscard(elseBranch, isDiscarded)

        case tpd.Try(body, handlers, finalizer) =>
          checkForDiscard(body, isDiscarded)
          handlers.foreach(checkForDiscard(_, isDiscarded))
          if !finalizer.isEmpty then checkForDiscard(finalizer, isDiscarded = true)

        case tpd.CaseDef(_, _, body) =>
          checkForDiscard(body, isDiscarded)

        case tpd.Match(_, cases) =>
          cases.foreach(checkForDiscard(_, isDiscarded))

        case tpd.Annotated(arg, _) =>
          checkForDiscard(arg, isDiscarded)

        case tpd.Typed(expr, _) =>
          checkForDiscard(expr, isDiscarded)

        case app @ tpd.Apply(tpd.TypeApply(tpd.Select(prefix, name), _), List(tpd.Block(List(defDef: tpd.DefDef), _)))
            if name.toString == "foreach" =>
          checkForDiscard(prefix, isDiscarded = false)
          checkForDiscard(defDef.rhs, isDiscarded)

        case ref: tpd.RefTree if isDiscarded && ref.tpe != null && 
             ref.tpe <:< monixTaskType && !(ref.tpe <:< defn.NullType) =>
          emitReport(
            ref.srcPos,
            "discarded Monix Task - this is probably a mistake because the Task must be run for its side effects"
          )

        case other =>
          object SubTreeTraverser extends tpd.TreeTraverser:
            override def traverse(tree: tpd.Tree)(using Context): Unit =
              checkForDiscard(tree, isDiscarded = false)
              traverseChildren(tree)
          SubTreeTraverser.traverse(other)
    
    DiscardChecker.checkForDiscard(unitTree, isDiscarded = false)
