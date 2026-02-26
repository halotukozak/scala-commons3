package com.avsystem.commons
package analyzer

import dotty.tools.dotc.ast.tpd
import tpd._
import dotty.tools.dotc.core.Contexts.Context
import scala.util.chaining.scalaUtilChainingOps
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.plugins.PluginPhase
import dotty.tools.dotc.typer.TyperPhase
import dotty.tools.dotc.transform.Pickler

trait AnalyzerRule(val name: String, var level: Level = Level.Warn) extends PluginPhase {

  var argument: Option[String] = None

  def requiredSymbols: List[Symbol] = Nil

  final def phaseName: String = s"avs.$name"

  override def runsAfter: Set[String] = Set(TyperPhase.name)

  override def runsBefore: Set[String] = Set(Pickler.name)

  protected final def report(tree: tpd.Tree, message: String)(using Context): Unit = level match {
    case Level.Off => ()
    case Level.Info => dotty.tools.dotc.report.echo(s"[AVS] $message", tree.srcPos)
    case Level.Warn => dotty.tools.dotc.report.warning(s"[AVS] $message", tree.srcPos)
    case Level.Error => dotty.tools.dotc.report.error(s"[AVS] $message", tree.srcPos)
  }

  extension (s: Symbol) protected def hasOrInheritsAnnotation(cls: Symbol)(using Context): Boolean =
    s.hasAnnotation(cls) || s.allOverriddenSymbols.exists(_.hasAnnotation(cls))

  protected def verifyIdent(tree: tpd.Ident)(using Context): Unit = ()
  protected def verifySelect(tree: tpd.Select)(using Context): Unit = ()
  protected def verifyThis(tree: tpd.This)(using Context): Unit = ()
  protected def verifySuper(tree: tpd.Super)(using Context): Unit = ()
  protected def verifyApply(tree: tpd.Apply)(using Context): Unit = ()
  protected def verifyTypeApply(tree: tpd.TypeApply)(using Context): Unit = ()
  protected def verifyLiteral(tree: tpd.Literal)(using Context): Unit = ()
  protected def verifyNew(tree: tpd.New)(using Context): Unit = ()
  protected def verifyTyped(tree: tpd.Typed)(using Context): Unit = ()
  protected def verifyAssign(tree: tpd.Assign)(using Context): Unit = ()
  protected def verifyBlock(tree: tpd.Block)(using Context): Unit = ()
  protected def verifyIf(tree: tpd.If)(using Context): Unit = ()
  protected def verifyClosure(tree: tpd.Closure)(using Context): Unit = ()
  protected def verifyMatch(tree: tpd.Match)(using Context): Unit = ()
  protected def verifyCaseDef(tree: tpd.CaseDef)(using Context): Unit = ()
  protected def verifyLabeled(tree: tpd.Labeled)(using Context): Unit = ()
  protected def verifyReturn(tree: tpd.Return)(using Context): Unit = ()
  protected def verifyWhileDo(tree: tpd.WhileDo)(using Context): Unit = ()
  protected def verifyTry(tree: tpd.Try)(using Context): Unit = ()
  protected def verifySeqLiteral(tree: tpd.SeqLiteral)(using Context): Unit = ()
  protected def verifyInlined(tree: tpd.Inlined)(using Context): Unit = ()
  protected def verifyQuote(tree: tpd.Quote)(using Context): Unit = ()
  protected def verifySplice(tree: tpd.Splice)(using Context): Unit = ()
  protected def verifyTypeTree(tree: tpd.TypeTree)(using Context): Unit = ()
  protected def verifyBind(tree: tpd.Bind)(using Context): Unit = ()
  protected def verifyAlternative(tree: Alternative)(using Context): Unit = ()
  protected def verifyUnApply(tree: UnApply)(using Context): Unit = ()
  protected def verifyValDef(tree: tpd.ValDef)(using Context): Unit = ()
  protected def verifyDefDef(tree: tpd.DefDef)(using Context): Unit = ()
  protected def verifyTypeDef(tree: tpd.TypeDef)(using Context): Unit = ()
  protected def verifyTemplate(tree: tpd.Template)(using Context): Unit = ()
  protected def verifyPackageDef(tree: tpd.PackageDef)(using Context): Unit = ()
  protected def verifyStats(trees: List[Tree])(using Context): Unit = ()
  protected def verifyUnit(tree: Tree)(using Context): Unit = ()
  protected def verifyOther(tree: Tree)(using Context): Unit = ()

  override final def transformIdent(tree: tpd.Ident)(using Context): Tree = tree.tap(verifyIdent)
  override final def transformSelect(tree: tpd.Select)(using Context): Tree = tree.tap(verifySelect)
  override final def transformThis(tree: tpd.This)(using Context): Tree = tree.tap(verifyThis)
  override final def transformSuper(tree: tpd.Super)(using Context): Tree = tree.tap(verifySuper)
  override final def transformApply(tree: tpd.Apply)(using Context): Tree = tree.tap(verifyApply)
  override final def transformTypeApply(tree: tpd.TypeApply)(using Context): Tree = tree.tap(verifyTypeApply)
  override final def transformLiteral(tree: tpd.Literal)(using Context): Tree = tree.tap(verifyLiteral)
  override final def transformNew(tree: tpd.New)(using Context): Tree = tree.tap(verifyNew)
  override final def transformTyped(tree: tpd.Typed)(using Context): Tree = tree.tap(verifyTyped)
  override final def transformAssign(tree: tpd.Assign)(using Context): Tree = tree.tap(verifyAssign)
  override final def transformBlock(tree: tpd.Block)(using Context): Tree = tree.tap(verifyBlock)
  override final def transformIf(tree: tpd.If)(using Context): Tree = tree.tap(verifyIf)
  override final def transformClosure(tree: tpd.Closure)(using Context): Tree = tree.tap(verifyClosure)
  override final def transformMatch(tree: Match)(using Context): Tree = tree.tap(verifyMatch)
  override final def transformCaseDef(tree: CaseDef)(using Context): Tree = tree.tap(verifyCaseDef)
  override final def transformLabeled(tree: Labeled)(using Context): Tree = tree.tap(verifyLabeled)
  override final def transformReturn(tree: Return)(using Context): Tree = tree.tap(verifyReturn)
  override final def transformWhileDo(tree: WhileDo)(using Context): Tree = tree.tap(verifyWhileDo)
  override final def transformTry(tree: Try)(using Context): Tree = tree.tap(verifyTry)
  override final def transformSeqLiteral(tree: SeqLiteral)(using Context): Tree = tree.tap(verifySeqLiteral)
  override final def transformInlined(tree: Inlined)(using Context): Tree = tree.tap(verifyInlined)
  override final def transformQuote(tree: Quote)(using Context): Tree = tree.tap(verifyQuote)
  override final def transformSplice(tree: Splice)(using Context): Tree = tree.tap(verifySplice)
  override final def transformTypeTree(tree: TypeTree)(using Context): Tree = tree.tap(verifyTypeTree)
  override final def transformBind(tree: Bind)(using Context): Tree = tree.tap(verifyBind)
  override final def transformAlternative(tree: Alternative)(using Context): Tree = tree.tap(verifyAlternative)
  override final def transformUnApply(tree: UnApply)(using Context): Tree = tree.tap(verifyUnApply)
  override final def transformValDef(tree: ValDef)(using Context): Tree = tree.tap(verifyValDef)
  override final def transformDefDef(tree: DefDef)(using Context): Tree = tree.tap(verifyDefDef)
  override final def transformTypeDef(tree: TypeDef)(using Context): Tree = tree.tap(verifyTypeDef)
  override final def transformTemplate(tree: Template)(using Context): Tree = tree.tap(verifyTemplate)
  override final def transformPackageDef(tree: PackageDef)(using Context): Tree = tree.tap(verifyPackageDef)
  override final def transformStats(trees: List[Tree])(using Context): List[Tree] = trees.tap(verifyStats)
  override final def transformUnit(tree: Tree)(using Context): Tree = tree.tap(verifyUnit)
  override final def transformOther(tree: Tree)(using Context): Tree = tree.tap(verifyOther)

}

enum Level {
  case Off, Info, Warn, Error
}
