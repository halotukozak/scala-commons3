package com.avsystem.commons
package misc

import scala.quoted.{Expr, FromExpr, Quotes, ToExpr, Type}

/**
 * Lifting / unlifting between AVSystem value types and `scala.quoted.Expr` for use inside
 *  macro impls.
 *
 *  The `Opt` family ([[Opt]], [[OptArg]], [[OptRef]], [[NOpt]]) is `extends AnyVal`. Defining a
 *  direct `given ToExpr[Opt[T]] = new ToExpr[Opt[T]] { ... }` instance causes an erasure-level
 *  bridge collision with the inherited abstract `ToExpr.apply(x: T)` — both erase to
 *  `apply(Object): Object`. The workaround is to expose plain helper functions instead of
 *  typeclass instances, so macro authors call e.g. `QuoteSupport.exprOf(myOpt)` rather than
 *  `Expr(myOpt)`. Non-value-class types (`Timestamp`, `Bytes`) get regular `ToExpr` / `FromExpr`
 *  givens.
 */
object QuoteSupport {

  // todo make it ToExpr/FromExpr
  def optExpr[T: {ToExpr, Type}](o: Opt[T])(using Quotes): Expr[Opt[T]] = o match {
    case Opt.Empty => '{ Opt.empty[T] }
    case Opt(v) => '{ Opt.some[T](${ Expr(v) }) }
  }

  // todo make it ToExpr/FromExpr
  def matchOpt[T: {FromExpr, Type}](x: Expr[Opt[T]])(using Quotes): Option[Opt[T]] = x match {
    case '{ Opt.empty[T] } => Some(Opt.Empty)
    case '{ Opt.Empty } => Some(Opt.Empty)
    case '{ Opt.some[T](${ Expr(v) }) } => Some(Opt.some(v))
    case _ => None
  }

  // todo make it ToExpr/FromExpr
  def optArgExpr[T: {ToExpr, Type}](o: OptArg[T])(using Quotes): Expr[OptArg[T]] = o match {
    case OptArg.Empty => '{ OptArg.empty[T] }
    case OptArg(v) => '{ OptArg.some[T](${ Expr(v) }) }
  }

  // todo make it ToExpr/FromExpr
  def matchOptArg[T: {FromExpr, Type}](x: Expr[OptArg[T]])(using Quotes): Option[OptArg[T]] = x match {
    case '{ OptArg.empty[T] } => Some(OptArg.Empty)
    case '{ OptArg.Empty } => Some(OptArg.Empty)
    case '{ OptArg.some[T](${ Expr(v) }) } => Some(OptArg.some(v))
    case _ => None
  }

  // todo make it ToExpr/FromExpr
  def optRefExpr[T <: AnyRef: {ToExpr, Type}](o: OptRef[T])(using Quotes): Expr[OptRef[T]] = o.toOpt match {
    case Opt.Empty => '{ OptRef.empty[T] }
    case Opt(v) => '{ OptRef.some[T](${ Expr(v) }) }
  }

  // todo make it ToExpr/FromExpr
  def matchOptRef[T <: AnyRef: {FromExpr, Type}](x: Expr[OptRef[T]])(using Quotes): Option[OptRef[T]] = x match {
    case '{ OptRef.empty[T] } => Some(OptRef.Empty)
    case '{ OptRef.Empty } => Some(OptRef.Empty)
    // OptRef.apply / .some accept `A | Null`; no FromExpr[T | Null] available, so only the empty
    // form round-trips. Lifting works via exprOf above.
    case _ => None
  }
  // todo make it ToExpr/FromExpr
  def nOptExpr[T: {ToExpr, Type}](n: NOpt[T])(using Quotes): Expr[NOpt[T]] =
    if (n.isEmpty) '{ NOpt.empty[T] } else '{ NOpt.some[T](${ Expr(n.get) }) }
  // todo make it ToExpr/FromExpr

  def matchNOpt[T: {FromExpr, Type}](x: Expr[NOpt[T]])(using Quotes): Option[NOpt[T]] = x match {
    case '{ NOpt.empty[T] } => Some(NOpt.Empty)
    case '{ NOpt.Empty } => Some(NOpt.Empty)
    case '{ NOpt.some[T](${ Expr(v) }) } => Some(NOpt.some(v))
    case _ => None
  }

  given timestampToExpr: ToExpr[Timestamp] with {
    def apply(t: Timestamp)(using Quotes): Expr[Timestamp] = '{ Timestamp(${ Expr(t.millis) }) }
  }
  given timestampFromExpr: FromExpr[Timestamp] with {
    def unapply(x: Expr[Timestamp])(using Quotes): Option[Timestamp] = x match {
      case '{ Timestamp(${ Expr(m) }) } => Some(Timestamp(m))
      case _ => None
    }
  }

  given bytesToExpr: ToExpr[Bytes] with {
    def apply(b: Bytes)(using Quotes): Expr[Bytes] = '{ Bytes(${ Expr(b.bytes) }) }
  }
  // No FromExpr[Bytes] — stdlib lacks FromExpr[Array[Byte]]. Lift-only.
}
