package com.avsystem.commons
package serialization
import scala.collection.mutable
trait GenCodecJavaBuilder { this: GenCodec.type =>
  inline def fromJavaBuilder[T, B](inline newBuilder: B)(inline build: B => T): GenCodec[T] =
    ${ fromJavaBuilderImpl[T, B]('{ newBuilder }, '{ build }) }
}
def fromJavaBuilderImpl[T: Type, B: Type](
  newBuilder: Expr[B],
  build: Expr[B => T],
)(using quotes: Quotes,
): Expr[GenCodec[T]] = {
  import quotes.reflect.*

  def returnTypeOf(ownerTpe: TypeRepr, method: Symbol): TypeRepr =
    ownerTpe.memberType(method).widen match
      case MethodType(_, _, res) => res
      case t => t

  extension (ms: Symbol) {
    private def isJavaGetter(ownerTpe: TypeRepr): Boolean =
      !ms.paramSymss.exists(_.exists(_.isTypeParam)) && ms.paramSymss == List(Nil) && {
        val expectedPrefix = if (returnTypeOf(ownerTpe, ms) =:= TypeRepr.of[Boolean]) "is" else "get"
        ms.name.startsWith(expectedPrefix) && ms.name.length > expectedPrefix.length &&
        ms.name.charAt(expectedPrefix.length).isUpper
      }

    private def isJavaSetter(ownerTpe: TypeRepr, propTpe: TypeRepr): Boolean =
      !ms.paramSymss.exists(_.exists(_.isTypeParam)) && ms.paramSymss.map(_.length) == List(1) && {
        ms.name.startsWith("set") && ms.name.length > 3 && ms.name.charAt(3).isUpper
      } && {
        ownerTpe.memberType(ms).widen match
          case MethodType(_, List(paramT), _) => paramT =:= propTpe
          case _ => false
      }
  }

  val fieldNames = new mutable.ListBuffer[Expr[String]]
  val getters = new mutable.ListBuffer[Expr[T => Any]]
  val setters = new mutable.ListBuffer[Expr[(B, Any) => B]]
  val deps = new mutable.ListBuffer[Expr[GenCodec[?]]]

  val tTpe = TypeRepr.of[T]
  val bTpe = TypeRepr.of[B]
  (tTpe.typeSymbol.methodMembers ++ tTpe.typeSymbol.declaredMethods).distinct.iterator.foreach {
    case getter if getter.isDefDef && getter.isJavaGetter(tTpe) =>
      returnTypeOf(tTpe, getter).asType match {
        case '[propType] =>
          val getterName = getter.name
          val setterName = getterName.replaceFirst("^(get|is)", "set")

          val setterOpt = bTpe.typeSymbol.methodMember(setterName).find { s =>
            s.isDefDef && s.isJavaSetter(bTpe, TypeRepr.of[propType])
          }
          setterOpt.foreach { setter =>
            val propName = setterName.charAt(3).toLower.toString + setterName.drop(4)
            fieldNames += Expr(propName)
            deps +=
              Expr
                .summon[GenCodec[propType]]
                .getOrElse(
                  throw new RuntimeException(
                    s"Cannot materialize GenCodec for ${Type.show[T]} because of problem with property $propName:\n",
                  ),
                )
            getters += Lambda(
              Symbol.spliceOwner,
              MethodType(List("v"))(_ => List(TypeRepr.of[T]), _ => TypeRepr.of[propType]),
              (sym, args) => args.head.asInstanceOf[Term].select(getter).appliedToNone,
            ).asExprOf[T => propType]

            setters += Lambda(
              Symbol.spliceOwner,
              MethodType(List("b", "v"))(_ => List(TypeRepr.of[B], TypeRepr.of[Any]), _ => TypeRepr.of[B]),
              (sym, args) => {
                given Quotes = sym.asQuotes
                val Seq(b: Term, v: Term) = args.runtimeChecked
                b.select(setter).appliedTo('{ ${ v.asExpr }.asInstanceOf[propType] }.asTerm)
              },
            ).asExprOf[(B, Any) => B]
          }
      }
    case _ =>
  }

  val typeReprStr = Expr(TypeRepr.of[T].show)
  '{
    new JavaBuilderBasedCodec[T, B](
      $typeReprStr,
      $newBuilder,
      $build,
      ${ Expr.ofList(fieldNames.result()) }.toArray,
      ${ Expr.ofList(getters.result()) }.toArray,
      ${ Expr.ofList(setters.result()) }.toArray,
    ) {
      override val dependencies = ${ Expr.ofList(deps.result()) }.toArray
    }
  }
}
