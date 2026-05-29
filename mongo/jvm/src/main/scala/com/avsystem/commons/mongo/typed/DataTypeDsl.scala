package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.annotation.explicitGenerics
import com.avsystem.commons.meta.OptionLike
import com.avsystem.commons.misc.TypedMap
import com.avsystem.commons.serialization.TransparentWrapping

import scala.annotation.{publicInBinary, tailrec}
import scala.collection.Map
import scala.quoted.*

trait DataRefDsl[E, T] {
  // convenience type alias
  type Ref[T0] = MongoPropertyRef[E, T0]

  // ThisRef = MongoPropertyRef for MongoPropertyRef and MongoRef for all other types
  // This makes it possible to refine the result type of `as`, `compose`, `andThen` etc. in MongoPropertyRef
  // TODO: can we redesign this hierarchy to get rid of this abstraction and simplify things?
  type ThisRef[E0, T0] <: MongoRef[E0, T0]
  def SelfRef: ThisRef[E, T]

  // called by .ref macro to ensure that the source type is not opaque and inner references are possible
  @publicInBinary private[typed] def asAdtRef(using ev: IsMongoAdtOrSubtype[T]): ThisRef[E, T] = SelfRef

  /**
   * A macro that interprets an anonymous function as a [[MongoPropertyRef]].
   *
   * Let's define a MongoDB entity:
   * {{{
   *   case class Entity(
   *     id: String,
   *     number: Int,
   *     data: Data,
   *     dataOpt: Opt[Data],
   *     dataList: List[Data],
   *     dataMap: Map[String, Data]
   *   ) extends MongoEntity[String],
   *   object Entity extends MongoEntityCompanion[Entity]
   *
   *   case class Data(
   *     value: Int,
   *     complexData: Map[String, List[Opt[Int]]]
   *   )
   *   object Data extends MongoDataCompanion[Data]
   * }}}
   *
   * The `.ref` macro is available on its companion object.
   *
   * The function may be a reference to one of its fields:
   * {{{
   *   val intRef: MongoPropertyRef[Entity, Int] =
   *     Entity.ref(_.int)
   *   val dataRef: MongoPropertyRef[Entity, Data] =
   *     Entity.ref(_.data)
   * }}}
   *
   * Chaining is also possible:
   *
   * {{{
   *   val dataValueRef: MongoPropertyRef[Entity, Int] =
   *     Entity.ref(_.data.value)
   * }}}
   *
   * When `T` is an `Option`, `Opt`, or similar `Option`-like type, the function may refer its `.get` method to return
   * a reference to its inner value.
   *
   * {{{
   *   val dataRef: MongoPropertyRef[Entity, Data] =
   *     Entity.ref(_.dataOpt.get)
   * }}}
   *
   * When `T` is a collection, the function may call its `apply` method to refer to an element at specific index. Also,
   * `.head` may be used as an alias for `.apply(0)`.
   *
   * {{{
   *   val firstDataRef: MongoPropertyRef[Entity, Data] =
   *     Entity.ref(_.dataList.head)
   *   val secondDataRef: MongoPropertyRef[Entity, Data] =
   *     Entity.ref(_.dataList(1))
   * }}}
   *
   * When `T` is a map, the function may call its `apply` method to refer to a value at specific key.
   *
   * {{{
   *   val dataAtOneRef: MongoPropertyRef[Entity, Data] =
   *     Entity.ref(_.dataMap("one"))
   * }}}
   *
   * Now consider a MongoDB entity expressed as a sealed hierarchy with `@flatten` annotation:
   *
   * {{{
   *   @flatten sealed trait UnionEntity extends MongoEntity[UnionEntity] {
   *     def id: String
   *   }
   *   sealed trait HasNumber extends UnionEntity {
   *     def number: Int
   *   }
   *   case class FirstCase(id: String, flag: Boolean)
   *     extends UnionEntity
   *   case class SecondCase(id: String, number: Int, num: Double)
   *     extends HasNumber
   *   case class ThirdCase(id: String, number: Int, data: Data)
   *     extends HasNumber
   *   object UnionEntity extends MongoEntityCompanion[UnionEntity]
   * }}}
   *
   * The function passed to `.ref` macro may now refer to fields shared by all case classes (represented as abstract
   * `def`s on the sealed trait):
   *
   * {{{
   *   val idRef: MongoPropertyRef[UnionEntity, String] =
   *     UnionEntity.ref(_.id)
   * }}}
   *
   * You may also access fields of individual case classes by "narrowing" the reference explicitly to one particular
   * case class:
   *
   * {{{
   *   val flagRef: MongoPropertyRef[UnionEntity, Boolean] =
   *     UnionEntity.ref(_.as[CaseOne].flag)
   * }}}
   *
   * The same may be done for a subset of case classes sharing some common field. This subset must be expressed with an
   * intermediate sealed trait, like `HasNumber` in the above example:
   *
   * {{{
   *   val numberRef: MongoPropertyRef[UnionEntity, Int] =
   *     UnionEntity.ref(_.as[HasNumber].number)
   * }}}
   *
   * Finally, you can chain all of the above references into more complex paths:
   *
   * {{{
   *   val deeplyNestedRef: MongoPropertyRef[UnionEntity, Int] =
   *     UnionEntity.ref(_.as[ThirdCase].data.complexData("key").head.get)
   * }}}
   */
  inline def ref[T0](inline fun: T => T0): MongoPropertyRef[E, T0] =
    ${ MongoRefMacros.refImpl[E, T, T0, ThisRef[E, T]]('SelfRef, 'fun) }

  /**
   * Given a MongoDB union data type (defined with a sealed hierarchy with `@flatten` annotation), you can narrow it to
   * one of its case classes or intermediate sealed traits.
   *
   * {{{
   *   @flatten sealed trait UnionEntity extends MongoEntity[UnionEntity] {
   *     def id: String
   *   }
   *   sealed trait HasNumber extends UnionEntity {
   *     def number: Int
   *   }
   *   case class FirstCase(id: String, flag: Boolean)
   *     extends UnionEntity
   *   case class SecondCase(id: String, number: Int, num: Double)
   *     extends HasNumber
   *   case class ThirdCase(id: String, number: Int, data: Data)
   *     extends HasNumber
   *   object UnionEntity extends MongoEntityCompanion[UnionEntity]
   *
   *   val thirdCaseRef: MongoRef[UnionEntity, ThirdCase] =
   *     UnionEntity.as[ThirdCase]
   *   val hasNumberRef: MongoRef[UnionEntity, HasNumber] =
   *     UnionEntity.as[HasNumber]
   * }}}
   *
   * You can use such "narrowed" reference as a prefix for accessing [[MongoPropertyRef]]s using the [[ref]] macro,
   * e.g. `thirdCaseRef.ref(_.data)`. You can also use it as a [[MongoProjection]] passed to one of
   * [[TypedMongoCollection]] methods. Note that in such case the projection also serves as a filter, limiting the
   * results of the query only to selected cases.
   */
  @explicitGenerics
  inline def as[C <: T]: MongoRef[E, C] =
    SelfRef.asAdtRef(using compiletime.summonInline).subtypeRefFor[C](using compiletime.summonInline)

  /**
   * Macro for obtaining a [[MongoDocumentFilter]] (condition) which is satisfied only by some specific subtype of an
   * entity type. The entity must be a sealed trait/class and the subtype must be either one of its case classes or an
   * intermediate sealed trait extended by some subset of its case classes.
   *
   * {{{
   *   @flatten sealed trait UnionEntity extends MongoEntity[UnionEntity] {
   *     def id: String
   *   }
   *   sealed trait HasNumber extends UnionEntity {
   *     def number: Int
   *   }
   *   case class FirstCase(id: String, flag: Boolean)
   *     extends UnionEntity
   *   case class SecondCase(id: String, number: Int, num: Double)
   *     extends HasNumber
   *   case class ThirdCase(id: String, number: Int, data: Data)
   *     extends HasNumber
   *   object UnionEntity extends MongoEntityCompanion[UnionEntity]
   *
   *   val isThirdCase: MongoDocumentFilter[UnionEntity] =
   *     UnionEntity.is[ThirdCase]
   *   val hasNumber: MongoDocumentFilter[UnionEntity] =
   *     UnionEntity.is[HasNumber]
   * }}}
   */
  @explicitGenerics
  inline def is[C <: T]: MongoDocumentFilter[E] =
    SelfRef
      .asAdtRef(using compiletime.summonInline)
      .subtypeFilterFor[C](negated = false)(using compiletime.summonInline)

  /**
   * A negated version of [[is]].
   */
  @explicitGenerics
  inline def isNot[C <: T]: MongoDocumentFilter[E] =
    SelfRef
      .asAdtRef(using compiletime.summonInline)
      .subtypeFilterFor[C](negated = true)(using compiletime.summonInline)
}

trait DataTypeDsl[T] extends DataRefDsl[T, T] {
  type ThisRef[E0, T0] = MongoRef[E0, T0]
}

private[typed] object MongoRefMacros {

  /**
   * Implements `DataRefDsl.ref(fun)` by interpreting the anonymous function `fun` as a chain of field selections,
   * subtype narrowings (`as[Subtype]`), option unwraps (`get`), collection/map/typed-map indexing and transparent
   * wrapper unwraps, producing the corresponding [[MongoRef]] chain.
   */
  def refImpl[E: Type, T: Type, T0: Type, B <: MongoRef[E, T]: Type](
    baseRef: Expr[B],
    fun: Expr[T => T0],
  )(using Quotes,
  ): Expr[MongoPropertyRef[E, T0]] = {
    import quotes.reflect.*

    val transparentGetSyms: Set[Symbol] =
      Set(TypeRepr.of[Opt[Any]].typeSymbol, TypeRepr.of[OptArg[Any]].typeSymbol, TypeRepr.of[OptRef[Any]].typeSymbol)
        .flatMap(_.methodMember("get")) ++ TypeRepr.of[Option[Any]].typeSymbol.methodMember("get")

    val (paramSym, bodyTerm) = {
      @tailrec
      def loop(t: Term): (Symbol, Term) = t match {
        case Inlined(_, _, inner) => loop(inner)
        case Block(List(d: DefDef), _: Closure) =>
          d.termParamss.flatMap(_.params) match {
            case List(p) =>
              d.rhs match {
                case Some(body) => (p.symbol, body)
                case None => report.errorAndAbort("lambda has no body", t.pos)
              }
            case _ => report.errorAndAbort("expected single-argument lambda", t.pos)
          }
        case Block(Nil, expr) => loop(expr)
        case _ => report.errorAndAbort(s"expected a lambda, got: ${t.show}", t.pos)
      }
      loop(fun.asTerm)
    }

    extension (expr: Expr[?]) {
      def castTo[X: Type] = expr match {
        case '{ $_ : tpe } =>
          '{
            compiletime.summonInline[tpe <:< X]
            $expr.asInstanceOf[X]
          }
      }
      def widenType: Type[?] = expr.asTerm.tpe.widen.asType
    }

    def isTransparentUnwrap[P: Type](fieldSym: Symbol): Boolean = {
      val sym = TypeRepr.of[P].typeSymbol
      sym.flags.is(Flags.Case) && {
        sym.primaryConstructor.paramSymss.flatten.filterNot(_.isType) match {
          case List(only) if only.name == fieldSym.name =>
            TypeRepr.of[P].memberType(only).widen.asType match {
              case '[r] => Expr.summon[TransparentWrapping[r, P]].isDefined
            }
          case _ => false
        }
      }
    }

    def optionLikeDefined[F: Type, W: Type]: Boolean =
      Expr.summon[OptionLike.Aux[F, W]].isDefined

    def asAdtRef[RefValueType: Type](refTerm: Expr[DataRefDsl[E, RefValueType]]): Expr[MongoRef[E, RefValueType]] =
      '{ $refTerm.asAdtRef(using compiletime.summonInline[IsMongoAdtOrSubtype[RefValueType]]) }

    def fieldRef[P: Type, R: Type](prefix: Expr[DataRefDsl[E, P]], fieldSym: Symbol): Expr[MongoPropertyRef[E, R]] =
      '{ ${ asAdtRef[P](prefix) }.fieldRefFor[R](${ Expr(fieldSym.name) }) }

    def subtypeRefFrom[P: Type, C <: P: Type](prefix: Expr[P]): Expr[MongoRef[E, C]] = '{
      ${ asAdtRef[P](build[P](prefix).asExprOf[DataRefDsl[E, P]]) }
        .subtypeRefFor[C](using compiletime.summonInline[ClassTag[C]])
    }.asInstanceOf[Expr[MongoRef[E, C]]]

    def applyFrom[P: Type, R: Type](prefix: Expr[P], arg: Expr[?]): Expr[MongoPropertyRef[E, R]] = {
      val ref = build[P](prefix)
      Type.of[P] match {
        case '[TypedMap[k]] => '{ $ref.typedMapKeyRef[R]($arg) }
        case '[scala.collection.Map[k, v]] => '{ $ref.dictKeyRef[R]($arg) }
        case '[Iterable[x]] => '{ $ref.indexRef[R](${ arg.asExprOf[Int] }) }
        case _ => '{ $ref.dictKeyRef[R]($arg) }
      }
    }

    def build[V: Type](body: Expr[V]): Expr[MongoPropertyRef[E, T0]] = {

      val AsCall: PartialFunction[Term, (Expr[?], Type[?])] = {
        case TypeApply(Apply(asSel, List(prefix)), List(subTpt)) if asSel.symbol.name == "as" =>
          (prefix.asExpr, subTpt.tpe.widen.asType)
        case Apply(TypeApply(asSel, List(subTpt)), List(prefix)) if asSel.symbol.name == "as" =>
          (prefix.asExpr, subTpt.tpe.widen.asType)
        case TypeApply(Select(prefix, "as"), List(subTpt)) =>
          (prefix.asExpr, subTpt.tpe.widen.asType)
      }

      val ApplyCall: PartialFunction[Term, (Expr[?], Expr[?])] = {
        case Apply(Select(prefix, "apply"), List(arg)) => (prefix.asExpr, arg.asExpr)
        case Apply(TypeApply(Select(prefix, "apply"), _), List(arg)) => (prefix.asExpr, arg.asExpr)
      }

      val OptionGet: PartialFunction[Term, (Expr[?], Symbol)] = {
        case sel @ Select(prefix, "get")
            if transparentGetSyms.contains(sel.symbol) ||
              (prefix.tpe.widen.asType match { case '[p] => optionLikeDefined[p, V] }) =>
          (prefix.asExpr, sel.symbol)
      }

      val result = body.asTerm match {
        case Inlined(_, _, inner) => build[V](inner.asExprOf[V])
        case Block(Nil, expr) => build[V](expr.asExprOf[V])
        case Typed(prefix, _) => build[V](prefix.asExprOf[V])

        case id: Ident if id.symbol == paramSym =>
          baseRef

        case AsCall(prefix, '[c]) =>
          prefix.widenType match {
            case '[type p >: c; p] => subtypeRefFrom[p, c](prefix.asExprOf[p])
          }

        case OptionGet(prefix, _) =>
          prefix.widenType match {
            case '[p] =>
              '{ ${ build[p](prefix.asExprOf[p]).asExprOf[MongoPropertyRef[E, p]] }.getOptionalRef[V] }
          }

        case Select(prefix, "head") =>
          prefix.tpe.widen.asType match {
            case '[p] =>
              '{ ${ build[p](prefix.asExprOf[p]).asExprOf[MongoPropertyRef[E, p]] }.indexRef[V](0) }
          }

        case sel @ Select(prefix, _) =>
          prefix.tpe.widen.asType match {
            case '[p] =>
              if (isTransparentUnwrap[p](sel.symbol))
                '{ ${ build[p](prefix.asExprOf[p]).asExprOf[MongoPropertyRef[E, p]] }.unwrapRef[V] }
              else
                fieldRef[p, V](build[p](prefix.asExprOf[p]).asExprOf[DataRefDsl[E, p]], sel.symbol)
          }

        case ApplyCall(prefix, arg) =>
          prefix.widenType match {
            case '[p] => applyFrom[p, V](prefix.asExprOf[p], arg.asExprOf[Any])
          }

        case other => report.errorAndAbort(s"invalid MongoDB field reference: ${other.show}", other.pos)
      }
      result.asInstanceOf[Expr[MongoPropertyRef[E, T0]]]
    }

    build[T0](bodyTerm.asExprOf[T0])
  }

}
