package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.annotation.{explicitGenerics, macroPrivate}
import com.avsystem.commons.meta.OptionLike
import com.avsystem.commons.misc.TypedMap
import com.avsystem.commons.serialization.TransparentWrapping

import scala.annotation.tailrec
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
  @macroPrivate def asAdtRef(using ev: IsMongoAdtOrSubtype[T]): ThisRef[E, T] = SelfRef

  /** A macro that interprets an anonymous function as a [[MongoPropertyRef]].
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

  /** Given a MongoDB union data type (defined with a sealed hierarchy with `@flatten` annotation), you can narrow it to
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
    SelfRef.asAdtRef(using scala.compiletime.summonInline).subtypeRefFor[C](using scala.compiletime.summonInline)

  /** Macro for obtaining a [[MongoDocumentFilter]] (condition) which is satisfied only by some specific subtype of an
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
    SelfRef.asAdtRef(using scala.compiletime.summonInline).subtypeFilterFor[C](negated = false)(using scala.compiletime.summonInline)

  /** A negated version of [[is]].
    */
  @explicitGenerics
  inline def isNot[C <: T]: MongoDocumentFilter[E] =
    SelfRef.asAdtRef(using scala.compiletime.summonInline).subtypeFilterFor[C](negated = true)(using scala.compiletime.summonInline)
}

trait DataTypeDsl[T] extends DataRefDsl[T, T] {
  type ThisRef[E0, T0] = MongoRef[E0, T0]
}

private[typed] object MongoRefMacros {

  /** Implements `DataRefDsl.ref(fun)` by interpreting the anonymous function `fun` as a chain of field selections,
    * subtype narrowings (`as[Subtype]`), option unwraps (`get`), collection/map/typed-map indexing and transparent
    * wrapper unwraps, producing the corresponding [[MongoRef]] chain.
    */
  def refImpl[E: Type, T: Type, T0: Type, B <: MongoRef[E, T]: Type](
    baseRef: Expr[B],
    fun: Expr[T => T0],
  )(using Quotes): Expr[MongoPropertyRef[E, T0]] = {
    import quotes.reflect.*

    val transparentGetSyms: Set[Symbol] =
      Set(TypeRepr.of[Opt[Any]].typeSymbol, TypeRepr.of[OptArg[Any]].typeSymbol, TypeRepr.of[OptRef[Any]].typeSymbol)
        .flatMap(_.methodMember("get")) ++ TypeRepr.of[Option[Any]].typeSymbol.methodMember("get")

    def invalid(t: Term): Nothing =
      report.errorAndAbort(s"invalid MongoDB field reference: ${t.show}", t.pos)

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

    def isTransparentUnwrap(prefixTpe: TypeRepr, fieldSym: Symbol): Boolean = {
      val sym = prefixTpe.typeSymbol
      sym.flags.is(Flags.Case) && {
        sym.primaryConstructor.paramSymss.flatten.filterNot(_.isType) match {
          case List(only) if only.name == fieldSym.name =>
            val paramTpe = prefixTpe.memberType(only).widen
            ((paramTpe.asType, prefixTpe.asType): @unchecked) match {
              case ('[r], '[t]) => Expr.summon[TransparentWrapping[r, t]].isDefined
            }
          case _ => false
        }
      }
    }

    def optionLikeDefined[F: Type, W: Type]: Boolean =
      Expr.summon[OptionLike.Aux[F, W]].isDefined

    def asAdtRef(refTerm: Term, refValueTpe: TypeRepr): Term = {
      val evidence = refValueTpe.asType match {
        case '[rt] => '{ scala.compiletime.summonInline[IsMongoAdtOrSubtype[rt]] }
      }
      Apply(Select.unique(refTerm, "asAdtRef"), List(evidence.asTerm))
    }

    def fieldRef(prefixTerm: Term, prefixTpe: TypeRepr, fieldSym: Symbol, fieldTpe: TypeRepr): Term = {
      val adt = asAdtRef(prefixTerm, prefixTpe)
      fieldTpe.asType match {
        case '[ft] =>
          Apply(
            TypeApply(Select.unique(adt, "fieldRefFor"), List(TypeTree.of[ft])),
            List(Literal(StringConstant(fieldSym.name))),
          )
      }
    }

    def subtypeRef(prefixTerm: Term, prefixTpe: TypeRepr, subTpe: TypeRepr): Term = {
      val adt = asAdtRef(prefixTerm, prefixTpe)
      subTpe.asType match {
        case '[st] =>
          val ctag = '{ scala.compiletime.summonInline[scala.reflect.ClassTag[st]] }
          Apply(TypeApply(Select.unique(adt, "subtypeRefFor"), List(TypeTree.of[st])), List(ctag.asTerm))
      }
    }

    // get/unwrap/head/apply are exposed on MongoPropertyRef as @macroPrivate member methods (not extensions),
    // so they resolve via a plain member select with the result type supplied explicitly by the macro.
    def memberCall(prefixRef: Term, method: String, resultTpe: TypeRepr, args: List[Term]): Term =
      resultTpe.asType match {
        case '[r] =>
          val sel = TypeApply(Select.unique(prefixRef, method), List(TypeTree.of[r]))
          if (args.isEmpty) sel else Apply(sel, args)
      }

    def applyCall(prefixRef: Term, prefixTpe: TypeRepr, arg: Term, resultTpe: TypeRepr): Term =
      // order matters: Map/TypedMap are themselves Iterable, so check them before the plain-collection case
      if (prefixTpe <:< TypeRepr.of[TypedMap[[X] =>> Any]])
        memberCall(prefixRef, "typedMapKeyRef", resultTpe, List(arg))
      else if (prefixTpe <:< TypeRepr.of[scala.collection.Map[Any, Any]])
        memberCall(prefixRef, "dictKeyRef", resultTpe, List(arg))
      else if (prefixTpe <:< TypeRepr.of[Iterable[Any]])
        memberCall(prefixRef, "indexRef", resultTpe, List(arg))
      else
        memberCall(prefixRef, "dictKeyRef", resultTpe, List(arg))

    def build(body: Term): Term = body match {
      case Inlined(_, _, inner) => build(inner)
      case Block(Nil, expr) => build(expr)
      case Typed(prefix, _) => build(prefix)

      case id: Ident if id.symbol == paramSym =>
        baseRef.asTerm

      // `as[Subtype]` extension call (macroDslExtensions / poly companion); the extension desugars with the
      // type argument either before or after the value argument depending on form
      case TypeApply(Apply(asSel, List(prefix)), List(subTpt)) if asSel.symbol.name == "as" =>
        subtypeRef(build(prefix), prefix.tpe.widen, subTpt.tpe)
      case Apply(TypeApply(asSel, List(subTpt)), List(prefix)) if asSel.symbol.name == "as" =>
        subtypeRef(build(prefix), prefix.tpe.widen, subTpt.tpe)
      case TypeApply(Select(prefix, "as"), List(subTpt)) =>
        subtypeRef(build(prefix), prefix.tpe.widen, subTpt.tpe)

      // option-like `.get`
      case Select(prefix, "get")
          if transparentGetSyms.contains(body.symbol) ||
            (((prefix.tpe.widen.asType, body.tpe.widen.asType): @unchecked) match {
              case ('[f], '[w]) => optionLikeDefined[f, w]
            }) =>
        memberCall(build(prefix), "getOptionalRef", body.tpe.widen, Nil)

      case Select(prefix, name) =>
        val prefixTpe = prefix.tpe.widen
        if (isTransparentUnwrap(prefixTpe, body.symbol)) memberCall(build(prefix), "unwrapRef", body.tpe.widen, Nil)
        else if (name == "head") memberCall(build(prefix), "indexRef", body.tpe.widen, List(Literal(IntConstant(0))))
        else fieldRef(build(prefix), prefixTpe, body.symbol, body.tpe.widen)

      case Apply(Select(prefix, "apply"), List(arg)) =>
        applyCall(build(prefix), prefix.tpe.widen, arg, body.tpe.widen)
      case Apply(TypeApply(Select(prefix, "apply"), _), List(arg)) =>
        applyCall(build(prefix), prefix.tpe.widen, arg, body.tpe.widen)

      case _ => invalid(body)
    }

    build(bodyTerm).asExprOf[MongoPropertyRef[E, T0]]
  }

}
