package com.avsystem.commons
package mirror

import com.avsystem.commons.meta.OptionLike
import com.avsystem.commons.serialization.{TransparentWrapping, whenAbsent}

import scala.annotation.{RefiningAnnotation, implicitNotFound, tailrec}
import scala.quoted.{Expr, Quotes, Type}

@implicitNotFound("No DerMirror could be generated.\nDiagnose any issues by calling DerMirror.derived directly")
sealed trait DerMirror {
  final type MirroredElemTypes = Tuple.Map[
    MirroredElems,
    [E] =>> E match {
      case DerElem.Of[t] => t
    },
  ]
  final type MirroredElemLabels = Tuple.Map[
    MirroredElems,
    [E] =>> E match {
      case DerElem.LabelOf[l] => l
    },
  ]
  type MirroredType
  type MirroredLabel <: String
  type Metadata <: Meta
  type MirroredElems <: Tuple
  type GeneratedElems <: Tuple
  def mirroredElems: MirroredElems
  def generatedElems: GeneratedElems
}

sealed trait DerElem {
  type MirroredType
  type MirroredLabel <: String
  type Metadata <: Meta
}

sealed trait DerFieldElem extends DerElem {
  def default: Option[MirroredType]
}

object DerFieldElem {
  type Of[T] = DerFieldElem { type MirroredType = T }
}

sealed trait DerSubElem extends DerElem
object DerSubElem {
  type Of[T] = DerSubElem { type MirroredType = T }
}

sealed trait DerSubSingletonElem extends DerSubElem {
  def value: MirroredType
}

object DerSubSingletonElem {
  type Of[T] = DerSubSingletonElem { type MirroredType = T }
}

sealed trait GeneratedDerElem extends DerFieldElem {
  type OuterMirroredType
  def apply(outer: OuterMirroredType): MirroredType

  final def default: Option[MirroredType] = None
}

object GeneratedDerElem {
  type Of[T] = GeneratedDerElem { type MirroredType = T }

  type OuterOf[Outer] = GeneratedDerElem { type OuterMirroredType = Outer }
}

// workaround for https://github.com/scala/scala3/issues/25245
sealed trait GeneratedDerElemWorkaround[Outer, Elem] extends GeneratedDerElem {
  final type OuterMirroredType = Outer
  final type MirroredType = Elem
}

object DerElem {
  type Of[T] = DerElem { type MirroredType = T }
  type LabelOf[l <: String] = DerElem { type MirroredLabel = l }
  type MetaOf[m <: Meta] = DerElem { type Metadata = m }
}

private trait Meta

open class MetaAnnotation extends RefiningAnnotation

object DerMirror {
  type Of[T] = DerMirror { type MirroredType = T }
  type ProductOf[T] = DerMirror.Product { type MirroredType = T }
  type SumOf[T] = DerMirror.Sum { type MirroredType = T }
  type SingletonOf[T] = DerMirror.Singleton { type MirroredType = T }
  type TransparentOf[T, U] = DerMirror.Transparent { type MirroredType = T; type MirroredElemType = U }

  type LabelOf[l <: String] = DerElem { type MirroredLabel = l }
  type MetaOf[m <: Meta] = DerElem { type Metadata = m }

  extension (m: DerMirror) {
    transparent inline def hasAnnotation[A <: MetaAnnotation]: Boolean = ${ hasAnnotationImpl[A, m.type] }
    inline def getAnnotation[A <: MetaAnnotation]: Option[A] = ${ getAnnotationImpl[A, m.type] }
  }
  transparent inline given derived[T]: Of[T] = ${ derivedImpl[T] }
  // like ValueOf but without the implicit search and boxing
  inline def singleValueOf[T <: scala.Singleton]: T = ${ singleValueOfImpl[T] }
  private def metaOf[DM <: DerMirror: Type](using quotes: Quotes): Type[? <: Meta] =
    Type.of[DM] match {
      // it cannot be extracted via type Metadata = meta
      case '[type meta <: Meta; DerMirror { type Metadata <: meta }] =>
        Type.of[meta]
    }
  private def getAnnotationImpl[A <: MetaAnnotation: Type, DM <: DerMirror: Type](using quotes: Quotes)
    : Expr[Option[A]] = {
    import quotes.reflect.*

    @tailrec def loop(tpe: TypeRepr): Option[Expr[A]] = tpe match {
      case AnnotatedType(underlying, annot) if annot.tpe <:< TypeRepr.of[A] => Some(annot.asExprOf[A])
      case AnnotatedType(underlying, _) => loop(underlying)
      case _ => None
    }

    Expr.ofOption(loop(TypeRepr.of(using metaOf[DM])))
  }
  private def hasAnnotationImpl[A <: MetaAnnotation: Type, DM <: DerMirror: Type](using quotes: Quotes): Expr[Boolean] = {
    import quotes.reflect.*

    @tailrec def loop(tpe: TypeRepr): Boolean = tpe match {
      case AnnotatedType(underlying, annot) if annot.tpe <:< TypeRepr.of[A] => true
      case AnnotatedType(underlying, _) => loop(underlying)
      case _ => false
    }

    Expr(loop(TypeRepr.of(using metaOf[DM])))
  }
  private def stringToType(str: String)(using quotes: Quotes): Type[? <: String] = {
    import quotes.reflect.*
    ConstantType(StringConstant(str)).asType.asInstanceOf[Type[? <: String]]
  }
  private def traverseTypes(tpes: List[Type[? <: AnyKind]])(using Quotes): Type[? <: Tuple] = {
    val empty: Type[? <: Tuple] = Type.of[EmptyTuple]
    tpes.foldRight(empty) {
      case ('[tpe], '[type acc <: Tuple; acc]) => Type.of[tpe *: acc]
      case (_, _) => wontHappen
    }
  }
  private def singleValueOfImpl[T <: scala.Singleton: Type](using quotes: Quotes): Expr[T] = {
    import quotes.reflect.*
    val term = TypeRepr.of[T] match {
      case ConstantType(c: Constant) => Literal(c)
      case tp: TypeRef if tp <:< TypeRepr.of[Unit] => Literal(UnitConstant())
      case n: TermRef => Ref(n.termSymbol)
      case ts: ThisType => This(ts.classSymbol.get)
      case tp => report.errorAndAbort(s"Unsupported singleton type: ${tp.show}")
    }
    term.asExprOf[T]
  }

  private def traverseTuple(tpe: Type[? <: Tuple])(using quotes: Quotes): List[Type[? <: AnyKind]] = tpe match {
    case '[EmptyTuple] => Nil
    case '[t *: ts] => Type.of[t] :: traverseTuple(Type.of[ts])
  }

  private def derivedImpl[T: Type](using quotes: Quotes): Expr[DerMirror.Of[T]] = {
    import quotes.reflect.*

    val tTpe = TypeRepr.of[T]
    val tSymbol = tTpe.typeSymbol

    def metaTypeOf(symbol: Symbol): Type[? <: Meta] = {
      val annotations = symbol.annotations.filter(_.tpe <:< TypeRepr.of[MetaAnnotation])
      annotations
        .foldRight(TypeRepr.of[Meta])((annot, tpe) => AnnotatedType(tpe, annot))
        .asType
        .asInstanceOf[Type[? <: Meta]]
    }

    extension (symbol: Symbol) {
      def hasAnnotationOf[AT <: Annotation: Type] =
        symbol.hasAnnotation(TypeRepr.of[AT].typeSymbol)

      def getAnnotationOf[AT <: Annotation: Type] =
        symbol.getAnnotation(TypeRepr.of[AT].typeSymbol).map(_.asExprOf[AT])
    }

    def labelTypeOf(sym: Symbol, fallback: String): Type[? <: String] = {
      val syms = Iterator(sym) ++ sym.allOverriddenSymbols
      val res = syms.find(_.hasAnnotationOf[name]).flatMap(_.getAnnotationOf[name])
      stringToType(res match {
        case Some('{ new `name`($value) }) => value.valueOrAbort
        case _ => fallback
      })
    }

    val generatedElems = for {
      member <- tSymbol.fieldMembers ++ tSymbol.declaredMethods
      if member.hasAnnotationOf[generated]
      _ = if (!(member.isValDef || member.isDefDef))
        report.errorAndAbort(
          "@generated can only be applied to vals and defs.",
          member.pos.getOrElse(Position.ofMacroExpansion),
        )
      _ = member.paramSymss match {
        case Nil => // no parameters, it's a val or a def without parameters
        case List(Nil) => // a def with empty parameter list
        case paramLists =>
          for {
            paramList <- paramLists
            param <- paramList
          } if (!param.flags.is(Flags.EmptyFlags)) symbolInfo(param).dbg // todo
      }
    } yield {
      val elemTpe = tTpe.memberType(member).widen

      (elemTpe.asType, labelTypeOf(member, member.name), metaTypeOf(member)).runtimeChecked match {
        case ('[elemTpe], '[type elemLabel <: String; elemLabel], '[type meta <: Meta; meta]) =>
          '{
            new GeneratedDerElemWorkaround[T, elemTpe] {
              type MirroredLabel = elemLabel
              type Metadata = meta
              def apply(outer: T): elemTpe = ${ '{ outer }.asTerm.select(member).asExprOf[elemTpe] }
            }: GeneratedDerElem {
              type MirroredType = elemTpe
              type MirroredLabel = elemLabel
              type Metadata = meta
              type OuterMirroredType = T
            }
          }
      }
    }

    def singleCaseFieldOf(symbol: Symbol): Symbol = symbol.caseFields match {
      case field :: Nil => field
      case _ => report.errorAndAbort(s"Expected a single case field for ${symbol.name}")
    }

    def derFieldOf(field: Symbol): Expr[DerFieldElem] =
      (field.termRef.widen.asType, labelTypeOf(field, field.name), metaTypeOf(field)).runtimeChecked match {
        case ('[fieldType], '[type elemLabel <: String; elemLabel], '[type fieldMeta <: Meta; fieldMeta]) =>
          '{
            new DerFieldElem {
              type MirroredType = fieldType
              type MirroredLabel = elemLabel
              type Metadata = fieldMeta

              def default = ${ defaultOf[fieldType](0, field) }
            }
          }
      }

    def defaultOf[E: Type](index: Int, symbol: Symbol): Expr[Option[E]] = Expr.ofOption {
      def fromWhenAbsent = symbol.getAnnotationOf[whenAbsent[?]].map {
        case '{ `whenAbsent`($value: E) } => value
        case '{ `whenAbsent`($value: e) } =>
          report.error(s"whenAbsent should have value with type ${Type.show[e]}")
          '{ ??? }
      }
      def fromOptionalParam = Option.when(symbol.hasAnnotationOf[optionalParam]) {
        Expr.summon[OptionLike[E]] match {
          case Some(impl) => '{ $impl.none }
          case None =>
            report.error(s"optionalParam should be used only for types with OptionLike defined")
            '{ ??? }
        }
      }
      def fromDefaultValue = tSymbol.companionModule.methodMembers.collectFirst {
        case m if m.name.startsWith("$lessinit$greater$default$" + (index + 1)) =>
          // todo: genericse
          Ref(m).asExprOf[E]
      }
      fromWhenAbsent orElse fromOptionalParam orElse fromDefaultValue
    }

    def newTFrom(args: List[Expr[?]]): Expr[T] =
      New(TypeTree.of[T])
        .select(tSymbol.primaryConstructor)
        .appliedToArgs(args.map(_.asTerm))
        .asExprOf[T]

    (
      metaTypeOf(tSymbol),
      labelTypeOf(tSymbol, tSymbol.name.stripSuffix("$")), // find a better way than stripping $
      Expr.ofTupleFromSeq(generatedElems),
    ).runtimeChecked match {
      case (
            '[type meta <: Meta; meta],
            '[type label <: String; label],
            '{ type generatedElems <: Tuple; $generatedElemsExpr: generatedElems },
          ) =>
        def deriveSingleton = Option.when(tTpe.isSingleton || tTpe <:< TypeRepr.of[Unit]) {
          Type.of[T] match {
            case '[type s <: scala.Singleton; s] =>
              '{
                new DerMirror.Singleton {
                  type MirroredType = s
                  type MirroredLabel = label
                  type Metadata = meta
                  type GeneratedElems = generatedElems

                  def generatedElems: GeneratedElems = $generatedElemsExpr
                  def value: s = singleValueOf[s]
                }.asInstanceOf[
                  DerMirror.SingletonOf[T] {
                    type MirroredLabel = label
                    type Metadata = meta
                    type GeneratedElems = generatedElems
                  },
                ]
              }
            case '[Unit] =>
              '{
                new DerMirror.Singleton {
                  type MirroredType = Unit
                  type MirroredLabel = label
                  type Metadata = meta
                  type GeneratedElems = generatedElems

                  def generatedElems: GeneratedElems = $generatedElemsExpr
                  def value: Unit = ()
                }.asInstanceOf[
                  DerMirror.SingletonOf[T] {
                    type MirroredLabel = label
                    type Metadata = meta
                    type GeneratedElems = generatedElems
                  },
                ]
              }
          }
        }

        def deriveTransparent = Option.when(tSymbol.hasAnnotation(TypeRepr.of[transparent].typeSymbol)) {
          if (generatedElems.nonEmpty)
            report.errorAndAbort(
              "@generated members are not supported in transparent mirrors",
              tSymbol.pos.getOrElse(Position.ofMacroExpansion),
            )

          derFieldOf(singleCaseFieldOf(tSymbol)) match {
            case '{
                  type fieldType
                  type derFieldElem <: DerFieldElem.Of[fieldType]
                  $derFieldExpr: derFieldElem
                } =>
              '{
                val tw = TransparentWrapping.derived[fieldType, T]

                new TransparentWorkaround[T, fieldType] {
                  type MirroredLabel = label
                  type Metadata = meta

                  type MirroredElems = derFieldElem *: EmptyTuple
                  def mirroredElems = $derFieldExpr *: EmptyTuple

                  def unwrap(value: MirroredType): MirroredElemType = tw.unwrap(value)
                  def wrap(value: MirroredElemType): MirroredType = tw.wrap(value)
                }: DerMirror.TransparentOf[T, fieldType] {
                  type MirroredLabel = label
                  type Metadata = meta
                  type MirroredElems = derFieldElem *: EmptyTuple
                }
              }
          }
        }

        def deriveValueClass = Option.when(tTpe <:< TypeRepr.of[AnyVal]) {
          derFieldOf(singleCaseFieldOf(tSymbol)) match {
            case '{
                  type fieldType
                  type derFieldElem <: DerFieldElem.Of[fieldType]
                  $derFieldExpr: derFieldElem
                } =>
              '{
                new DerMirror.Product {
                  type MirroredLabel = label
                  type MirroredType = T
                  type Metadata = meta

                  type MirroredElems = derFieldElem *: EmptyTuple
                  def mirroredElems: MirroredElems = $derFieldExpr *: EmptyTuple

                  type GeneratedElems = generatedElems
                  def generatedElems: GeneratedElems = $generatedElemsExpr

                  def fromUnsafeArray(product: Array[Any]): T =
                    ${ newTFrom(List('{ product(0).asInstanceOf[fieldType] })) }
                }: DerMirror.ProductOf[T] {
                  type MirroredLabel = label
                  type Metadata = meta
                  type MirroredElems = derFieldElem *: EmptyTuple
                  type GeneratedElems = generatedElems
                }
              }
          }
        }

        def deriveProduct = Expr.summon[Mirror.ProductOf[T]].map {
          case '{
                type mirroredElemTypes <: Tuple
                type label <: String;

                $m: Mirror.ProductOf[T] {
                  type MirroredLabel = label
                  type MirroredElemTypes = mirroredElemTypes
                }
              } =>

            val elems = Expr.ofTupleFromSeq(
              tSymbol.caseFields.zipWithIndex
                .zip(traverseTuple(Type.of[mirroredElemTypes]))
                .map {
                  case ((fieldSymbol, index), '[fieldTpe]) =>
                    (labelTypeOf(fieldSymbol, fieldSymbol.name), metaTypeOf(fieldSymbol)).runtimeChecked match {
                      case ('[type elemLabel <: String; elemLabel], '[type meta <: Meta; meta]) =>
                        '{
                          new DerFieldElem {
                            type MirroredType = fieldTpe
                            type MirroredLabel = elemLabel
                            type Metadata = meta

                            def default = ${ defaultOf[fieldTpe](index, fieldSymbol) }
                          }
                        }
                    }
                  case (_, _) => wontHappen
                },
            )

            elems match {
              case '{ type mirroredElems <: Tuple; $mirroredElemsExpr: mirroredElems } =>
                '{
                  new DerMirror.Product {
                    type MirroredType = T
                    type MirroredLabel = label
                    type Metadata = meta
                    type MirroredElems = mirroredElems

                    def mirroredElems = $mirroredElemsExpr
                    def fromUnsafeArray(product: Array[Any]): T = $m.fromProduct(Tuple.fromArray(product))

                    type GeneratedElems = generatedElems
                    def generatedElems: GeneratedElems = $generatedElemsExpr
                  }: DerMirror.ProductOf[T] {
                    type MirroredLabel = label
                    type Metadata = meta
                    type MirroredElems = mirroredElems
                    type GeneratedElems = generatedElems
                  }
                }
            }
        }

        def deriveSum = Expr.summon[Mirror.SumOf[T]].map {
          case '{
                type mirroredElemTypes <: Tuple
                type label <: String;

                $_ : Mirror.SumOf[T] {
                  type MirroredLabel = label
                  type MirroredElemTypes = mirroredElemTypes
                }
              } =>

            val elems = Expr.ofTupleFromSeq(traverseTuple(Type.of[mirroredElemTypes]).map { case '[subType] =>
              val subType = TypeRepr.of[subType]
              val subSymbol = if (subType.termSymbol.isNoSymbol) subType.typeSymbol else subType.termSymbol

              (labelTypeOf(subSymbol, subSymbol.name), metaTypeOf(subSymbol)).runtimeChecked match {
                case ('[type elemLabel <: String; elemLabel], '[type meta <: Meta; meta]) =>
                  Type.of[subType] match {
                    case '[type s <: scala.Singleton; s] => '{
                      new DerSubSingletonElem {
                        type MirroredType = s
                        type MirroredLabel = elemLabel
                        type Metadata = meta

                        def value: s = singleValueOf[s]
                      }
                    }
                    case '[s] =>
                      '{
                        new DerSubElem {
                          type MirroredType = subType
                          type MirroredLabel = elemLabel
                          type Metadata = meta
                        }
                      }
                  }
              }
            })

            elems match {
              case '{
                    type mirroredElems <: Tuple; $mirroredElemsExpr: mirroredElems
                  } => {
                '{
                  new DerMirror.Sum {
                    type MirroredType = T
                    type MirroredLabel = label
                    type Metadata = meta
                    type MirroredElems = mirroredElems
                    def mirroredElems = $mirroredElemsExpr

                    type GeneratedElems = generatedElems
                    def generatedElems: GeneratedElems = $generatedElemsExpr
                  }: DerMirror.SumOf[T] {
                    type MirroredLabel = label
                    type Metadata = meta
                    type MirroredElems = mirroredElems
                    type GeneratedElems = generatedElems
                  }
                }
              }
              case '{ $_ : x } => report.errorAndAbort(s"Unexpected Mirror type: ${Type.show[x]}")
            }

          case x => report.errorAndAbort(s"Unexpected Mirror type: ${x.show}")
        }

        deriveSingleton orElse deriveTransparent orElse deriveValueClass orElse deriveProduct orElse deriveSum getOrElse {
          report.errorAndAbort(s"Unsupported Mirror type for ${tTpe.show}")
        }
    }
  }

  sealed trait Product extends DerMirror {
    def fromUnsafeArray(product: Array[Any]): MirroredType
  }
  sealed trait Sum extends DerMirror
  sealed trait Singleton extends DerMirror {
    final type MirroredElems = EmptyTuple
    def value: MirroredType
    final def mirroredElems: MirroredElems = EmptyTuple
  }

  sealed trait Transparent extends DerMirror {
    final type GeneratedElems = EmptyTuple
    type MirroredElemType
    type MirroredElems <: DerElem.Of[MirroredElemType] *: EmptyTuple
    def unwrap(value: MirroredType): MirroredElemType
    def wrap(value: MirroredElemType): MirroredType
    final def generatedElems: GeneratedElems = EmptyTuple
  }

  // workaround for https://github.com/scala/scala3/issues/25245
  sealed trait TransparentWorkaround[T, U] extends DerMirror.Transparent {
    final type MirroredType = T
    final type MirroredElemType = U
  }
}
