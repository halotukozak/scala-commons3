package com.avsystem.commons
package misc

import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec}

/**
 * Typeclass that contains string representation of a concrete type. This representation should correctly parse and
 * typecheck when used as a type in Scala source code.
 *
 * Instances of `TypeString` are implicitly macro-materialized. The macro will fail if the type contains references to
 * local symbols, i.e. symbols that only exist in limited scope and cannot be referred to from any place in source
 * code. This includes type parameters, this-references to enclosing classes, etc.
 *
 * For example, the code below will NOT compile:
 * {{{
 *   def listTypeRepr[T]: String = TypeString.of[List[T]]
 * }}}
 * because `T` is a local symbol that only has meaning inside its own method. However, if you provide external
 * `TypeString` instance for `T`, the macro will pick it up and no longer complain:
 * {{{
 *   def listTypeRepr[T: TypeString]: String = TypeString.of[List[T]]
 * }}}
 * Then, `listTypeRepr[Int]` will produce a string `"List[Int]"`
 */
class TypeString[T](val value: String) extends AnyVal {
  override def toString: String = value
}
object TypeString {
  inline given [T] => TypeString[T] = ${ materializeImpl[T] }
  def of[T: TypeString]: String = TypeString[T].value
  def apply[T](using ts: TypeString[T]): TypeString[T] = ts
  @deprecatedName("keyCodec", since = "3.0.0")
  given [T] => GenKeyCodec[TypeString[T]] =
    GenKeyCodec.create[TypeString[T]](new TypeString(_), _.value)
  @deprecatedName("codec", since = "3.0.0")
  given [T] => GenCodec[TypeString[T]] =
    GenCodec.createSimple[TypeString[T]](i => new TypeString(i.readString()), (o, ts) => o.writeString(ts.value))
  private def materializeImpl[T: Type](using quotes: Quotes) = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[T].dealias
    val typeString = Expr(tpe.show(using Printer.TypeReprShortCode))
    '{ new TypeString[T]($typeString) }
  }
}

/**
 * Typeclass that contains JVM fully qualified class name corresponding to given type. `JavaClassName.of[T]` is always
 * equal to `classTag[T].runtimeClass.getName`
 *
 * `JavaClassName` can be used instead of `ClassTag` in ScalaJS when ScalaJS linker is configured to drop class names.
 * Also, unlike `ClassTag`, `JavaClassName` contains just a string so it can be easily serialized and deserialized.
 */
class JavaClassName[T](val value: String) extends AnyVal {
  override def toString: String = value
}
object JavaClassName extends JavaClassNameLowPriority {
  def apply[T](using ts: JavaClassName[T]): JavaClassName[T] = ts
  def of[T: JavaClassName]: String = JavaClassName[T].value
  @deprecatedName("NothingClassName", since = "3.0.0")
  given JavaClassName[Nothing] = new JavaClassName("scala.runtime.Nothing$")
  @deprecatedName("NothingArrayClassName", since = "3.0.0")
  given JavaClassName[Array[Nothing]] = new JavaClassName("[Lscala.runtime.Nothing$;")
  @deprecatedName("UnitClassName", since = "3.0.0")
  given JavaClassName[Unit] = new JavaClassName("void")
  @deprecatedName("BooleanClassName", since = "3.0.0")
  given JavaClassName[Boolean] = new JavaClassName("boolean")
  @deprecatedName("ByteClassName", since = "3.0.0")
  given JavaClassName[Byte] = new JavaClassName("byte")
  @deprecatedName("ShortClassName", since = "3.0.0")
  given JavaClassName[Short] = new JavaClassName("short")
  @deprecatedName("IntClassName", since = "3.0.0")
  given JavaClassName[Int] = new JavaClassName("int")
  @deprecatedName("LongClassName", since = "3.0.0")
  given JavaClassName[Long] = new JavaClassName("long")
  @deprecatedName("FloatClassName", since = "3.0.0")
  given JavaClassName[Float] = new JavaClassName("float")
  @deprecatedName("DoubleClassName", since = "3.0.0")
  given JavaClassName[Double] = new JavaClassName("double")
  @deprecatedName("CharClassName", since = "3.0.0")
  given JavaClassName[Char] = new JavaClassName("char")
  @deprecatedName("AnyClassName", since = "3.0.0")
  given JavaClassName[Any] = new JavaClassName("java.lang.Object")
  @deprecatedName("AnyValClassName", since = "3.0.0")
  given JavaClassName[AnyVal] = new JavaClassName("java.lang.Object")
  @deprecatedName("arrayClassName", since = "3.0.0")
  given [T: JavaClassName] => JavaClassName[Array[T]] = {
    val elementName = JavaClassName.of[T] match {
      case "void" => "Lscala.runtime.BoxedUnit;"
      case "boolean" => "Z"
      case "byte" => "B"
      case "short" => "S"
      case "int" => "I"
      case "long" => "J"
      case "float" => "F"
      case "double" => "D"
      case "char" => "C"
      case arr if arr.startsWith("[") => arr
      case n => s"L$n;"
    }
    new JavaClassName("[" + elementName)
  }
  @deprecatedName("keyCodec", since = "3.0.0")
  given GenKeyCodec[JavaClassName[?]] =
    GenKeyCodec.create[JavaClassName[?]](new JavaClassName(_), _.value)
  @deprecatedName("codec", since = "3.0.0")
  given GenCodec[JavaClassName[?]] =
    GenCodec.createSimple[JavaClassName[?]](i => new JavaClassName(i.readString()), (o, ts) => o.writeString(ts.value))
}

trait JavaClassNameLowPriority { this: JavaClassName.type =>
  @deprecatedName("materialize", since = "3.0.0")
  inline given derived[T]: JavaClassName[T] = ${ derivedImpl[T] }
}

def derivedImpl[T: Type](using quotes: Quotes) = {
  import quotes.reflect.*
  def javaClassName(sym: Symbol): String = {
    val nameSuffix = if (sym.flags.is(Flags.Module) && !sym.flags.is(Flags.Package)) "$" else ""
    val selfName = sym.name + nameSuffix
    val owner = sym.owner
    val prefix =
      if (owner == defn.RootClass) ""
      else if (owner.flags.is(Flags.Package)) javaClassName(owner) + "."
      else if (owner.flags.is(Flags.Module)) javaClassName(owner)
      else javaClassName(owner) + "$"
    prefix + selfName
  }

  val tpe = TypeRepr.of[T].dealias
  if (tpe.typeSymbol.isClassDef && tpe.typeSymbol != defn.ArrayClass) {
    val name = Expr(javaClassName(tpe.typeSymbol))
    '{ new JavaClassName[T]($name) }
  } else
    report.errorAndAbort(s"${Type.show[T]} does not represent a regular class")
}
