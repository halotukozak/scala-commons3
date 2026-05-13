package com.avsystem.commons
package macros

import com.avsystem.commons.misc.{JavaClassName, TypeString}
import org.scalactic.source.Position
import org.scalatest.funsuite.AnyFunSuite

object JavaClassNameTest {
  class Inner {
    class MoreInner {
      class SuperInner
    }
  }
  object Inner {
    class EvenInner
    object EvenInner
  }
}

class JavaClassNameTest extends AnyFunSuite {
  // @TodoScala3Migration: scala-3 TypeString.show collapses `.type`, so JavaClassNameTest vs
  // JavaClassNameTest.type produce the same TypeString. Until materializeImpl distinguishes them,
  // disambiguate test names with JavaClassName.of[T].
  def testCase[T: ClassTag: JavaClassName: TypeString](implicit pos: Position): Unit =
    test(s"${TypeString.of[T]} (${JavaClassName.of[T]})")(
      assert(JavaClassName.of[T] == classTag[T].runtimeClass.getName)
    )

  testCase[Any]
  testCase[AnyRef]
  testCase[AnyVal]
  testCase[Unit]
  testCase[Boolean]
  testCase[Char]
  testCase[Byte]
  testCase[Short]
  testCase[Int]
  testCase[Long]
  testCase[Float]
  testCase[Double]
  testCase[String]
  // @TodoScala3Migration: Scala 3 will not summon ClassTag[Nothing]; restore when the test base provides one explicitly
  // test[Nothing]
  testCase[Array[Boolean]]
  testCase[Array[Char]]
  testCase[Array[Byte]]
  testCase[Array[Short]]
  testCase[Array[Int]]
  testCase[Array[Long]]
  testCase[Array[Float]]
  testCase[Array[Double]]
  testCase[Array[String]]
  // @TodoScala3Migration: Scala 3 will not summon ClassTag[Array[Nothing]]; restore when provided explicitly
  // test[Array[Nothing]]
  testCase[JavaClassNameTest]
  testCase[JavaClassNameTest.type]
  testCase[JavaClassNameTest.Inner]
  testCase[JavaClassNameTest.Inner#MoreInner]
  testCase[JavaClassNameTest.Inner#MoreInner#SuperInner]
  testCase[JavaClassNameTest.Inner.type]
  testCase[JavaClassNameTest.Inner.EvenInner]
  testCase[JavaClassNameTest.Inner.EvenInner.type]
}
