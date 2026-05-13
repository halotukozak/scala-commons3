package com.avsystem.commons
package misc

import org.scalatest.funsuite.AnyFunSuite

class DelegationTest extends AnyFunSuite {
  trait Destination[T] {
    val te: T
    def simple(omg: Int): String
    def meth[C[+X] <: Iterable[X]](map: Map[T, C[String]]): C[(T, String)]
    def multi(a: String)(b: String): String
    def vararg(values: String*): String
  }

  class Source {
    val te: Double = 3.14
    def simple(omg: Int): String = omg.toString
    def meth[C[+X] <: Iterable[X]](map: Map[Double, C[String]]): C[(Double, String)] =
      null.asInstanceOf[C[(Double, String)]]
    def multi(a: String)(b: String): String = a + b
    def vararg(values: String*): String = values.mkString("")
  }

  // @TodoScala3Migration: Delegation.apply / materializeDelegation are stubbed `???` in scala-3 MiscMacros.
  // Re-enable once the macro is ported.
  ignore("simple test") {
    val source = new Source
    val destination = Delegation[Destination[Double]](source)

    assert(source.te == destination.te)
    assert(source.simple(42) == destination.simple(42))
    assert(source.meth[List](Map.empty) == destination.meth[List](Map.empty))
    assert(source.multi("4")("2") == destination.multi("4")("2"))
    assert(source.vararg("4", "2") == destination.vararg("4", "2"))
  }
}
