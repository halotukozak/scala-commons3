package com.avsystem.commons
package rpc

import org.scalatest.funsuite.AnyFunSuite

class RpcDerivationTest extends AnyFunSuite {

  trait Real {
    def hello(name: String): String
    def add(x: Int, y: Int): Int
    def ping(): Unit
  }

  trait Raw {
    def hello(name: String): String
    def add(x: Int, y: Int): Int
    def ping(): Unit
  }

  test("echo-proxy AsRaw delegates each method to the real instance") {
    var pingCount = 0
    val real: Real = new Real {
      def hello(name: String): String = s"hello $name"
      def add(x: Int, y: Int): Int = x + y
      def ping(): Unit = pingCount += 1
    }

    val asRaw: AsRaw[Raw, Real] = RpcDerivation.materializeAsRaw[Raw, Real]
    val raw: Raw = asRaw.asRaw(real)

    assert(raw.hello("world") == "hello world")
    assert(raw.add(2, 3) == 5)
    raw.ping()
    raw.ping()
    assert(pingCount == 2)
  }
}
