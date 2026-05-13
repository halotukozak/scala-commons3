package com.avsystem.commons
package misc

import org.scalatest.funsuite.AnyFunSuite

object Obj {
  val x: String = "fuu"

  class Inner {
    val x: String = "fuu"

    def valueOfX: x.type = valueOf[x.type]
    def valueOfThis: this.type = valueOf[this.type]
  }
}

class ValueOfTest extends AnyFunSuite {
  test("object") {
    assert(valueOf[Obj.type] == Obj)
  }

  test("static val") {
    assert(valueOf[Obj.x.type] == Obj.x)
  }

  test("inner val of local") {
    val i = new Obj.Inner
    assert(valueOf[i.x.type] == i.x)
    assert(i.valueOfX == i.x)
  }

  test("this") {
    val i = new Obj.Inner
    assert(i.valueOfThis == i)
  }
}
