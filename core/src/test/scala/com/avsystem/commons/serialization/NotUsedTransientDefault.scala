package com.avsystem.commons.serialization

import org.scalatest.funsuite.AnyFunSuite

final class NotUsedTransientDefault extends AnyFunSuite {
  case class Valid(@transientDefault a: String = "default")
  case class Invalid(@transientDefault a: String)

  test("no warnings when @transientDefault is used properly") {
    assertCompiles(
      // language=Scala
      "GenCodec.materialize[Valid]",
    )
  }

  test("fails to compile when missing default value") {
    assertDoesNotCompile(
      // language=Scala
      "GenCodec.materialize[Invalid]",
    )
  }
}
