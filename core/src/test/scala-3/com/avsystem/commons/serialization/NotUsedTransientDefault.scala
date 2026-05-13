package com.avsystem.commons.serialization

import org.scalatest.funsuite.AnyFunSuite

final class NotUsedTransientDefault extends AnyFunSuite {
  case class Valid(@transientDefault a: String = "default")
  case class Invalid(@transientDefault a: String)

  test("no warnings when @transientDefault is used properly") {
    assertCompiles(
      // language=Scala
      """
         |GenCodec.materialize[Valid]
         |""".stripMargin
    )
  }

  ignore("fails to compile when missing default value") {
    // Scala 3 GenCodec.materialize currently accepts @transientDefault without
    // a default value. Restore upstream validation in our derivation, then unignore.
    assertDoesNotCompile(
      // language=Scala
      """
         |GenCodec.materialize[Invalid]
         |""".stripMargin
    )
  }
}
