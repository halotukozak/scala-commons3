package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class BasePackageTest extends AnyFunSuite with AnalyzerTest {

  test("base package only") {
    // language=Scala
    assertNoErrors("""
        |package com.avsystem.commons
        |
        |object bar
        |""".stripMargin, onlyRuleWithArg("basePackage", "com.avsystem.commons"))
  }

  test("chained base package") {
    assertNoErrors(
      // language=Scala
      """
        |package com.avsystem
        |package commons
        |
        |object bar
        |""".stripMargin, onlyRuleWithArg("basePackage", "com.avsystem.commons"))
  }

  test("base package with chained subpackage") {
    assertNoErrors(
      // language=Scala
      """
        |package com.avsystem.commons
        |package core
        |
        |object bar
        |""".stripMargin, onlyRuleWithArg("basePackage", "com.avsystem.commons"))
  }

  test("base package object") {
    assertNoErrors(
      // language=Scala
      """
        |package com.avsystem
        |
        |package object commons
        |""".stripMargin, onlyRuleWithArg("basePackage", "com.avsystem.commons"))
  }

  test("base package object with imports") {
    assertNoErrors(
      // language=Scala
      """
        |package com.avsystem
        |
        |import scala.collection.mutable.Seq
        |import scala.collection.mutable.Set
        |
        |package object commons
        |""".stripMargin, onlyRuleWithArg("basePackage", "com.avsystem.commons"))
  }

  test("no base package") {
    assertErrors(1, // language=Scala
      """
        |object bar
        |""".stripMargin, onlyRuleWithArg("basePackage", "my.base.pkg"),
    )
  }

  test("no base package with imports") {
    assertErrors(1, // language=Scala
      """
        |import scala.collection.mutable.Seq
        |import scala.collection.mutable.Set
        |
        |object bar
        |""".stripMargin, onlyRuleWithArg("basePackage", "my.base.pkg"),
    )
  }

  test("wrong base package") {
    assertErrors(1, // language=Scala
      """
        |package com.avsystem.kommons
        |
        |object bar
        |""".stripMargin, onlyRuleWithArg("basePackage", "my.base.pkg"),
    )
  }

  test("unchained subpackage") {
    assertErrors(1, // language=Scala
      """
        |package com.avsystem.commons.core
        |
        |object bar
        |""".stripMargin, onlyRuleWithArg("basePackage", "my.base.pkg"),
    )
  }

  test("unchained subpackage with imports") {
    assertErrors(1, // language=Scala
      """
        |package com.avsystem.commons.core
        |
        |import scala.collection.mutable.Seq
        |import scala.collection.mutable.Set
        |
        |object bar
        |""".stripMargin, onlyRuleWithArg("basePackage", "my.base.pkg"),
    )
  }
}
