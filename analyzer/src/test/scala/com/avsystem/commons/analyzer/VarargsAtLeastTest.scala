package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class VarargsAtLeastTest extends AnyFunSuite with AnalyzerTest {

  // language=Scala
  val need3ParamDefinition =
    """def need3Params(@atLeast(3) params: Int*): Unit = ()"""

  test("too few varargs parameters should be rejected") {
    assertErrors(
      1,
      scala"""
             |$need3ParamDefinition
             |need3Params(1, 2)
             |""".stripMargin,
    )
  }

  test("enough varargs parameters should not be rejected") {
    assertNoErrors(
      scala"""
             |$need3ParamDefinition
             |
             |need3Params(1, 2, 3)
             |need3Params(1, 2, 3, 4)
             |""".stripMargin,
    )
  }

  test("collection passed as varargs parameter should not be rejected") {
    assertNoErrors(
      scala"""
             |$need3ParamDefinition
             |
             |need3Params(List(1, 2)*)
             |""".stripMargin,
    )
  }
}
