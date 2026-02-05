package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class ImplicitTypesTest extends AnyFunSuite with AnalyzerTest:
  test("placeholder test"):
    assertNoErrors(scala"val x = 1")
