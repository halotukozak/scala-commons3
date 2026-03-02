package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class ImplicitFunctionParamsTest extends AnyFunSuite with AnalyzerTest {

  // Define a SAM type (Single Abstract Method) for all SAM type tests
  private val SamDefinition =
    // language=Scala
    """|
       |trait IntToString {     
       |  def apply(i: Int): String
       |}      
       |""".stripMargin

  test("regular parameter with function type should pass") {
    assertNoErrors(scala"def goodMethod1(f: Int => String): Unit = ???")
  }

  test("regular parameter with partial function type should pass") {
    assertNoErrors(scala"def goodMethod2(pf: PartialFunction[Int, String]): Unit = ???")
  }
  test("regular class parameter with function type should pass") {
    assertNoErrors(scala"class GoodClass1(f: Int => String)")
  }

  test("regular class parameter with partial function type should pass") {
    assertNoErrors(scala"class GoodClass2(pf: PartialFunction[Int, String])")
  }

  test("regular parameter with SAM type should pass") {
    assertNoErrors(scala"$SamDefinition def goodMethod1(f: IntToString): Unit = ???")
  }

  test("regular class parameter with SAM type should pass") {
    assertNoErrors(scala"$SamDefinition class GoodClass1(f: IntToString)")
  }

  test("regular parameter with Function2 type should pass") {
    assertNoErrors(scala"def goodMethod1(f: (Int, String) => Boolean): Unit = ???")
  }

  test("regular parameter with Function3 type should pass") {
    assertNoErrors(scala"def goodMethod2(f: (Int, String, Double) => Boolean): Unit = ???")
  }

  test("regular class parameter with Function2 type should pass") {
    assertNoErrors(scala"class GoodClass1(f: (Int, String) => Boolean)")
  }

  test("regular class parameter with Function3 type should pass") {
    assertNoErrors(scala"class GoodClass2(f: (Int, String, Double) => Boolean)")
  }

  for (keyword <- Seq("implicit", "using")) {

    test(s"$keyword parameter with non-function type should pass") {
      assertNoErrors(scala"def goodMethod3($keyword s: String): Unit = ???")
    }

    test(s"$keyword parameter with function type should fail") {
      assertErrors(1, scala"def badMethod1($keyword f: Int => String): Unit = ???")
    }

    test(s"$keyword  parameter with function type in second parameter list should fail") {
      assertErrors(1, scala"def badMethod2(x: Int)($keyword  f: Int => String): Unit = ???")
    }

    test(s"$keyword  parameter with partial function type should fail") {
      assertErrors(1, scala"def badMethod3($keyword  pf: PartialFunction[Int, String]): Unit = ???")
    }

    test(s"$keyword  parameter with partial function type in second parameter list should fail") {
      assertErrors(1, scala"def badMethod4(x: Int)($keyword  pf: PartialFunction[Int, String]): Unit = ???")
    }

    test(s"$keyword  class parameter with non-function type should pass") {
      assertNoErrors(scala"class GoodClass3($keyword  s: String)")
    }

    test(s"$keyword  class parameter with function type should fail") {
      assertErrors(1, scala"class BadClass1($keyword  f: Int => String)")
    }

    test(s"$keyword  class parameter with function type in second parameter list should fail") {
      assertErrors(1, scala"class BadClass2(x: Int)($keyword  f: Int => String)")
    }

    test(s"$keyword  class parameter with partial function type should fail") {
      assertErrors(1, scala"class BadClass3($keyword  pf: PartialFunction[Int, String])")
    }

    test(s"$keyword  class parameter with partial function type in second parameter list should fail") {
      assertErrors(1, scala"class BadClass4(x: Int)($keyword  pf: PartialFunction[Int, String])")
    }

    test(s"$keyword  parameter with SAM type should pass") {
      assertNoErrors(scala"$SamDefinition def goodMethod2($keyword  f: IntToString): Unit = ???")
    }

    test(s"$keyword  parameter with SAM type in second parameter list should pass") {
      assertNoErrors(scala"$SamDefinition def goodMethod3(x: Int)($keyword  f: IntToString): Unit = ???")
    }

    test(s"$keyword  class parameter with SAM type should pass") {
      assertNoErrors(scala"$SamDefinition class GoodClass2($keyword  f: IntToString)")
    }

    test(s"$keyword  class parameter with SAM type in second parameter list should pass") {
      assertNoErrors(scala"$SamDefinition class GoodClass3(x: Int)($keyword  f: IntToString)")
    }

    test(s"$keyword  parameter with non-function type should pass (multiple params context)") {
      assertNoErrors(scala"def goodMethod3($keyword  s: String): Unit = ???")
    }

    test(s"$keyword  parameter with Function2 type should fail") {
      assertErrors(1, scala"def badMethod1($keyword  f: (Int, String) => Boolean): Unit = ???")
    }

    test(s"$keyword  parameter with Function2 type in second parameter list should fail") {
      assertErrors(1, scala"def badMethod2(x: Int)($keyword  f: (Int, String) => Boolean): Unit = ???")
    }

    test(s"$keyword  parameter with Function3 type should fail") {
      assertErrors(1, scala"def badMethod3($keyword  f: (Int, String, Double) => Boolean): Unit = ???")
    }

    test(s"$keyword  parameter with Function3 type in second parameter list should fail") {
      assertErrors(1, scala"def badMethod4(x: Int)($keyword  f: (Int, String, Double) => Boolean): Unit = ???")
    }

    test(s"$keyword  class parameter with non-function type should pass (multiple params context)") {
      assertNoErrors(scala"class GoodClass3($keyword  s: String)")
    }

    test(s"$keyword  class parameter with Function2 type should fail") {
      assertErrors(1, scala"class BadClass1($keyword  f: (Int, String) => Boolean)")
    }

    test(s"$keyword  class parameter with Function2 type in second parameter list should fail") {
      assertErrors(1, scala"class BadClass2(x: Int)($keyword  f: (Int, String) => Boolean)")
    }

    test(s"$keyword  class parameter with Function3 type should fail") {
      assertErrors(1, scala"class BadClass3($keyword  f: (Int, String, Double) => Boolean)")
    }

    test(s"$keyword  class parameter with Function3 type in second parameter list should fail") {
      assertErrors(1, scala"class BadClass4(x: Int)($keyword  f: (Int, String, Double) => Boolean)")
    }
  }
}
