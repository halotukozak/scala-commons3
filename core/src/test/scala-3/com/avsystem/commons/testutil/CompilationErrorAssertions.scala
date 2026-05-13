package com.avsystem.commons
package testutil

import org.scalatest.Assertions

import scala.compiletime.testing.{Error, typeCheckErrors}

trait CompilationErrorAssertions extends Assertions {
  inline def typeErrorFor(inline code: String): String =
    typeCheckErrors(code) match {
      case Nil => fail(s"Expected compilation error for: $code")
      case errs => errs.map(_.message).mkString("\n")
    }
}
