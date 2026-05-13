/* DISABLED for Scala 3 build — see scala-2.13 version. TODO: port to Scala 3.
package com.avsystem.commons
package testutil

import com.avsystem.commons.macros.TestMacros
import org.scalatest.Assertions

trait CompilationErrorAssertions extends Assertions {
  def typeErrorFor(code: String): String = macro TestMacros.typeErrorImpl
}
*/
