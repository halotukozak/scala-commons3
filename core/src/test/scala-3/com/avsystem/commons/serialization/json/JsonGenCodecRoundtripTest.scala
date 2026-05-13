/* @TodoScala3Migration DISABLED: many small ports needed — explicit-nulls on null literals, 2-arg testRoundtrip overload missing, wildcard _-to-? rewrites, GenCodec derivation for SealedKey / Expr GADTs, etc. Restore once the test-helper API and derivation parity are in place.
package com.avsystem.commons
package serialization.json

import com.avsystem.commons.serialization.{GenCodecRoundtripTest, Input, Output}

class JsonGenCodecRoundtripTest extends GenCodecRoundtripTest {
  type Raw = String

  def writeToOutput(write: Output => Unit): String = {
    val sb = new JStringBuilder
    write(new JsonStringOutput(sb))
    sb.toString
  }

  def createInput(raw: String): Input =
    new JsonStringInput(new JsonReader(raw))
}
*/
