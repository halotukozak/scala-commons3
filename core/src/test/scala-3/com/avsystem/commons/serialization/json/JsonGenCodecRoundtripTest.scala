/* @TodoScala3Migration DISABLED for Scala 3 build — see scala-2.13 version. TODO: port to Scala 3.
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
