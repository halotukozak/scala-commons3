/* @TodoScala3Migration DISABLED: many small ports needed — explicit-nulls on null literals, 2-arg testRoundtrip overload missing, wildcard _-to-? rewrites, GenCodec derivation for SealedKey / Expr GADTs, etc. Restore once the test-helper API and derivation parity are in place.
package com.avsystem.commons
package serialization

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

class StreamGenCodecTest extends GenCodecRoundtripTest {
  type Raw = Array[Byte]

  def writeToOutput(write: Output => Unit): Array[Byte] = {
    val baos = new ByteArrayOutputStream
    write(new StreamOutput(new DataOutputStream(baos)))
    baos.toByteArray
  }

  def createInput(raw: Array[Byte]): Input =
    new StreamInput(new DataInputStream(new ByteArrayInputStream(raw)))
}
*/
