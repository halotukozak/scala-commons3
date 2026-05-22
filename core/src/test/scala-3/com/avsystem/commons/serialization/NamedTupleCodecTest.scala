package com.avsystem.commons
package serialization

import com.avsystem.commons.serialization.json.JsonStringOutput
import org.scalatest.funsuite.AnyFunSuite

class NamedTupleCodecTest extends AnyFunSuite {
  type Person = (name: String, age: Int)

  // TODO: Made library doesn't unfold NamedTuple[Names, Values] — `tSymbol.caseFields`
  // is empty so the derived Made.Product has Elems = EmptyTuple. Needs Made-side
  // detection of NamedTuple type and synthesis of fields from the Names/Values tuples.
  ignore("NamedTuple should serialize as object") {
    val p: Person = (name = "Alice", age = 30)
    val json = JsonStringOutput.write[Person](p)
    info(json)
    assert(json.contains("name") && json.contains("Alice") && json.contains("age") && json.contains("30"))
  }
}
