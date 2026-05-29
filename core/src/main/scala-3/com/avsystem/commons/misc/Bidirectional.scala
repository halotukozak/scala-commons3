package com.avsystem.commons
package misc

/**
 * Creates reversed partial function.
 *
 * @deprecated
 *   The Scala 2 `Bidirectional` macro has not been ported to Scala 3 and will not be. Implement a reversed partial
 *   function manually instead of relying on this stub, which always fails at use site.
 */
@deprecated("Bidirectional macro not ported to Scala 3 — write the reversed PartialFunction manually.", since = "3.0.0")
object Bidirectional {
  inline def apply[A, B](pf: PartialFunction[A, B]): (PartialFunction[A, B], PartialFunction[B, A]) =
    scala.compiletime.error(
      "com.avsystem.commons.misc.Bidirectional has not been ported to Scala 3. Write the reversed PartialFunction manually.",
    )
}
