package com.avsystem.commons.annotation

import scala.annotation.StaticAnnotation

/**
 * Marker for code or tests left unfinished during the Scala 3 port.
 *  Grep `TodoScala3Migration` to find everything that still needs work:
 *  stubbed macro impls (`???`), ignored test cases, fully disabled files.
 *  `reason` should briefly describe what's missing and what unblocks it.
 */
final class TodoScala3Migration(val reason: String = "") extends StaticAnnotation
