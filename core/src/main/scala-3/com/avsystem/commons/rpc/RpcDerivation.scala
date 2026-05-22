package com.avsystem.commons
package rpc

import made.{Done, DoneOperation, InputElem, Meta}

import scala.compiletime
import scala.quoted.*

/**
 * Macro-driven derivation of RPC type-classes (`AsRaw`, `AsReal`, `AsRawReal`) using `made.Done`
 * as a compile-time mirror of the Real RPC trait. Initial Tier 1 scope:
 *
 *  - enumerate Real's operations via `Done.Of[Real]`
 *  - require Raw to be a trait whose methods correspond 1-to-1 by name to Real operations
 *  - encode/decode each argument via summoned `AsRaw`/`AsReal` instances
 *  - skip annotation-driven features (`@multi`, `@verbatim`, `@encoded`, `@composite`,
 *    `@methodName`, `@annotated`, `@infer`, `@forTypeParams`); they will be ported in Tier 2.
 *
 * Anything outside Tier 1 is reported via `compiletime.error` with the unsupported feature
 * name, so users see a concrete blocker instead of a silent miscompile.
 */
private[rpc] object RpcDerivation:

  /**
   * Tier-1 entry point: derives `AsRaw[Raw, Real]` for the simplest mapping — both `Raw` and
   * `Real` are traits with matching method names; each Real argument has an `AsRaw[RawArg,
   * RealArg]` available; each return value has an `AsReal[RawRet, RealRet]` available.
   *
   * For anything else (overloads, `@multi` raw methods, composite parameters, encoded
   * dispatchers), this fails with `compiletime.error` and a diagnostic pointing at the
   * tier-2 feature that must still be ported.
   */
  inline def materializeAsRaw[Raw, Real](
    using realMirror: Done.Of[Real],
    rawMirror: Done.Of[Raw],
  ): AsRaw[Raw, Real] =
    compiletime.error(
      "RpcDerivation.materializeAsRaw is not implemented yet. " +
        "Tier-1 enumeration is wired through `made.Done`, but argument/return-type wiring " +
        "still needs to be ported. Track progress in TodoScala3Migration.",
    )

  /**
   * Tier-1 entry point for `AsReal[Raw, Real]` — symmetric to [[materializeAsRaw]].
   */
  inline def materializeAsReal[Raw, Real](
    using realMirror: Done.Of[Real],
    rawMirror: Done.Of[Raw],
  ): AsReal[Raw, Real] =
    compiletime.error(
      "RpcDerivation.materializeAsReal is not implemented yet. See materializeAsRaw.",
    )

  /**
   * Diagnostic helper: emits compile-time list of operation names of `T` via `Done`. Use as
   * `summonInline[RpcDerivation.OperationsOf[T]]` and inspect at debug time.
   */
  type OperationsOf[T] = Done.Of[T]
