package com.avsystem.commons
package rpc

/**
 * Base trait for companion objects of raw RPC traits.
 */
@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@scala.annotation.nowarn("msg=deprecated")
trait RawRpcCompanion[Raw] extends RawRpcMacros[Raw] {
  type AsRawRpc[Real] = AsRaw[Raw, Real]
  type AsRealRpc[Real] = AsReal[Raw, Real]
  type AsRawRealRpc[Real] = AsRawReal[Raw, Real]

  def asRealRpc[Real](using asReal: AsRealRpc[Real]): AsRealRpc[Real] = asReal
  def asRawRpc[Real](using asRaw: AsRawRpc[Real]): AsRawRpc[Real] = asRaw
  def asRawRealRpc[Real](using asRawReal: AsRawRealRpc[Real]): AsRawRealRpc[Real] = asRawReal

  def asReal[Real](raw: Raw)(using asRealRpc: AsRealRpc[Real]): Real = asRealRpc.asReal(raw)
  def asRaw[Real](real: Real)(using asRawRpc: AsRawRpc[Real]): Raw = asRawRpc.asRaw(real)
}
