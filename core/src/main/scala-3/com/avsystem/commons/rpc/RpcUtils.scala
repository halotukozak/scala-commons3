package com.avsystem.commons
package rpc

import scala.collection.Factory

@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@scala.annotation.nowarn("msg=deprecated")
class InvalidRpcCall(msg: String, cause: Throwable | Null = null) extends RuntimeException(msg, cause)

@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@scala.annotation.nowarn("msg=deprecated")
class InvalidRpcArgument(val rpcName: String, val argName: String, cause: Throwable)
  extends InvalidRpcCall(s"Argument $argName of RPC $rpcName is invalid: ${cause.getMessage}", cause)

@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@scala.annotation.nowarn("msg=deprecated")
class MissingRpcArgument(val rpcName: String, val argName: String)
  extends InvalidRpcCall(s"Argument $argName of RPC $rpcName is missing")

@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@scala.annotation.nowarn("msg=deprecated")
class UnknownRpc(val rpcName: String, val rawMethodName: String)
  extends InvalidRpcCall(s"Unknown RPC $rpcName for raw method $rawMethodName")

@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@scala.annotation.nowarn("msg=deprecated")
class MissingOptionalRpc(val rawMethodName: String)
  extends InvalidRpcCall(s"No matching RPC for optional raw method $rawMethodName")

@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@scala.annotation.nowarn("msg=deprecated")
object RpcUtils extends RpcUtilsMacros {
  def createEmpty[Coll](fac: Factory[Nothing, Coll]): Coll =
    createBuilder[Nothing, Coll](fac, 0).result()

  def createBuilder[Elem, Coll](fac: Factory[Elem, Coll], size: Int): MBuilder[Elem, Coll] = {
    val b = fac.newBuilder
    b.sizeHint(size)
    b
  }

  def readArg[Raw, Real](rpcName: String, argName: String, asReal: AsReal[Raw, Real], raw: Raw): Real =
    try asReal.asReal(raw)
    catch {
      case NonFatal(cause) => invalidArg(rpcName, argName, cause)
    }

  def invalidArg(rpcName: String, argName: String, cause: Throwable): Nothing =
    throw new InvalidRpcArgument(rpcName, argName, cause)

  def missingArg(rpcName: String, argName: String): Nothing =
    throw new MissingRpcArgument(rpcName, argName)

  def unknownRpc(rpcName: String, rawMethodName: String): Nothing =
    throw new UnknownRpc(rpcName, rawMethodName)

  def missingOptionalRpc(rawMethodName: String): Nothing =
    throw new MissingOptionalRpc(rawMethodName)

  def interceptEnc[NewRaw, Raw, Real](asRaw: AsRaw[NewRaw, Real], interceptor: EncodingInterceptor[NewRaw, Raw])
    : AsRaw[Raw, Real] =
    real => interceptor.toOriginalRaw(asRaw.asRaw(real))

  def interceptDec[NewRaw, Raw, Real](asReal: AsReal[NewRaw, Real], interceptor: DecodingInterceptor[NewRaw, Raw])
    : AsReal[Raw, Real] =
    raw => asReal.asReal(interceptor.toNewRaw(raw))

}
