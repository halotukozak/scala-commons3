package com.avsystem.commons.rpc

@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@scala.annotation.nowarn("msg=deprecated")
trait AsRawMacros { this: AsRaw.type =>
  inline def materialize[Raw, Real]: AsRaw[Raw, Real] = ???
  inline def materializeForApi[Raw, Real]: AsRaw[Raw, Real] = ???
}

@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@scala.annotation.nowarn("msg=deprecated")
trait AsRealMacros { this: AsReal.type =>
  inline def materialize[Raw, Real]: AsReal[Raw, Real] = ???
}

@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@scala.annotation.nowarn("msg=deprecated")
trait AsRawRealMacros { this: AsRawReal.type =>
  inline def materialize[Raw, Real]: AsRawReal[Raw, Real] = ???
}

@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@scala.annotation.nowarn("msg=deprecated")
trait RpcMetadataMacros { this: RpcMetadata.type =>
  inline def materialize[M[_], Real]: M[Real] = ???
  inline def materializeForApi[M[_], Real]: M[Real] = ???
  inline def auto[T]: T = ???
}

@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@scala.annotation.nowarn("msg=deprecated")
trait RpcUtilsMacros {
  def compilationError(error: String): Nothing = ???
}

@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@scala.annotation.nowarn("msg=deprecated")
trait RawRpcMacros[Raw] {
  inline def materializeAsRaw[Real]: AsRaw[Raw, Real] = ???
  inline def materializeAsReal[Real]: AsReal[Raw, Real] = ???
  inline def materializeAsRawReal[Real]: AsRawReal[Raw, Real] = ???
  inline def materializeApiAsRaw[Real]: AsRaw[Raw, Real] = ???
}

@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@scala.annotation.nowarn("msg=deprecated")
trait RPCFrameworkMacros {
  inline def materializeAsRaw[T]: AsRaw[?, T] = ???
  inline def materializeAsReal[T]: AsReal[?, T] = ???
  inline def materializeAsRawReal[T]: AsRawReal[?, T] = ???
  inline def materializeMetadata[RealRPC]: Any = ???
  inline def materializeFullInfo[T]: Any = ???
}

@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@scala.annotation.nowarn("msg=deprecated")
trait FullRPCInfoMacros {
  inline implicit def asRealRPC: Any = ???
  inline implicit def asRawRPC: Any = ???
  inline implicit def metadata: Any = ???
}

@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@scala.annotation.nowarn("msg=deprecated")
trait RpcMetadataCompanionMacros[M[_]] {
  inline def materialize[Real]: M[Real] = ???
}

@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@scala.annotation.nowarn("msg=deprecated")
trait ApiMetadataCompanionMacros[M[_]] {
  inline def materialize[Real]: M[Real] = ???
}
