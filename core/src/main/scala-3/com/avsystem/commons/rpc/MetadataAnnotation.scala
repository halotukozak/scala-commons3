package com.avsystem.commons
package rpc

/**
 * Annotations that extend this trait will be retained for runtime in `RPCMetadata` typeclass instances
 */
@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@scala.annotation.nowarn("msg=deprecated")
trait MetadataAnnotation extends StaticAnnotation
