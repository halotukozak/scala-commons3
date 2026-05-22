package com.avsystem.commons
package rpc

import com.avsystem.commons.meta.MetadataCompanion

/**
 * Base trait for companion objects of RPC metadata classes.
 *
 * RPC metadata class is a generic class which captures information about some RPC trait's API (its abstract methods).
 * The `materialize` macro is responsible for doing this compile-time reflection. It is steered by various
 * meta-annotations present in the definition of the metadata class, e.g. [[rpcMethodMetadata]].
 *
 * @tparam M
 *   metadata class type constructor
 */
@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@scala.annotation.nowarn("msg=deprecated")
trait RpcMetadataCompanion[M[_]] extends MetadataCompanion[M] with RpcMetadataCompanionMacros[M]

/**
 * Like [[RpcMetadataCompanion]] but reflects over the entire public API of a particular Scala type (unlike RPC traits
 * which only have their abstract methods captured).
 */
@deprecated("RPC framework is not maintained for Scala 3; will be removed in a future release.", since = "3.0.0")
@scala.annotation.nowarn("msg=deprecated")
trait ApiMetadataCompanion[M[_]] extends MetadataCompanion[M] with ApiMetadataCompanionMacros[M]
