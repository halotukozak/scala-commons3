package com.avsystem.commons
package serialization

trait GenCodecFailures { this: GenCodec.type =>
  class ReadFailure(msg: String, cause: Throwable | Null) extends RuntimeException(msg, cause) {
    def this(msg: String) = this(msg, null)

    override def fillInStackTrace(): Throwable =
      if (cause == null) super.fillInStackTrace() else this
  }
  case class MissingField(typeRepr: String, fieldName: String)
    extends ReadFailure(s"Cannot read $typeRepr, field $fieldName is missing in decoded data")
  case class UnknownCase(typeRepr: String, caseName: String)
    extends ReadFailure(s"Cannot read $typeRepr, unknown case: $caseName")

  case class MissingCase(typeRepr: String, caseFieldName: String, fieldToRead: Opt[String])
    extends ReadFailure(fieldToRead match {
      case Opt(fr) => s"Cannot read field $fr of $typeRepr before $caseFieldName field is read"
      case Opt.Empty => s"Cannot read $typeRepr, $caseFieldName field is missing"
    })
  case class NotSingleField(typeRepr: String, empty: Boolean)
    extends ReadFailure(
      s"Cannot read $typeRepr, expected object with exactly one field but got " +
        (if (empty) "empty object" else "more than one"),
    )
  case class CaseReadFailed(typeRepr: String, caseName: String, cause: Throwable)
    extends ReadFailure(s"Failed to read case $caseName of $typeRepr", cause)
  case class FieldReadFailed(typeRepr: String, fieldName: String, cause: Throwable)
    extends ReadFailure(s"Failed to read field $fieldName of $typeRepr", cause)
  case class ListElementReadFailed(idx: Int, cause: Throwable)
    extends ReadFailure(s"Failed to read list element at index $idx", cause)
  case class MapFieldReadFailed(fieldName: String, cause: Throwable)
    extends ReadFailure(s"Failed to read map field $fieldName", cause)
  class WriteFailure(msg: String, cause: Throwable | Null) extends RuntimeException(msg, cause) {
    def this(msg: String) = this(msg, null)

    override def fillInStackTrace(): Throwable =
      if (cause == null) super.fillInStackTrace() else this
  }
  case class UnknownWrittenCase[T](typeRepr: String, value: T)
    extends WriteFailure(s"Failed to write $typeRepr: value $value does not match any of known subtypes")
  case class UnapplyFailed(typeRepr: String)
    extends WriteFailure(s"Could not write $typeRepr, unapply/unapplySeq returned false or empty value")
  case class CaseWriteFailed(typeRepr: String, caseName: String, cause: Throwable)
    extends WriteFailure(s"Failed to write case $caseName of $typeRepr", cause)
  case class FieldWriteFailed(typeRepr: String, fieldName: String, cause: Throwable)
    extends WriteFailure(s"Failed to write field $fieldName of $typeRepr", cause)
  case class ListElementWriteFailed(idx: Int, cause: Throwable)
    extends WriteFailure(s"Failed to write list element at index $idx", cause)
  case class MapFieldWriteFailed(fieldName: String, cause: Throwable)
    extends WriteFailure(s"Failed to write map field $fieldName", cause)
}
