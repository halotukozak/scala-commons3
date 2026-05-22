package com.avsystem.commons
package serialization

import com.avsystem.commons.serialization.GenCodec.OOOFieldsObjectCodec
import org.scalatest.funsuite.AnyFunSuite

final case class RecordWithDefaults(
  @transientDefault a: String = "",
  b: Int = 42,
) {
  @generated def c: String = s"$a-$b"
}
object RecordWithDefaults extends HasGenCodec[RecordWithDefaults]

final case class RecordWithOpts(
  @optionalParam abc: Opt[String] = Opt.Empty,
  @transientDefault flag: Opt[Boolean] = Opt.Empty,
  b: Int = 42,
)
object RecordWithOpts extends HasGenCodec[RecordWithOpts]

final case class SingleFieldRecordWithOpts(@optionalParam abc: Opt[String] = Opt.Empty)
object SingleFieldRecordWithOpts extends HasGenCodec[SingleFieldRecordWithOpts]

final case class SingleFieldRecordWithTD(@transientDefault abc: String = "abc")
object SingleFieldRecordWithTD extends HasGenCodec[SingleFieldRecordWithTD]

class ObjectSizeTest extends AnyFunSuite {
  extension [T](c: GenCodec[T])
    private def sized: OOOFieldsObjectCodec[T] = c.asInstanceOf[OOOFieldsObjectCodec[T]]

  test("computing object size") {
    assert(RecordWithDefaults.codec.sized.size(RecordWithDefaults()) == 2)
    assert(RecordWithDefaults.codec.sized.size(RecordWithDefaults("fuu")) == 3)
    assert(RecordWithOpts.codec.sized.size(RecordWithOpts("abc".opt)) == 2)
    assert(RecordWithOpts.codec.sized.size(RecordWithOpts("abc".opt, true.opt)) == 3)
    assert(RecordWithOpts.codec.sized.size(RecordWithOpts()) == 1)
    assert(SingleFieldRecordWithOpts.codec.sized.size(SingleFieldRecordWithOpts()) == 0)
    assert(SingleFieldRecordWithOpts.codec.sized.size(SingleFieldRecordWithOpts("abc".opt)) == 1)
    assert(SingleFieldRecordWithTD.codec.sized.size(SingleFieldRecordWithTD()) == 0)
    assert(SingleFieldRecordWithTD.codec.sized.size(SingleFieldRecordWithTD("haha")) == 1)
  }

  test("computing object size with custom output") {
    val defaultIgnoringOutput = new SequentialOutput {
      override def customEvent[T](marker: CustomEventMarker[T], event: T): Boolean =
        marker match {
          case IgnoreTransientDefaultMarker => true
          case _ => super.customEvent(marker, event)
        }
      override def finish(): Unit = ()
    }
    assert(RecordWithDefaults.codec.sized.size(RecordWithDefaults(), defaultIgnoringOutput.opt) == 3)
    assert(RecordWithDefaults.codec.sized.size(RecordWithDefaults("fuu"), defaultIgnoringOutput.opt) == 3)
    assert(RecordWithOpts.codec.sized.size(RecordWithOpts("abc".opt), defaultIgnoringOutput.opt) == 3)
    assert(RecordWithOpts.codec.sized.size(RecordWithOpts("abc".opt, true.opt), defaultIgnoringOutput.opt) == 3)
    assert(RecordWithOpts.codec.sized.size(RecordWithOpts(), defaultIgnoringOutput.opt) == 2)
    assert(
      SingleFieldRecordWithOpts.codec.sized.size(SingleFieldRecordWithOpts(), defaultIgnoringOutput.opt) == 0
    ) // @optionalParam field should NOT be counted
    assert(SingleFieldRecordWithOpts.codec.sized.size(SingleFieldRecordWithOpts("abc".opt), defaultIgnoringOutput.opt) == 1)
    assert(
      SingleFieldRecordWithTD.codec.sized.size(SingleFieldRecordWithTD(), defaultIgnoringOutput.opt) == 1
    ) // @transientDefault field should be counted
    assert(SingleFieldRecordWithTD.codec.sized.size(SingleFieldRecordWithTD("haha"), defaultIgnoringOutput.opt) == 1)
  }
}
