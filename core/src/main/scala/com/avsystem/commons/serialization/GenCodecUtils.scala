package com.avsystem.commons.serialization

import com.avsystem.commons.BIterable

import scala.collection.Factory
import scala.util.control.NonFatal

trait GenCodecUtils { this: GenCodec.type =>
  extension [A](coll: BIterable[A]) {
    def writeToList(lo: ListOutput)(using writer: GenCodec[A]): Unit = {
      lo.declareSizeOf(coll)
      coll.foreach(new (A => Unit) {
        private var idx = 0
        def apply(a: A): Unit = {
          try writer.write(lo.writeElement(), a)
          catch { case NonFatal(e) => throw ListElementWriteFailed(idx, e) }
          idx += 1
        }
      })
    }
  }

  extension [A, B](coll: BIterable[(A, B)]) {
    def writeToObject(oo: ObjectOutput)(using keyWriter: GenKeyCodec[A], writer: GenCodec[B]): Unit = {
      oo.declareSizeOf(coll)
      coll.foreach { case (key, value) =>
        val fieldName = keyWriter.write(key)
        try writer.write(oo.writeField(fieldName), value)
        catch { case NonFatal(e) => throw MapFieldWriteFailed(fieldName, e) }
      }
    }
  }

  extension (li: ListInput) {
    def collectTo[A: GenCodec, C](using fac: Factory[A, C]): C = {
      val b = fac.newBuilder
      li.knownSize match {
        case -1 =>
        case size => b.sizeHint(size)
      }
      var idx = 0
      while (li.hasNext) {
        val a =
          try read[A](li.nextElement())
          catch { case NonFatal(e) => throw ListElementReadFailed(idx, e) }
        b += a
        idx += 1
      }
      b.result()
    }
  }

  extension (oi: ObjectInput) {
    def collectTo[K: GenKeyCodec, V: GenCodec, C](using fac: Factory[(K, V), C]): C = {
      val b = fac.newBuilder
      oi.knownSize match {
        case -1 =>
        case size => b.sizeHint(size)
      }
      while (oi.hasNext) {
        val fi = oi.nextField()
        val entry =
          try (GenKeyCodec.read[K](fi.fieldName), read[V](fi))
          catch { case NonFatal(e) => throw MapFieldReadFailed(fi.fieldName, e) }
        b += entry
      }
      b.result()
    }
  }

}
