## Out-of-scope failures noticed during 05-04

- core/src/test/scala/com/avsystem/commons/misc/SharedExtensionsTest.scala
  - 'sourceCode' — uses SharedExtensions.sourceCode which is a ??? stub (extension method removed in scala-3 fork)
  - 'withSourceCode' — same root cause
  Both pre-exist on 04-05-meta-annotations base. Restore via dedicated source-position macro slice.

