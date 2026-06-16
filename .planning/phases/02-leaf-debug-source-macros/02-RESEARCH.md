# Phase 2: Leaf debug/source macros — Research

**Researched:** 2026-06-01
**Domain:** Scala 3 `scala.quoted` + `inline` — porting Scala 2 blackbox macros to Scala 3 inline + quotes
**Confidence:** HIGH (Scala 3 macro patterns; upstream Scala 2 impls in hand) · MEDIUM (per-macro edge-case behaviour)

## User Constraints (from CONTEXT.md)

### Locked Decisions

- **PR granularity** — 1 PR per logical concern (~4 PRs total). Proposed slicing:
  1. **debug-reify** — `SharedExtensions.show*` (10 macros) + `sourceCode` + `withSourceCode`
  2. **source-positions** — `positioned.here` + `SourceInfo.here`
  3. **implicit-lookup** — `Implicits.infer` / `infer(clue)` / `inferNonMacro`
  4. **class-name + SAM** — `SimpleClassName.materialize` + `Sam.apply` / `SamCompanion.apply` / `isValidSam`
  - Each PR self-contained, no cross-PR dependency on this phase's other slices.
- **Macro style** — Mix per macro at Claude's discretion. Default `inline def` + `${ impl[T] }`. Use `transparent inline` only where call-site type refinement matters (e.g. `SimpleClassName.materialize`).
- **Test policy** — Un-comment matching test file from Phase 1's `/* */` wrap; add minimal smoke test if no original existed. Restored test must pass on `01-big-bang`.
- **Branch strategy** — Each PR off `01-big-bang` (PR #860 base). Land in parallel. `[Scala 3]` title prefix, milestone "Scala 3" (#1), draft on open.
- **MIGRATION.md policy** — Each PR removes restored entries from `## Backlog` and updates `Total tags: N`. No "completed" markers.

### Claude's Discretion

- Exact `Quotes` API call patterns (`Type.of[T]`, `TypeRepr.of[T]`, `Expr.summon`, etc.)
- Error-reporting style (`report.error` vs `report.errorAndAbort` vs `Expr[Nothing]`)
- `inline def` wrapper + non-inline helper vs everything in macro body
- `???` stub removal — direct replacement vs incremental
- Edge cases in `show*` (constructor vs method `Symbol` rendering)
- Cribbing upstream Scala 2 impl: translate idiomatically, don't transliterate

### Deferred Ideas (OUT OF SCOPE)

- `TypeString` restoration (depends on `SimpleClassName` — Phase 3)
- `meta/` derivation core (MacroInstances, AdtMetadataCompanion, MetadataCompanion, metaAnnotations)
- `AnnotationOf`, `ApplierUnapplier`, `Bidirectional`, `Delegation`, `SealedUtils`, `ValueEnum`
- `analyzer` module re-enable (Scala 3 plugin rewrite — L)
- Test files depending on `meta/` or `serialization/` stubs

## Phase Requirements

The phase has no atomic REQ-IDs in `REQUIREMENTS.md` (Phase 2+ uses "requirements defined per restoration PR"). Effective per-slice requirements derived from Backlog rows and CONTEXT.md:

| Slice ID | Description | Research Support |
|----------|-------------|-----------------|
| **DEBUG-01** | Restore `SharedExtensions.show{Ast,RawAst,Symbol,SymbolFullName,Type,RawType,TypeSymbol,TypeSymbolFullName}` | Code Example — show* family using `quotes.reflect` + `report.info` |
| **DEBUG-02** | Restore `SharedExtensions.sourceCode` + `withSourceCode` | Code Example — `Position.sourceCode` via macro on the receiver expression |
| **POS-01** | Restore `annotation.positioned.here` (`def here: Int`) | Code Example — `Position.startLine`/`.start` from caller via `quotes.reflect.Position.ofMacroExpansion` |
| **POS-02** | Restore `misc.SourceInfo.here` (implicit `SourceInfo`) | Code Example — same position API + symbol owner chain |
| **IMPL-01** | Restore `misc.Implicits.infer[T]` / `infer[T](clue: String)` / `inferNonMacro[T](clue: String)` | Code Example — `Expr.summon[T]` + `report.errorAndAbort` |
| **CLS-01** | Restore `misc.SimpleClassName.materialize[T]` | Code Example — `TypeRepr.of[T].typeSymbol.name` |
| **SAM-01** | Restore `misc.Sam.apply` / `SamCompanion.apply` / `isValidSam` (**or skip** — deprecated, see Pitfall #6) | Code Example — abstract-method enumeration + synthesized class literal |

## Summary

Phase 2 ports 6 leaf-macro concerns from `= ???` stubs to Scala 3 `inline def` + `scala.quoted` impls. Every target macro is **blackbox-equivalent** (no whitebox return-type inference except `SimpleClassName.materialize` which conceptually wants `SimpleClassName[T]`) — they map cleanly onto `inline def` + `${ impl[T] }(...)` with a `def impl[T: Type](using Quotes): Expr[R]` helper.

The Scala 2 reference impls live on `upstream/master`:
- `macros/.../UniversalMacros.scala` — the 10 `show*` + `sourceCode` + `withSourceCode` family (each peels `c.prefix.tree`, emits `c.error` with reflection-rendered string, returns prefix unchanged).
- `macros/.../misc/MiscMacros.scala` — `infer` / `clueInfer` / `inferNonMacro` (line ~17), `sourceInfo` (line ~36), `posPoint` (line 499), `simpleClassName` (line 687).
- `macros/.../misc/SamMacros.scala` — `validateSam`, `createSam` (full file).

**Primary recommendation:** Use the Scala 2 impls as **structural blueprints** only; the Scala 3 versions are dramatically shorter because (a) `Expr.summon[T]` replaces `inferImplicitValue`, (b) `Position.sourceCode` replaces hand-coded source-substring extraction, (c) `'{ ... }` quote/splice replaces `q"..."` Tree builders, (d) `report.errorAndAbort` replaces `abort`. Plan to write ≤30 LOC per macro (excluding signature + import). Most macros (`show*`, `posPoint`, `simpleClassName`, `infer`) are ~5–10 LOC each.

**One memory rule to revisit during planning:** `Sam` / `SamCompanion` are both `@deprecated` since 2.28.0 with stdlib SAM conversion as replacement. Memory `feedback_dont_port_deprecated.md` says **skip @deprecated APIs that have stdlib replacements**. SAM-01 should likely **drop the slice** (delete deprecated objects + their tests) rather than port. Planner decision point — flag to user.

## Standard Stack

No external libs added. Everything lives in stdlib Scala 3:

### Core
| API | Module | Purpose | Why Standard |
|-----|--------|---------|--------------|
| `scala.quoted.Quotes` | stdlib | Macro context (passed via `using`) | Only way to write Scala 3 macros |
| `scala.quoted.Expr[T]` / `Type[T]` | stdlib | Quoted code + erased generic types | Replaces Scala 2 `Tree` / `WeakTypeTag` |
| `scala.quoted.Expr.summon[T]` | stdlib | Compile-time implicit search | Replaces `c.inferImplicitValue` |
| `quotes.reflect.*` | stdlib (TASTy reflect) | Symbol/TypeRepr/Position/Tree introspection | Replaces `c.universe._` |
| `quotes.reflect.report` | stdlib | `info` / `warning` / `error` / `errorAndAbort` | Replaces `c.error` / `c.abort` |

### Supporting (already on classpath)
| API | Purpose |
|-----|---------|
| `Position.sourceCode: Option[String]` | Extract source text of an expression — collapses `sourceCode` macro to 1 line |
| `Position.startLine` / `.startColumn` / `.start` / `.sourceFile.path` / `.sourceFile.name` | All `SourceInfo` fields available directly |
| `TypeRepr.of[T].typeSymbol.name` | `SimpleClassName` impl |
| `TypeRepr.of[T].show` / `Printer.TypeReprCode` / `Printer.TypeReprStructure` | `showType` / `showRawType` rendering |

### Alternatives Considered
| Instead of | Could Use | Tradeoff |
|------------|-----------|----------|
| `inline def` + `${ impl }` | Pure `inline def` (no quotes) | Pure-inline works only for trivial constant-folding; debug/reflection macros need `Quotes` |
| `report.errorAndAbort` | Returning `Expr[Nothing]` via `'{ throw … }` | Aborts at compile-time — strictly better for our cases (callers expect compile errors, not runtime throws) |
| `Position.sourceCode` | Hand-built substring extraction (the Scala 2 approach) | Stdlib API is ~12 LOC shorter and handles indent/range edges correctly |
| `transparent inline def` everywhere | Plain `inline def` | Only `SimpleClassName.materialize` benefits from refined return; rest just return `String`/`Int`/`(A, String)` — plain `inline` is simpler |

**No new dependencies.** No need to bump anything.

**Version verification:** Scala compiler is already pinned at 3.8.2 (per Phase 1 Plan 01). No version bump in Phase 2. `scala.quoted` API is stable across 3.x.

## Architecture Patterns

### Standard macro file layout (per slice)

```
core/src/main/scala/com/avsystem/commons/
├── <package>/
│   ├── <PublicType>.scala          # holds `inline def foo[T] = ${ FooMacros.fooImpl[T] }`
│   └── macros/
│       └── <Concern>Macros.scala   # NEW — holds `def fooImpl[T: Type](using Quotes): Expr[R]`
```

Putting macro impls in a sibling `macros/` package (per concern, NOT the deleted `commons-macros` module) keeps the public API file readable. Single-file consolidation is also acceptable for very small slices (e.g. `positioned.here` — 5-line impl can live inline in the same object).

### Pattern 1: Plain `inline def` + impl helper

**What:** Most common. Caller invokes `inline`; compiler splices the macro result.
**When to use:** Return type is concrete (`String`, `Int`, `Unit`, `(A, String)`, `T` where `T` is a method type param).
**Example (cribbed style):**
```scala
// In SimpleClassName.scala (USER-FACING)
inline given materialize[T]: SimpleClassName[T] = ${ SimpleClassNameMacros.materializeImpl[T] }

// In SimpleClassNameMacros.scala (IMPL)
import scala.quoted.*

object SimpleClassNameMacros:
  def materializeImpl[T: Type](using Quotes): Expr[SimpleClassName[T]] =
    import quotes.reflect.*
    val sym = TypeRepr.of[T].dealias.typeSymbol
    val name = sym.name
    '{ SimpleClassName[T](${ Expr(name) }) }
```

### Pattern 2: `transparent inline` for refined return

**What:** Compiler keeps the precise inferred type at the call site.
**When to use:** Caller benefits from the macro narrowing the return type beyond what the signature states. **In Phase 2, NO target macro actually needs this** — every signature is already concrete. CONTEXT.md hints at `SimpleClassName.materialize` but the result is just `SimpleClassName[T]`, and `T` is already a method type param — plain `inline` suffices. **Recommendation: do NOT use `transparent inline` anywhere in Phase 2.** Save it for derivation macros in Phase 3+.

### Pattern 3: `inline def` consuming receiver via extension

**What:** `show*` / `sourceCode` are members of `UniversalOps[A]` (extension-like AnyVal). Need to lift the receiver `a: A` into a macro.
**When to use:** Macro acts on a method receiver (or first parameter).
**Example:**
```scala
// In SharedExtensions.scala — inside `class UniversalOps[A](private val a: A) extends AnyVal`
inline def showAst: A = ${ ShowMacros.showAstImpl('a) }

// In ShowMacros.scala
def showAstImpl[A: Type](a: Expr[A])(using Quotes): Expr[A] =
  import quotes.reflect.*
  report.info(Printer.TreeCode.show(a.asTerm), a.asTerm.pos)
  a
```
Note: `report.info` (not `report.error` as Scala 2 used) — printing AST as an **error** stopped compilation; `info` shows the message and proceeds. **Decision point for planner:** match Scala 2 behaviour (`error` → halts) or switch to `info` (prints + compiles). Recommendation: `report.info` is more useful (the Scala 2 `error` impl was a hack to surface the message; a proper `info` channel exists in Scala 3).

### Pattern 4: Position-only macro (no type param)

**What:** Macro that just needs `Position.ofMacroExpansion`.
**Example:**
```scala
inline def here: Int = ${ PositionedMacros.posPointImpl }

def posPointImpl(using Quotes): Expr[Int] =
  import quotes.reflect.*
  Expr(Position.ofMacroExpansion.start)
```

### Recommended file layout per slice

| Slice | New files | Modified files |
|-------|-----------|----------------|
| debug-reify | `core/src/main/scala/com/avsystem/commons/macros/ShowMacros.scala` | `SharedExtensions.scala` |
| source-positions | `core/src/main/scala/com/avsystem/commons/annotation/macros/PositionedMacros.scala`, `core/src/main/scala/com/avsystem/commons/misc/macros/SourceInfoMacros.scala` | `positioned.scala`, `SourceInfo.scala` |
| implicit-lookup | `core/src/main/scala/com/avsystem/commons/misc/macros/ImplicitsMacros.scala` | `Implicits.scala` |
| class-name | `core/src/main/scala/com/avsystem/commons/misc/macros/SimpleClassNameMacros.scala` | `SimpleClassName.scala` |
| SAM (if not dropped) | `core/src/main/scala/com/avsystem/commons/misc/macros/SamMacros.scala` | `Sam.scala`, `SamCompanion.scala` |

### Anti-Patterns to Avoid

- **Storing `Quotes` in a field.** Always pass via `using` (per official reflection docs). Path-dependent typing leaks otherwise.
- **`q"..."` syntax.** Doesn't exist in Scala 3. Use `'{ ... }` and `${ ... }`.
- **Re-introducing `commons-macros` module.** Module is deleted (will-not-migrate). Inline macros live in the same compilation unit as their callers.
- **`transparent inline` by default.** Adds compiler complexity, can blow up inference. Only use when actually needed.
- **`@nowarn` / `-Wconf` suppressions.** Memory rule: fix at source.

## Don't Hand-Roll

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Reading source-text of an expression | Hand-coded `source.content.slice(start, end)` + indent stripping | `Position.sourceCode: Option[String]` | Stdlib handles range, multiline indent, source-file lifecycle |
| Implicit-search-at-compile-time | Tree-walking + `Symbol.findImplicit` | `Expr.summon[T]` | Official API, integrates with implicit-not-found machinery |
| Rendering an `Expr[T]` as Scala code | `toString` on Tree | `Printer.TreeCode.show(term)` / `Printer.TreeStructure` | `toString` shows internal compiler form (per docs warning) |
| Symbol "owner chain" enumeration | Manual `.owner` recursion | `Symbol.owner` chain + filter by `isPackageDef`/`maybeOwner`/`Symbol.noSymbol` | Cribs from Scala 2 cleanly |
| Position info on macro call site | Building `Position` manually | `Position.ofMacroExpansion` | Single API call |

**Key insight:** The 6 slices total ~150 LOC of Scala 2 macro code. Scala 3 equivalents should land at ~60–80 LOC total because the stdlib API has caught up with the helpers `commons-macros` used to provide.

## Common Pitfalls

### Pitfall 1: `report.error` vs `report.errorAndAbort` vs `report.info`
**What goes wrong:** Use `report.error` and the compiler proceeds — your macro may emit a placeholder `Expr` that crashes downstream type-checking. Use `errorAndAbort` and the compile halts immediately (preferred for "you passed an invalid arg" cases). Use `info` for "print this for debugging" — Scala 2 used `c.error` for `show*` macros as a hack to surface the message; Scala 3 has a proper `info` channel.
**How to avoid:**
- `infer*` macros: `report.errorAndAbort(msg)` when implicit not found.
- `show*` macros: `report.info(msg, pos)` (not `error`) — prints AST/type, doesn't block compile.
- `Sam.apply` validation failure: `report.errorAndAbort`.

### Pitfall 2: `Position.sourceCode` returns `Option[String]`
**What goes wrong:** Returns `None` for synthetic positions, REPL, or some IDE contexts. Pattern-match instead of `.get`.
**Fix:** `Position.ofMacroExpansion.sourceCode.getOrElse("<unknown>")` — or `report.errorAndAbort` if absence is genuinely a bug. Scala 2 also threw in this case (the `-Yrangepos` check).

### Pitfall 3: Recursive `inline` blow-up
**What goes wrong:** `inline def` that calls itself or another `inline def` can hit compiler recursion limits. None of Phase 2's macros are recursive, but `SimpleClassName.materialize` is `implicit` / `given` and could be summoned in a chain. Keep impl body pure compile-time (no `inline` -> `inline` chains).
**Warning sign:** Compile time per file > 10s, or "Maximal number of successive inlines exceeded" error.

### Pitfall 4: Mixing `inline given` and old `implicit def` for the same name
**What goes wrong:** `SimpleClassName.materialize` is currently `implicit def`. Replacing with `inline given` may break callers that explicitly named the implicit. Recommendation: use `inline implicit def materialize[T]: SimpleClassName[T] = ${ ... }` — preserves the original name + import path. (Scala 3 allows `inline implicit def`.)
**Fix:** Match the original `implicit def` form unless you have a reason to switch to `given`.

### Pitfall 5: `Type[T]` ergonomics
**What goes wrong:** Forgetting the `using Type[T]` evidence: `def impl[T](using Quotes): Expr[R]` won't compile if the body uses `TypeRepr.of[T]`. Always write `[T: Type]`.
**Fix:** Standard signature: `def fooImpl[T: Type](using Quotes): Expr[R]`.

### Pitfall 6: `Sam` / `SamCompanion` are deprecated
**What goes wrong:** Memory rule `feedback_dont_port_deprecated.md` says skip @deprecated APIs with stdlib replacements. Both `Sam` (since 2.28.0) and `SamCompanion` (since 2.28.0) advise native SAM conversion. Porting them re-establishes the obsolete surface for downstream.
**Recommendation:** Planner should propose **deleting** `Sam.scala` + `SamCompanion.scala` + their tests instead of porting. If the user wants them retained for source-compat, the impl is straightforward (`SamMacros.scala` blueprint provided below) — but delete is the principled choice.

### Pitfall 7: `c.prefix.tree` ↔ Scala 3 receiver capture
**What goes wrong:** Scala 2's `show*` macros peel `c.prefix.tree` via `Apply(_, List(prefix))` to access the receiver of `UniversalOps[A]`. In Scala 3, the receiver is just a method parameter — `inline def showAst: A = ${ impl('a) }` (where `a` is the `extends AnyVal` field). Different mental model — receiver is **passed**, not **discovered**.
**Fix:** When the macro lives inside `class UniversalOps[A](private val a: A) extends AnyVal`, splice `'a` directly. The `Expr[A]` carries everything you need (`a.asTerm.tpe`, `a.asTerm.pos`, etc.).

## Code Examples

All examples verified against Scala 3 official docs (`docs.scala-lang.org/scala3/guides/macros/`) and the upstream Scala 2 impls fetched from `upstream/master`.

### Slice 1: debug-reify (`SharedExtensions.show*` + `sourceCode` + `withSourceCode`)

**Scala 2 reference:** `upstream/master:macros/src/main/scala/com/avsystem/commons/macros/UniversalMacros.scala`

```scala
// File: core/src/main/scala/com/avsystem/commons/macros/ShowMacros.scala
package com.avsystem.commons.macros

import scala.quoted.*

object ShowMacros:
  def showAstImpl[A: Type](a: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    report.info(Printer.TreeCode.show(a.asTerm), a.asTerm.pos)
    a

  def showRawAstImpl[A: Type](a: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    report.info(Printer.TreeStructure.show(a.asTerm), a.asTerm.pos)
    a

  def showSymbolImpl[A: Type](a: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    report.info(a.asTerm.symbol.toString, a.asTerm.pos)
    a

  def showSymbolFullNameImpl[A: Type](a: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    report.info(a.asTerm.symbol.fullName, a.asTerm.pos)
    a

  def showTypeImpl[A: Type](a: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    report.info(TypeRepr.of[A].widen.show, a.asTerm.pos)
    a

  def showRawTypeImpl[A: Type](a: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    report.info(Printer.TypeReprStructure.show(TypeRepr.of[A].widen), a.asTerm.pos)
    a

  def showTypeSymbolImpl[A: Type](a: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    report.info(TypeRepr.of[A].typeSymbol.toString, a.asTerm.pos)
    a

  def showTypeSymbolFullNameImpl[A: Type](a: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*
    report.info(TypeRepr.of[A].typeSymbol.fullName, a.asTerm.pos)
    a

  def sourceCodeImpl[A: Type](a: Expr[A])(using Quotes): Expr[String] =
    import quotes.reflect.*
    val txt = a.asTerm.pos.sourceCode.getOrElse {
      report.errorAndAbort("source code unavailable at this position", a.asTerm.pos)
    }
    Expr(txt)

  def withSourceCodeImpl[A: Type](a: Expr[A])(using Quotes): Expr[(A, String)] =
    val src = sourceCodeImpl[A](a)
    '{ ($a, $src) }
```

```scala
// File: core/src/main/scala/com/avsystem/commons/SharedExtensions.scala
// Inside `class UniversalOps[A](private val a: A) extends AnyVal { ... }`
inline def showAst: A                = ${ macros.ShowMacros.showAstImpl[A]('a) }
inline def showRawAst: A             = ${ macros.ShowMacros.showRawAstImpl[A]('a) }
inline def showSymbol: A             = ${ macros.ShowMacros.showSymbolImpl[A]('a) }
inline def showSymbolFullName: A     = ${ macros.ShowMacros.showSymbolFullNameImpl[A]('a) }
inline def showType: A               = ${ macros.ShowMacros.showTypeImpl[A]('a) }
inline def showRawType: A            = ${ macros.ShowMacros.showRawTypeImpl[A]('a) }
inline def showTypeSymbol: A         = ${ macros.ShowMacros.showTypeSymbolImpl[A]('a) }
inline def showTypeSymbolFullName: A = ${ macros.ShowMacros.showTypeSymbolFullNameImpl[A]('a) }
inline def sourceCode: String        = ${ macros.ShowMacros.sourceCodeImpl[A]('a) }
inline def withSourceCode: (A, String) = ${ macros.ShowMacros.withSourceCodeImpl[A]('a) }
```

### Slice 2: source-positions (`positioned.here` + `SourceInfo.here`)

**Scala 2 reference:** `MiscMacros.posPoint` (line 499), `MiscMacros.sourceInfo` (line 36).

```scala
// File: core/src/main/scala/com/avsystem/commons/annotation/positioned.scala
object positioned:
  inline def here: Int = ${ posPointImpl }
  private def posPointImpl(using Quotes): Expr[Int] =
    import quotes.reflect.*
    Expr(Position.ofMacroExpansion.start)
```

```scala
// File: core/src/main/scala/com/avsystem/commons/misc/SourceInfo.scala
object SourceInfo:
  def apply()(using si: SourceInfo): SourceInfo = si

  inline given here: SourceInfo = ${ sourceInfoImpl }

  private def sourceInfoImpl(using Quotes): Expr[SourceInfo] =
    import quotes.reflect.*
    val p = Position.ofMacroExpansion
    val sf = p.sourceFile
    val filePath = sf.path
    val fileName = sf.name
    val offset = p.start
    val line = p.startLine + 1   // 1-based to match Scala 2 c.enclosingPosition.line
    val column = p.startColumn + 1
    val lineContent =
      sf.content.map(_.linesIterator.toIndexedSeq.lift(p.startLine).getOrElse("")).getOrElse("")
    val enclosing: List[String] =
      LazyList.iterate(Symbol.spliceOwner)(_.owner)
        .takeWhile(s => s != Symbol.noSymbol && !s.isPackageDef)
        .map(_.name)
        .toList
    '{
      SourceInfo(
        ${ Expr(filePath) }, ${ Expr(fileName) }, ${ Expr(offset) },
        ${ Expr(line) }, ${ Expr(column) },
        ${ Expr(lineContent) },
        ${ Expr(enclosing) }
      )
    }
```

**Note:** Scala 2 used `def here` (was `implicit def` semantically via `materialize`-style); Scala 3 syntax is `inline given here: SourceInfo`. Public surface still accessible via `implicitly[SourceInfo]` / `summon[SourceInfo]`.

### Slice 3: implicit-lookup (`Implicits.infer` family)

**Scala 2 reference:** `MiscMacros.infer` / `clueInfer` / `inferNonMacro` (lines 14–36).

```scala
// File: core/src/main/scala/com/avsystem/commons/misc/Implicits.scala
object Implicits:
  inline def infer[T]: T = ${ inferImpl[T]('{ "" }) }
  inline def infer[T](clue: String): T = ${ inferImpl[T]('clue) }
  // inferNonMacro intentionally identical surface — name signals "doesn't recursively expand macros".
  // In Scala 3, `Expr.summon` doesn't support "disable macros" — so inferNonMacro reduces to infer(clue).
  // Document this as a behavioural narrowing in MIGRATION.md if accepted.
  inline def inferNonMacro[T](clue: String): T = ${ inferImpl[T]('clue) }

  private def inferImpl[T: Type](clue: Expr[String])(using Quotes): Expr[T] =
    import quotes.reflect.*
    Expr.summon[T] match
      case Some(e) => e
      case None =>
        val clueStr = clue.value.getOrElse("")
        val prefix = if clueStr.nonEmpty then s"$clueStr: " else ""
        report.errorAndAbort(s"${prefix}could not find implicit value for ${TypeRepr.of[T].show}")
```

**MIGRATION.md note for the PR:** `inferNonMacro` previously called `inferImplicitValue(..., withMacrosDisabled = true)` — Scala 3 has no equivalent flag on `Expr.summon`. The Scala 3 impl therefore behaves identically to `infer(clue)`. Either (a) drop `inferNonMacro` as obsolete, (b) keep alias and document behaviour change. Planner picks.

### Slice 4: class-name (`SimpleClassName.materialize`)

**Scala 2 reference:** `MiscMacros.simpleClassName` (line 687).

```scala
// File: core/src/main/scala/com/avsystem/commons/misc/SimpleClassName.scala
object SimpleClassName:
  def of[T](using scn: SimpleClassName[T]): String = scn.name
  inline implicit def materialize[T]: SimpleClassName[T] = ${ materializeImpl[T] }

  private def materializeImpl[T: Type](using Quotes): Expr[SimpleClassName[T]] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[T].dealias
    val sym = tpe.typeSymbol
    if !sym.isClassDef then
      report.errorAndAbort(s"${tpe.show} does not represent a regular class")
    val name = sym.name
    '{ SimpleClassName[T](${ Expr(name) }) }
```

**Note:** `using` form is fine — `def of[T](implicit scn: SimpleClassName[T])` works equivalently. Match existing source style (`implicit` keyword) to minimize diff.

### Slice 5: SAM (`Sam.apply` / `SamCompanion.apply` / `isValidSam`)

**Recommendation: DELETE rather than port** (Pitfall #6). Both objects are `@deprecated` with stdlib replacement guidance. If user insists on porting:

**Scala 2 reference:** `upstream/master:macros/.../SamMacros.scala` (full file).

Pseudo-skeleton (do not literally translate — Scala 3 SAM via `compiletime.summonFrom` + `quotes.reflect.ClassDef.apply` is genuinely involved):

```scala
def createSamImpl[T: Type](fun: Expr[Any])(using Quotes): Expr[T] =
  import quotes.reflect.*
  val target = TypeRepr.of[T]
  val cls = target.classSymbol.getOrElse(
    report.errorAndAbort(s"${target.show} is not a class or trait"))
  val abstractMethods = cls.declaredMethods.filter(_.flags.is(Flags.Abstract))
  abstractMethods match
    case m :: Nil if m.flags.is(Flags.Method) && !m.flags.is(Flags.Private) =>
      // synthesize anonymous class via ClassDef.apply — see scala3 docs/library examples
      // OR: emit `new T { def m(args*) = fun(args*) }` via quotes by name and let compiler check.
      ???
    case _ =>
      report.errorAndAbort("target must have exactly one public, abstract, non-generic method")

def isValidSamImpl[T: Type, F: Type](using Quotes): Expr[ValidSam[T, F]] =
  // validate via same shape check, return '{ new ValidSam[T, F] {} }
  ???
```

**Effort assessment:** SAM port is ~80 LOC + tricky `ClassDef.apply` API. ~3× more work than every other slice combined. **Strongly recommend dropping the deprecated APIs** per memory rule.

## State of the Art

| Old Approach | Current Approach | When Changed | Impact |
|--------------|------------------|--------------|--------|
| `c.universe._` + `q"..."` quasiquotes | `scala.quoted.*` + `'{ ... }` / `${ ... }` | Scala 3.0 (2021) | Wholesale rewrite |
| `c.error(pos, msg)` / `c.abort` | `report.error(msg, pos)` / `report.errorAndAbort(msg, pos)` | Scala 3.0 | Cleaner API |
| `c.inferImplicitValue` | `Expr.summon[T]` | Scala 3.0 | Returns `Option[Expr[T]]` |
| `c.prefix.tree` peeling | Pass receiver as `Expr[A]` parameter | Scala 3.0 | Explicit > implicit |
| `WeakTypeTag[T]` | `Type[T]` (or `[T: Type]`) | Scala 3.0 | Different but equivalent |
| `c.enclosingPosition` | `Position.ofMacroExpansion` | Scala 3.0 | Direct API |
| Hand-rolled source-slice from `pos.source.content` | `Position.sourceCode: Option[String]` | Scala 3.0 | Stdlib does it |
| `commons-macros` (Scala 2 module) | Inline macros in same compilation unit | Phase 1 pivot | Module deleted |

**Deprecated / removed in scope:**
- `c.universe._` — gone in Scala 3.
- `-Yrangepos` — Scala 3 always has range positions.
- `WeakTypeTag[T]` — replaced by `Type[T]`.
- `Sam` / `SamCompanion` (project-level) — @deprecated 2.28.0; planner should consider dropping.

## Open Questions

1. **Should SAM slice be dropped instead of ported?**
   - What we know: `Sam` and `SamCompanion` are `@deprecated` since 2.28.0 with stdlib SAM as replacement.
   - What we know: memory rule `feedback_dont_port_deprecated.md` says skip @deprecated APIs with stdlib replacements.
   - What's unclear: whether downstream still depends on them.
   - Recommendation: planner asks user once during phase planning. Default = delete + remove from Backlog + remove from disabled tests.

2. **`inferNonMacro` semantic narrowing.**
   - What we know: Scala 2 used `withMacrosDisabled = true`; Scala 3 `Expr.summon` has no equivalent.
   - Recommendation: Plan keeps the alias for source-compat, documents the behavioural change in MIGRATION.md §3.

3. **`show*` family: `info` vs `error` channel.**
   - What we know: Scala 2 used `c.error` so the message appeared in compile output. Scala 3 has a proper `info` channel.
   - Recommendation: Switch to `report.info`. If user wants the "halt build" Scala 2 behaviour, swap to `error`.

4. **`SourceInfo.enclosingSymbols` semantics.**
   - What we know: Scala 2 walked `c.internal.enclosingOwner` chain to `rootMirror.RootClass`. Scala 3 equivalent is `Symbol.spliceOwner` chain to `Symbol.noSymbol`.
   - What's unclear: whether ".name" rendering matches exactly (Scala 2 used `.decodedName.toString` with a getter fallback).
   - Recommendation: Restore the matching test (if any) and adjust; otherwise smoke-test on a known location and accept minor drift.

## Validation Architecture

### Test Framework
| Property | Value |
|----------|-------|
| Framework | ScalaTest (existing — already on classpath; used across `core/src/test/`) |
| Config file | None (auto-discovered by sbt-scalatest) |
| Quick run command | `sbt 'commons-core/testOnly com.avsystem.commons.<Slice>Test'` |
| Full suite command | `sbt commons-core/test` (Phase 2 only touches `commons-core`) |

### Phase Requirements → Test Map

| Slice | Behavior | Test Type | Automated Command | File Exists? |
|-------|----------|-----------|-------------------|-------------|
| DEBUG-01 (show*) | Compile-time print of AST/symbol/type/raw; runtime returns receiver unchanged | unit (smoke — runtime assert + compile assert) | `sbt 'commons-core/testOnly com.avsystem.commons.SharedExtensionsTest'` | Likely ❌ (no `*ShowTest.scala` found in pre-pivot) → Wave 0 smoke |
| DEBUG-02 (`sourceCode`, `withSourceCode`) | Runtime returns the literal source text of the receiver expression | unit | same file as above | Smoke (Wave 0) |
| POS-01 (`positioned.here`) | Returns `Int` offset of call site | unit (smoke — relative-offset assert) | `sbt 'commons-core/testOnly com.avsystem.commons.annotation.PositionedTest'` | ❌ Wave 0 |
| POS-02 (`SourceInfo.here`) | Returns populated `SourceInfo` (filePath, line ≥ 1, lineContent contains call) | unit | `sbt 'commons-core/testOnly com.avsystem.commons.misc.SourceInfoTest'` | ❌ Wave 0 |
| IMPL-01 (`Implicits.infer*`) | Succeeds when implicit exists; fails compile with clue prefix when absent | unit + neg-test | `sbt 'commons-core/testOnly com.avsystem.commons.misc.ImplicitsTest'` | ❌ Wave 0 (compile-error tests require `compileErrors` helper or `expectCompilationError`) |
| CLS-01 (`SimpleClassName`) | `SimpleClassName.of[String] == "String"`, `of[List[Int]] == "List"` | unit | `sbt 'commons-core/testOnly com.avsystem.commons.misc.SimpleClassNameTest'` | ❌ Wave 0 |
| SAM-01 (if not dropped) | Synthesized SAM instance invokes function correctly | unit | `sbt 'commons-core/testOnly com.avsystem.commons.misc.SamTest'` | ❌ Wave 0 |

### Observable Signals (Nyquist)

Per restored macro, the following signals form the validation rhythm:

- **Source presence:** `git grep -n 'inline def <macroName>\|inline given <macroName>\|inline implicit def <macroName>' core/src/main/` → 1+ match.
- **Stub absence:** `git grep -n 'TODO\[scala3-port\]: <macroName>' core/src/main/` → 0 matches AND no `= ???` body remains on that line.
- **Compile gate:** `sbt commons-core/compile` exit 0 (must remain green per slice PR).
- **Test compile gate:** `sbt commons-core/Test/compile` exit 0.
- **Smoke test pass:** matching `testOnly` command exit 0.
- **Full suite pass:** `sbt commons-core/test` exit 0 at slice-merge time.
- **MIGRATION.md sync:** Backlog row removed AND `Total tags: N` decremented to match `git grep -c 'TODO\[scala3-port\]' -- '*.scala'`.
- **Scalafmt:** `sbt scalafmtCheckAll scalafmtSbtCheck` exit 0.

### Sampling Rate

- **Per task commit (per macro):** quick gate — `sbt commons-core/compile` + matching `testOnly`.
- **Per slice (per PR):** full slice — `sbt commons-core/compile commons-core/Test/compile commons-core/test scalafmtCheckAll`.
- **Phase gate (before `/gsd:verify-work`):** full suite green on all 4 PRs merged on top of `01-big-bang`.

### Wave 0 Gaps

Per slice, the planner must add (because pre-pivot test files for these specific concerns were not present — they used to live alongside derivation tests that are deferred):

- [ ] **debug-reify slice:** `core/src/test/scala/com/avsystem/commons/SharedExtensionsShowTest.scala` — smoke for each `show*` (assert runtime returns receiver; compile asserts `report.info` does not abort) + `sourceCode` (literal-text assert) + `withSourceCode` (tuple-shape assert).
- [ ] **source-positions slice:** `core/src/test/scala/com/avsystem/commons/annotation/PositionedTest.scala` (single assert: two adjacent `positioned.here` differ by a known small offset) + `core/src/test/scala/com/avsystem/commons/misc/SourceInfoTest.scala` (assert `filePath endsWith "SourceInfoTest.scala"`, `line > 0`, `lineContent contains "here"`).
- [ ] **implicit-lookup slice:** `core/src/test/scala/com/avsystem/commons/misc/ImplicitsTest.scala` — positive (define a `given Foo` and `Implicits.infer[Foo]` succeeds) + negative (use `compiletime.testing.typeCheckErrors` to assert `Implicits.infer[NotProvided]("clue")` fails with message containing `clue`).
- [ ] **class-name slice:** `core/src/test/scala/com/avsystem/commons/misc/SimpleClassNameTest.scala` — assert known names for `String`, `List`, custom case class.
- [ ] **SAM slice (if not dropped):** smoke for `Sam.apply[Runnable](42)` and `SamCompanion.apply` round-trip — same shape as Scala 2 tests upstream.
- [ ] Confirm `compiletime.testing.typeCheckErrors` is the right Scala 3 helper for negative tests (alternative: `scalatest`'s `assertDoesNotCompile`).

If user accepts the SAM-drop recommendation, the SAM Wave 0 gap collapses to: delete `Sam.scala` / `SamCompanion.scala` + their (commented-out) test files, then no test work needed.

## Sources

### Primary (HIGH confidence)
- `upstream/master:macros/.../UniversalMacros.scala` — Scala 2 impls for the 10 `show*` + `sourceCode` + `withSourceCode` macros (full file content fetched via `git show`).
- `upstream/master:macros/.../misc/MiscMacros.scala` — Scala 2 impls for `infer`, `clueInfer`, `inferNonMacro`, `sourceInfo`, `posPoint` (line 499), `simpleClassName` (line 687).
- `upstream/master:macros/.../misc/SamMacros.scala` — Scala 2 impl for `validateSam` / `createSam` (full file content fetched).
- Scala 3 official docs: `https://docs.scala-lang.org/scala3/guides/macros/quotes.html` — `Quotes`/`Expr`/`Type`/`Expr.summon` API.
- Scala 3 official docs: `https://docs.scala-lang.org/scala3/guides/macros/reflection.html` — `quotes.reflect`, Symbol, TypeRepr, Position API.

### Secondary (MEDIUM confidence)
- Scala 3 official reference Reporting API (`scala.quoted.Quotes.reflect.report`) — confirmed via reflection docs page.
- `inline given` vs `inline implicit def` interchangeability for `materialize`-style usage — official Scala 3 docs on `inline` + `given`.

### Tertiary (LOW confidence)
- Exact `Symbol.spliceOwner` chain semantics vs Scala 2's `c.internal.enclosingOwner` — believed equivalent but worth a manual smoke test against a known location.
- `Position.sourceCode` behaviour in IDE / REPL contexts — relevant only if downstream uses this in scripting.

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH — all APIs are stdlib, no external dep.
- Architecture: HIGH — patterns mirror official docs.
- Pitfalls: HIGH — directly informed by Scala 2 impls + Scala 3 docs side-by-side.
- Per-macro skeletons: MEDIUM-HIGH for slices 1–4, MEDIUM for `SourceInfo.here` (owner chain), LOW-MEDIUM for SAM (recommended dropped).

**Research date:** 2026-06-01
**Valid until:** 2026-07-01 (Scala 3.8.x stable; Scala 3.9 not yet released — re-check on Scala 3.9 GA)
