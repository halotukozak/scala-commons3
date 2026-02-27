# GenCodec Gap Analysis: d4342be5 (Scala 2) vs HEAD (Scala 3)

Comprehensive comparison of `GenCodec` implementation between commit `d4342be57aa50986a4d07861e40561b5be4adbac` (Merge
of scalafmt PR, last Scala 2 state) and current HEAD.

---

## 1. MACRO/DERIVATION INFRASTRUCTURE

### Done (working)

- `GenCodec.derived[T]` via `DerMirror` — replaces `GenCodec.materialize`
- `GenCodec.materializeRecursively[T]` — reimplemented via `AllowRecursiveDerivation` + `derived`
- Implicit derivation guards: two `inline given` replacing old `materializeImplicitly` + `AllowImplicitMacro`
- `AllowDerivation` / `AllowRecursiveDerivation` gating mechanism
- `DerMirror` full derivation for: singletons, products, transparent wrappers, flat sums, nested sums
- `MacroInstances` rewritten from trait+whitebox-macro to `NamedTuple`+`inline given`+`summonInline`
- `TupleDerivation` — 560 lines of boilerplate → 4 lines with `Tuple.Map`+`compiletime.summonAll`

### Stubbed (`???`) — needs real implementation or intentional

- **`GenCodec.materialize[T]`** — returns `???` (stub). Was Scala 2 macro. Is `derived` the full replacement?
- **`GenObjectCodec.derived[T]`** — returns `???`
- **`GenObjectCodec.materialize[T]`** — `inline given` returning `???`
- **`ApplyUnapplyCodec.derived[T]`** — returns `???`
- **`ApplyUnapplyCodec.materialize[T]`** — `given` returning `???`
- **`SimpleRawRef` codec** — `given GenCodec[SimpleRawRef] = ???`
- **`RawRef.Creator.ref[T]`** and **`GenRef.Creator.ref[T]`** — stubs in `RawRefCreatorMacros`/`GenRefCreatorMacros`
- **`GenRefImplicitsMacros` `given Conversion`** — stub `= ???`
- **`SerializationMacros` object** — contains dead stub methods: `materializeImpl`, `fromApplyUnapplyProviderImpl`,
  `applyUnapplyCodecImpl`, `materializeRecursivelyImpl`, `materializeImplicitlyImpl`, `refImpl`, `fun2GenRefImpl`
- **All `MetaMacros` stubs** — `AdtMetadataCompanionMacros.materialize`, `MetadataCompanionMacros.materialize`,
  `MetadataCompanionLazyMacros.lazyMetadata`, `InferMacros.value`, etc.

---

## 2. MISSING CODEC DERIVATION FEATURES

These features worked in Scala 2 and are **not yet ported**:

| Feature                            | Evidence                                                                                             |
|------------------------------------|------------------------------------------------------------------------------------------------------|
| **Custom `apply`/`unapply` codec** | `CaseClassLike` companion commented out; test "case class like" disabled                             |
| **Inherited `apply`/`unapply`**    | `HasInheritedApply` companion commented out; test disabled                                           |
| **`fromApplyUnapplyProvider`**     | `ThirdPartyFakeCompanion` test "apply/unapply provider based codec" disabled                         |
| **`unapplySeq` (varargs)**         | `VarargsCaseClassLike`, `OnlyVarargsCaseClassLike` companions commented out; tests disabled          |
| **Varargs case class roundtrip**   | `VarargsCaseClass`/`OnlyVarargsCaseClass` roundtrip tests commented out (write tests still active)   |
| **Recursive generic case class**   | `Node[T]` companion commented out (`HasPolyGenCodec`/`HasGadtCodec` for local types); tests disabled |
| **Recursive generic ADT write**    | `Tree[Int]` write test disabled in `SimpleGenCodecTest` (but roundtrip still active — inconsistency) |

---

## 3. NULL-SAFETY ARCHITECTURE — Complete Rewrite

### Removed

- `NullSafeCodec[T]` trait (with `nullable: Boolean` flag)
- `nullable: Boolean` constructor param removed from: `SingletonCodec`, `ApplyUnapplyCodec`, `ProductCodec`,
  `SealedHierarchyCodec`, `NestedSealedHierarchyCodec`, `FlatSealedHierarchyCodec`, `JavaBuilderBasedCodec`
- All null-aware factory methods: `nullSafe`, `nullable`, `nonNull`, `nonNullString`, `nullableString`,
  `nullableSimple`, `nonNullSimple`, `nullableList`, `nonNullList`, `nullableObject`, `nonNullObject`

### Added

- `NullableCodec[T]` trait using `T | Null` union types
- `createNullable[T]` factory method
- Generic `given [T] => GenCodec[T] => GenCodec[T | Null]` — auto-wraps any codec
- All primitive/collection codecs now use `createSimple`/`createList`/`createObject` uniformly (no per-codec null
  distinction)

### Needs verification

- Does the new null handling work correctly for all previously-nullable types? Old code distinguished `AnyRef` (
  nullable) from `AnyVal` (non-nullable) at the codec level. New code delegates to a single generic `T | Null` wrapper.

---

## 4. ANNOTATION SYSTEM — Relocated & Retyped

| Annotation          | Old location    | New location    | Old base           | New base                                        |
|---------------------|-----------------|-----------------|--------------------|-------------------------------------------------|
| `@name`             | `serialization` | `mirror`        | `StaticAnnotation` | `MetaAnnotation` (extends `RefiningAnnotation`) |
| `@transparent`      | `serialization` | `mirror`        | `StaticAnnotation` | `MetaAnnotation`                                |
| `@optionalParam`    | `serialization` | `mirror`        | `StaticAnnotation` | `MetaAnnotation`                                |
| `@generated`        | `serialization` | `mirror`        | `StaticAnnotation` | `MetaAnnotation`                                |
| `@transientDefault` | `serialization` | `serialization` | `StaticAnnotation` | `RefiningAnnotation`                            |
| `@whenAbsent`       | `serialization` | `serialization` | `StaticAnnotation` | `RefiningAnnotation`                            |
| `@flatten`          | `serialization` | `serialization` | `StaticAnnotation` | `MetaAnnotation`                                |
| `@defaultCase`      | `serialization` | `serialization` | `StaticAnnotation` | `StaticAnnotation` (unchanged)                  |
| `@outOfOrder`       | `serialization` | `serialization` | `StaticAnnotation` | `StaticAnnotation` (unchanged)                  |

---

## 5. PRODUCT (CASE CLASS) DERIVATION — Deep Dive

### 5.1 Field Discovery

**Old**: Scala 2 macro inspects companion's `apply`/`unapply` method parameters. Each parameter becomes an `ApplyParam`
with idx, symbol, default value, codec instance, and optionLike.

**New**: `DerMirror.derivedImpl` uses `tSymbol.caseFields` + `Mirror.ProductOf[T].MirroredElemTypes`. Each field becomes
a `DerFieldElem` with type, label, metadata annotations, and default value.

### 5.2 Field Name Resolution (`@name`)

Both systems check `@name` annotations on the symbol and its overrides, falling back to the plain field name.
- Old: `findAnnotation(sym, NameAnnotType)` at macro expansion time
- New: `labelTypeOf(sym, sym.name)` using `sym.allOverriddenSymbols` → encoded as compile-time string literal type

Both resolve names into the `fieldNames: Array[String]` passed to `ApplyUnapplyCodec`/`ProductCodec` constructor.
**Working correctly.**

### 5.3 Codec Summoning for Fields

- Old: `c.inferImplicitValue` per-field
- New: `compiletime.summonFrom` per-field via `summonInstances`. For products: `summonAllowed = true`,
  `deriveAllowed = false` — only summons existing instances, never recursively derives for field types
  (unless `AllowRecursiveDerivation` is in scope).

### 5.4 Default Value Injection — **NOT IMPLEMENTED**

**Old**: The macro generated per-field `instantiate` code choosing between:
- `getField[T](fieldValues, idx)` — throws if missing
- `getField[T](fieldValues, idx, default)` — returns default if missing
- `getOptField[O, A](fieldValues, idx, optionLike)` — returns `optionLike.none` if missing

Defaults come from: `@whenAbsent` annotation (highest priority) > `@optionalParam` + `OptionLike.none` > Scala default
parameter value.

**New**: `DerMirror.defaultOf` correctly resolves defaults with the same priority chain and stores them in
`DerFieldElem.default: Option[MirroredType]`. **However, `GenCodec.deriveProduct` NEVER reads `DerFieldElem.default`.**
The `unsafeDerived` method only extracts `instances`, `fieldNames`, and `fromUnsafeArray` — it does NOT extract defaults
from the `DerMirror.Product`.

The `instantiate` method always calls `getField(fieldValues, idx)` (no-default variant), which throws `MissingField`
for any absent field:
```scala
override protected def instantiate(fieldValues: FieldValues): T & Product =
  fromUnsafeArray(Array.range(0, dependencies.length).map(getField(fieldValues, _)))
```

**Impact**: Any case class with default values, `@whenAbsent`, or `@optionalParam` will throw `MissingField` if the
field is absent in serialized data.

### 5.5 `@transientDefault` — **NOT IMPLEMENTED**

**Old**: The macro generated write code that compared field value against the default, skipping the write if equal:
```scala
// generated: writeField(output, idx, value, transientDefaultValue)
protected final def writeField[A](output: ObjectOutput, idx: Int, value: A, transient: A): Unit =
  if (value != transient) writeField(output, idx, value)
```
Also checked `IgnoreTransientDefaultMarker` to allow callers to force-write all fields. `size()` subtracted transient
fields.

**New**: `ProductCodec.writeFields` writes EVERY field unconditionally:
```scala
final def writeFields(output: ObjectOutput, value: T): Unit = {
  val size = value.productArity
  @tailrec def loop(idx: Int): Unit =
    if (idx < size) { writeField(output, idx, value.productElement(idx)); loop(idx + 1) }
  loop(0)
}
```

No `@transientDefault` check, no `IgnoreTransientDefaultMarker` check, no size adjustment.

### 5.6 `@optionalParam` + `OptionLike` — **NOT IMPLEMENTED**

**Old**: When `@optionalParam` was present on a field:
1. Codec wrapped in `OptionalFieldValueCodec(optionLike, innerCodec)`
2. Read: `getOptField(fieldValues, idx, optionLike)` → missing = `optionLike.none`
3. Write: `writeOptField(output, idx, value, optionLike)` → empty = skip writing

**New**: `DerMirror.defaultOf` correctly detects `@optionalParam` and resolves `OptionLike[E].none` as default. But:
- Codec is the plain element codec, NOT wrapped in `OptionalFieldValueCodec`
- Write always writes the field (even if `None`/`Opt.Empty`)
- Read uses `getField(fieldValues, idx)` without default → throws on missing

### 5.7 `@generated` Fields — **NOT IMPLEMENTED for products**

**Old**: The macro discovered `@generated` members on the type (including inherited from parent traits), resolved their
names (respecting `@name`), and generated code to write them alongside regular constructor fields. Generated fields
were write-only (never read). They contributed to serialized `size()`.

**New**: `DerMirror` correctly discovers generated members and creates `GeneratedDerElem` instances with right types,
labels, and extractor functions. `GenCodec.unsafeDerived` even computes `generatedNames`/`generatedExtractors`/
`generatedCodecs` for ALL mirror types — **but only passes them to the `SingletonOf` branch** (via
`deriveSingletonWithGenerated`). For products, they are computed and then discarded.

Evidence: `SomeCaseClass` test silently changed to remove `"someStrLen" -> 5` expectation. `Generator` test actively
fails. Singleton `SomeObject` works correctly (`Map("random" -> 42)`).

### 5.8 Construction Method

- Old: Direct call to companion's `apply(...)` method
- New: `Mirror.fromProduct(Tuple.fromArray(fieldValuesArray))` via `DerMirror.Product.fromUnsafeArray`

### 5.9 `ProductCodec` vs `ApplyUnapplyCodec`

**Old**: Macro chose between `ProductCodec` (optimized, for simple case classes without @transientDefault/@optionalParam/
@generated/primitive optimization) and full `ApplyUnapplyCodec` (with all feature support).

**New**: `deriveProduct` ALWAYS creates `ProductCodec`. There is no code path that creates a customized
`ApplyUnapplyCodec` from derivation. The primitive `writeField` overloads (Boolean/Int/Long/Double fast paths)
in `ApplyUnapplyCodec` are never utilized by the new derivation since `ProductCodec.writeFields` goes through
`value.productElement(idx)` which returns `Any`.

### 5.10 What `deriveProduct` Needs

The `deriveProduct` method needs to be enhanced to accept and use:
1. **`defaults: Array[Option[() => Any]]`** — from `DerFieldElem.default`, for read-path fallbacks
2. **`transientDefaults: Array[Boolean]`** — from checking `@transientDefault` in `Metadata` type, for write-skip
3. **`optionalParams: Array[Option[OptionLike[?]]]`** — for `@optionalParam` fields, both read and write
4. **`generatedNames/extractors/codecs`** — for write-only generated fields
5. **`IgnoreTransientDefaultMarker` support** — in the write path

The DerMirror already carries all the necessary metadata — the gap is entirely in `GenCodec.unsafeDerived` →
`deriveProduct` not consuming it.

---

## 6. SEALED HIERARCHY (NESTED) DERIVATION — Deep Dive

### 6.1 Runtime Codec Classes — Unchanged

`NestedSealedHierarchyCodec` in `macroCodecs.scala` is functionally identical between old and new (only `nullable`
param removed). The read/write logic is the same:

- **Write**: Emits exactly one field whose name is the case name, value is the full serialization of the case class.
- **Read**: Expects exactly one field. Field name is the case discriminator. Unknown names → `UnknownCase`. Empty/
  multi-field → `NotSingleField`.

### 6.2 Case Name Resolution

- Old: `targetNameMap` validates uniqueness across all subtypes. Uses `@name` annotation or decoded symbol name.
- New: `DerMirror.labelTypeOf` uses `@name` annotation or symbol name. **No uniqueness validation.**

### 6.3 Instantiation

**Old** (macro-generated):
```scala
new NestedSealedHierarchyCodec[T](tpe.toString, typeOf[Null] <:< tpe,
  Array("CaseName1", "CaseName2"), Array(classOf[Case1], classOf[Case2])) {
  def caseDependencies = Array(implicitly[GenCodec[Case1]], implicitly[GenCodec[Case2]])
}
```

**New** (`deriveNestedSum`):
```scala
new NestedSealedHierarchyCodec[T](typeRepr, fieldNames, classes.map(_.runtimeClass)) {
  override def caseDependencies: Array[GenCodec[?]] = instances
}
```

Case codecs gathered via `summonInstances` with `deriveAllowed = true, summonAllowed = false` — each case is
recursively derived (not summoned from scope).

### 6.4 Status: **Mostly working.** Missing: case name uniqueness validation at compile time.

---

## 7. SEALED HIERARCHY (FLAT / `@flatten`) DERIVATION — Deep Dive

### 7.1 Runtime Codec Classes — Unchanged

`FlatSealedHierarchyCodec` in `macroCodecs.scala` is functionally identical (only `nullable` removed). The read/write
logic is the same:

- **Write**: Writes `_case` field (unless transient default case), then delegates to case's
  `OOOFieldsObjectCodec.writeFields` to inline all case fields.
- **Read**: Tries `peekField(caseFieldName)` first. Falls back to iterating: accumulate OOO fields, trigger default
  case on seeing a case-dependent field, skip unknown fields. When case determined, calls
  `readFlatCase(caseName, oooFields, input, caseCodec)`.

### 7.2 `@defaultCase` — **NOT IMPLEMENTED**

**Old**: Macro inspected each subtype for `@defaultCase(transient: Boolean)`, validated only one exists, passed
`defaultCaseIdx` and `defaultCaseTransient` to constructor.

**New**: `deriveFlattenSum` **hard-codes** `defaultCaseIdx = 0` and `defaultCaseTransient = false`:
```scala
new FlatSealedHierarchyCodec[T](typeRepr, fieldNames, classes.map(_.runtimeClass),
  fieldNames, fieldNames.toSet, caseFieldName,
  0,      // defaultCaseIdx = 0 always
  false,  // defaultCaseTransient = false always
) { ... }
```

The `@defaultCase` annotation is never inspected. The first case (index 0) is always treated as the default.

### 7.3 `@outOfOrder` Fields — **NOT IMPLEMENTED**

**Old**: Macro identified `@outOfOrder` fields, validated cross-case type consistency, computed separate
`oooFieldNames`, `oooDependencies` (field-level codecs), and `caseDependentFieldNames`.

**New**: `deriveFlattenSum` passes **all case names as both `oooFieldNames` AND `caseDependentFieldNames`**:
```scala
new FlatSealedHierarchyCodec[T](typeRepr, fieldNames, ...,
  fieldNames,          // oooFieldNames = case names (WRONG — should be shared field names)
  fieldNames.toSet,    // caseDependentFieldNames = case names (WRONG — should be case-specific field names)
  caseFieldName, 0, false,
) {
  override def oooDependencies: Array[GenCodec[?]] = instances      // case codecs, not field codecs
  override def caseDependencies: Array[OOOFieldsObjectCodec[?]] =
    instances.map(_.asInstanceOf[OOOFieldsObjectCodec[?]])           // unchecked cast
}
```

Problems:
- `oooFieldNames` should be names of `@outOfOrder`-annotated fields shared across cases, not case discriminator names
- `caseDependentFieldNames` should be case-specific field names, not case names
- `oooDependencies` should be field-level codecs for OOO fields, not case-level codecs
- The cast `GenCodec[?]` → `OOOFieldsObjectCodec[?]` is unchecked (works only because `ProductCodec`/`SingletonCodec`
  happen to extend `OOOFieldsObjectCodec`)

### 7.4 Compile-Time Validations — **NOT IMPLEMENTED**

The following old-system validations are missing:
- `@defaultCase` only-one validation
- `@outOfOrder` cross-case type consistency
- Case field name collision with `caseFieldName` (e.g., a field named `"_case"`)
- `@transparent` in flat hierarchy rejection
- Case name uniqueness

### 7.5 `@flatten` Custom `caseFieldName` — **Working**

`DerMirror.getAnnotation[flatten]` correctly extracts the annotation instance. `flatten.caseFieldName` field accessed
directly. Default `"_case"` handled by the annotation's secondary constructor `def this() = this("_case")`.

---

## 8. OOO FIELDS / `FieldValues` SYSTEM — Deep Dive

### 8.1 Core Logic — Unchanged

The `OOOFieldsObjectCodec` trait, `FieldValues` class, `PeekingObjectInput`, and `DefaultCaseObjectInput` are
functionally identical between old and new. Only cosmetic changes:
- `_` → `?` wildcard
- `private[this]` → `private`
- `FieldInput | Null` union type with `.nn` assertion in `PeekingObjectInput`

### 8.2 `readObject` Logic — Unchanged

```
1. Create FieldValues(fieldNames, deps, typeRepr)
2. Rewrite from outOfOrderFields (from flat hierarchy)
3. Iterate ObjectInput, reading each field into FieldValues
4. Call instantiate(fieldValues) to construct T
```

### 8.3 `writeField` Primitive Optimizations — Cosmetic Change Only

The four primitive fast-path overloads (Boolean/Int/Long/Double) in `ApplyUnapplyCodec` reference the singleton codec
by its compiler-generated given name:
- Old: `GenCodec.BooleanCodec` → New: `GenCodec.given_GenCodec_Boolean`
- Old: `GenCodec.IntCodec` → New: `GenCodec.given_GenCodec_Int`
- Old: `GenCodec.LongCodec` → New: `GenCodec.given_GenCodec_Long`
- Old: `GenCodec.DoubleCodec` → New: `GenCodec.given_GenCodec_Double`

Logic identical: if codec matches the standard primitive codec, use fast-path writing to `SimpleOutput`.

**Note**: These fast-path overloads are never used by `deriveProduct` since `ProductCodec.writeFields` goes through
`value.productElement(idx)` which returns `Any`, always hitting the generic `writeField[A]` path.

### 8.4 `getField`/`getOptField` — Unchanged

```scala
getField[A](fieldValues, idx, default)  // returns default if missing
getField[A](fieldValues, idx)           // throws MissingField if missing
getOptField[O, A](fieldValues, idx, optionLike)  // returns optionLike.none if missing
```

All three variants exist in both old and new. The new derivation only uses the no-default `getField` variant.

---

## 9. `@generated` FIELD HANDLING — Deep Dive

### 9.1 What `@generated` Did in Scala 2

Write-only field generation mechanism:
1. **Products**: Macro discovered `@generated` members on the type (including inherited from parent traits via
   `tpe.members.filter(isGenerated)`), resolved names with `@name`, generated write code for each.
2. **Singletons**: `forSingleton` created extended `SingletonCodec` that wrote generated fields.
3. **Flat sealed hierarchies**: Each case included its own and inherited `@generated` fields.
4. **Read unaffected**: Generated fields were never read — only constructor params were deserialized.
5. **Size**: Generated fields contributed to serialized `size()`.

### 9.2 What Works Now

- **DerMirror** correctly discovers `@generated` members and creates `GeneratedDerElem` instances
  (`GeneratedAnnotationTest` passes for products, value classes, sums, enums, singletons)
- **Singletons**: `deriveSingletonWithGenerated` correctly writes generated fields. `SomeObject` test passes:
  `Map("random" -> 42)`.
- `GenCodec.unsafeDerived` computes `generatedNames`/`generatedExtractors`/`generatedCodecs` at top for ALL types.

### 9.3 What's Missing

- **Products**: Computed generated field info is **discarded** — never passed to `deriveProduct`.
  `SomeCaseClass` test was silently changed (removed `"someStrLen" -> 5` expectation).
- **Flat sealed hierarchies**: Generated fields from sealed trait not propagated to case codecs.
- The fix requires `deriveProduct` to accept `generatedNames: Array[String]`,
  `generatedExtractors: Array[T => Any]`, `generatedCodecs: Array[GenCodec[?]]` and override `writeFields`/`size`.

---

## 10. TRANSPARENT WRAPPER & VALUE CLASS — Deep Dive

### 10.1 `@transparent` Detection

- Old: Macro checks `isTransparent(sym)` via `findAnnotation(sym, TransparentAnnotType)`. Lives in `serialization`.
- New: `DerMirror.derivedImpl` checks `tSymbol.hasAnnotation(TypeRepr.of[transparent].typeSymbol)`. Lives in `mirror`.

Derivation priority in new system: `singleton > transparent > valueClass > product > sum`.

### 10.2 Value Classes

- Old: Treated as regular products via `applyUnapply` (single field). Serialized as `Map("str" -> "costam")`.
- New: Special `deriveValueClass` branch creates `DerMirror.Product` with single field. Same serialization.
  Separate branch needed because AnyVal subtypes don't have standard `Mirror.ProductOf`.

### 10.3 `TransparentWrapping` Typeclass

- Old: Manual implementation only (via `TransparentWrapperCompanion` superclass or explicit instance)
- New: Inline macro `TransparentWrapping.derived[R, T]` can auto-derive it. `TransparentWrapperCompanion` delegates
  to this via `MacroInstances`.

### 10.4 Codec Delegation

Both systems: `GenCodec.fromTransparentWrapping` wraps inner codec in `Transformed(wrappedCodec, tw.unwrap, tw.wrap)`.
Two paths exist: `GenCodec` level and `OOOFieldsObjectCodec` level (for flat hierarchies).

New system adds a third path: `deriveTransparentWrapper` directly creates `Transformed` when `DerMirror.Transparent`
is matched in `unsafeDerived`.

### 10.5 Changes in Test Data

- `ThingId`: `@transparent` annotation removed (was redundant with `StringWrapperCompanion`)
- Old's prohibition of `@transparent` in `@flatten` hierarchies: **removed** in new system (no validation)

### 10.6 Status: **Fully working.** All transparent wrapper and value class tests pass.

---

## 11. DEFAULT VALUES — Deep Dive

### 11.1 `DerMirror.defaultOf` (the working part)

Priority chain — matches old system exactly:
1. `fromWhenAbsent` — `symbol.getAnnotationOf[whenAbsent[?]]` → annotation value
2. `fromOptionalParam` — `symbol.hasAnnotationOf[optionalParam]` → `OptionLike[E].none`
3. `fromDefaultValue` — `companionModule.methodMembers` → `$lessinit$greater$default$N`

Result stored in `DerFieldElem.default: Option[MirroredType]`.

### 11.2 `@whenAbsent` Annotation

- Old: `extends StaticAnnotation`. `whenAbsent.value` was a Scala 2 whitebox macro.
- New: `extends RefiningAnnotation`. `whenAbsent.value` is a Scala 3 inline/macro using Quotes API.
  Walks owner chain to find `DefaultValueMethod` pattern, resolves annotated parameter, extracts value.

Annotation resolution works correctly. Gap is in codec not using the resolved value.

### 11.3 `@transientDefault` Annotation

- Old: `extends StaticAnnotation`. Detected by macro via `hasAnnotation(param.sym, TransientDefaultAnnotType)`.
- New: `extends RefiningAnnotation`. Available in `DerFieldElem.Metadata` type member but never checked by
  `deriveProduct`.

### 11.4 `IgnoreTransientDefaultMarker`

Object itself unchanged. In old system, macro-generated codec checked it:
```scala
val ignoreTransientDefault = output.customEvent(IgnoreTransientDefaultMarker, ())
if (ignoreTransientDefault) writeFieldNoTransientDefault(p, value)
else writeFieldTransientDefaultPossible(p, value)
```
New system never checks it.

### 11.5 Full Feature Matrix

| Feature                         | DerMirror      | GenCodec Derivation | Status          |
|---------------------------------|----------------|---------------------|-----------------|
| `@whenAbsent` default           | Resolved       | Not passed to codec | **Missing**     |
| `@optionalParam` default        | Resolved       | Not passed to codec | **Missing**     |
| Scala default value             | Resolved       | Not passed to codec | **Missing**     |
| `@transientDefault` write skip  | In Metadata    | Not checked         | **Missing**     |
| `@optionalParam` write unwrap   | In Metadata    | Not checked         | **Missing**     |
| `IgnoreTransientDefaultMarker`  | N/A            | Not checked         | **Missing**     |
| `OptionalFieldValueCodec` wrap  | N/A            | Not used            | **Missing**     |
| `size()` transient adjustment   | N/A            | Not implemented     | **Missing**     |

---

## 12. COMPANION OBJECT / HasGenCodec INFRASTRUCTURE

### Done

- `HasGenCodec`, `HasPolyGenCodec`, `HasGadtCodec`, etc. — migrated to `given`/`using`, opaque types, `AllowDerivation`
  gating
- `TransparentWrapperCompanion` — fully rewritten with Scala 3 macro `derivedImpl`
- `MacroInstances` — `NamedTuple` based (`.codec` instead of `.apply()`)

### Changed semantics

- `PolyCodec`, `PolyObjectCodec`, `GadtCodec`, `RecursiveCodec`, `AUCodec` — all converted from traits to opaque types
- `CodecWithKeyCodec` — eliminated, replaced by `GenKeyCodecFromTransparentWrapper`
- `HasGenAndKeyCodec` — uses new opaque type

---

## 13. GenKeyCodec CHANGES

### Done

- `forSealedEnum` — reimplemented via `DerMirror.SumOf` + inline match
- `forTransparentWrapper` — reimplemented via `TransparentWrapping` + `Transformed`
- All primitive key codecs migrated from `implicit lazy val` → `given`
- `fromTransparentWrapping` → `given` syntax

### Note

- Commented-out Scala 2 macro code still present in file (cleanup needed)

---

## 14. TUPLE CODECS

### Done

- 21 individual `tuple2Codec`..`tuple22Codec` → single generic `given [Tup <: Tuple] => GenCodec[Tup]`
- `TupleGenCodecs.scala` entirely deleted
- Implementation uses `Tuple.map`/`Tuple.zip` for read/write as list

---

## 15. COLLECTION CODECS

### Done

- 13 explicit per-collection-type implicits removed (generic `seqCodec`/`setCodec` cover them)
- All use `createList`/`createObject` instead of `nullableList`/`nullableObject`
- `@targetName("seqCodec")` added for JVM name disambiguation

---

## 16. SYNTAX MODERNIZATION (pervasive, done)

- `implicit def`/`val` → `given`
- `implicit class` → `extension`
- `(implicit ...)` → `(using ...)`
- `_` → `?` (wildcard types)
- `._` → `.*` (imports)
- `private[this]` → `private`
- `= _` → `= null.asInstanceOf[T]` or `scala.compiletime.uninitialized`
- Scaladoc format changed throughout

---

## 17. OTHER NOTABLE CHANGES

| Item                               | Details                                                                                              |
|------------------------------------|------------------------------------------------------------------------------------------------------|
| `ReadFailure`/`WriteFailure` cause | `Throwable` → `Throwable \| Null`                                                                   |
| `SubclassCodec`                    | No longer extends `NullSafeCodec`; no `nullable` param                                              |
| `ErrorReportingCodec`              | `typeRepr` moved from abstract def to constructor param; error methods explicit `: Nothing` returns  |
| `SizePolicy`                       | Rewritten from `AbstractValueEnum` → Scala 3 `enum`                                                 |
| `Void` codec                       | Added explicit `.asInstanceOf[Void]` cast                                                            |
| `OptRef` codec                     | `T >: Null` constraint dropped → just `T`                                                           |
| `fromJavaBuilder`                  | Fully reimplemented as inline + `scala.quoted` macro (in `GenCodec.scala` itself, ~80 lines)         |
| `TypeRepr[T]`                      | New opaque type — compile-time type name via `quotes.reflect`                                        |
| Named codecs lost                  | All `BooleanCodec`, `StringCodec`, etc. names → anonymous `given`                                    |
| `ParamFlags.rawFlags`              | Has `@name("dupa")` annotation — looks like a debugging artifact                                     |

---

## 18. FILE-LEVEL CHANGES

### Deleted files

- `serialization/name.scala` → moved to `mirror/name.scala`
- `serialization/transparent.scala` → moved to `mirror/transparent.scala`
- `serialization/optionalParam.scala` → moved to `mirror/optionalParam.scala`
- `serialization/generated.scala` → moved to `mirror/generated.scala`
- `serialization/TupleGenCodecs.scala` — replaced by generic tuple codec

### New files

- `mirror/DerMirror.scala` — core Scala 3 derivation mirror (548 lines)
- `mirror/generated.scala`, `mirror/name.scala`, `mirror/transparent.scala`, `mirror/optionalParam.scala` — relocated
  annotations
- `mirror/utils.scala` — `Expr.ofOption` extension for macros
- `serialization/TypeRepr.scala` — compile-time type name opaque type
- `serialization/SerializationMacros.scala` — Scala 3 macro stubs replacing Scala 2 macro entry points
- `meta/AllowDerivation.scala` — derivation gating mechanism
- `meta/MetaMacros.scala` — centralized Scala 3 macro stubs

### Heavily modified files

- `serialization/GenCodec.scala` — 654→730 lines, complete derivation rewrite
- `serialization/macroCodecs.scala` — `nullable` removal, `typeRepr` refactor
- `serialization/HasGenCodec.scala` — opaque types, `given`/`using`, `AllowDerivation` gating
- `serialization/GenKeyCodec.scala` — `forSealedEnum`/`forTransparentWrapper` reimplemented
- `serialization/TransparentWrapperCompanion.scala` — Scala 3 macro derivation
- `serialization/InputOutput.scala` — `SizePolicy` enum rewrite
- `serialization/whenAbsent.scala` — full macro rewrite
- `meta/MacroInstances.scala` — complete rewrite to `NamedTuple` architecture
- `tuples/TupleDerivation.scala` — 575→14 lines

---

## 19. COMPREHENSIVE SUMMARY: WHAT STILL NEEDS WORK

### Critical — Product codec gaps (deriveProduct needs enhancement)

1. **Default value injection on read** — `DerFieldElem.default` exists but is never passed to `deriveProduct`.
   Missing fields throw `MissingField` instead of using defaults. Affects `@whenAbsent`, `@optionalParam`, and
   Scala default parameter values.
2. **`@transientDefault` on write** — Fields are always written. No comparison against defaults. No
   `IgnoreTransientDefaultMarker` check. No `size()` adjustment.
3. **`@optionalParam` + `OptionLike`** — No `OptionalFieldValueCodec` wrapping. No `writeOptField`/`getOptField`
   usage. Empty optionals written as values instead of omitted.
4. **`@generated` fields for products** — Computed in `unsafeDerived` but discarded. Only singleton path uses them.
   Products never write generated fields.
5. **Primitive `writeField` specialization** — `ProductCodec` goes through `productElement(idx): Any`, bypassing
   the Boolean/Int/Long/Double fast paths in `ApplyUnapplyCodec`.

### Critical — Flat sealed hierarchy gaps (deriveFlattenSum needs enhancement)

6. **`@defaultCase`** — Annotation ignored. First case (index 0) always treated as default, non-transient.
7. **`@outOfOrder` fields** — Not inspected. `oooFieldNames` and `caseDependentFieldNames` are set to case names
   (wrong). `oooDependencies` are case-level codecs (should be field-level). OOO field accumulation in
   `FlatSealedHierarchyCodec.readObject` won't work correctly.
8. **`@generated` fields in flat hierarchies** — Not propagated from sealed trait to case codecs.

### Critical — Missing derivation features

9. **Custom `apply`/`unapply` codec derivation** (5 disabled tests)
10. **`unapplySeq` / varargs codec derivation** (4 disabled tests)
11. **`fromApplyUnapplyProvider` pattern** (1 disabled test)
12. **Recursive generic types with `HasPolyGenCodec`/`HasGadtCodec`** (2 disabled tests)
13. **All `???` stubs** — `GenObjectCodec.derived`, `ApplyUnapplyCodec.derived`, `GenRef`/`RawRef` macros,
    metadata macros

### Missing compile-time validations (old macro had them)

14. **Case name uniqueness** in sealed hierarchies — old `targetNameMap` validated; new does not
15. **`@defaultCase` only-one validation** — old aborted if multiple `@defaultCase`; new ignores entirely
16. **`@outOfOrder` cross-case type consistency** — old validated types match; new ignores entirely
17. **Case field name collision with `caseFieldName`** — old rejected field named `"_case"`; new does not check
18. **`@transparent` in flat hierarchy rejection** — old explicitly rejected; new does not check

### Behavioral differences (may be intentional, needs verification)

19. GADT tests using `Expr[Null]`/`Expr[String]` instead of `Expr[_]` — existential type handling changed
20. Inconsistency: `Tree[Int]` write test disabled in `SimpleGenCodecTest` but roundtrip active in
    `GenCodecRoundtripTest`

### Cleanup

21. Dead stubs in `SerializationMacros` object
22. Commented-out Scala 2 code in `GenKeyCodec`
23. `@name("dupa")` debugging artifact in `ParamFlags`
