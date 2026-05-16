# Design — On-Demand (Server) Translation Mode for Charon

**Status:** Draft / RFC
**Author:** _draft_
**Last updated:** 2026-05-16

## 1. Motivation

Today Charon translates an entire crate (and its in-scope dependencies) in a single batch invocation, producing one `.llbc` / `.ullbc` JSON or postcard file. This is fine for small to medium crates, but breaks down for large ones:

- Translation time dominates the workflow of any analysis client that only needs a small subset of the crate (e.g. a verifier called on a few entry points).
- Peak memory usage scales with crate size: the entire `TransformCtx` is held in memory.
- The resulting file is large to (de)serialize on the client side.
- A client cannot interactively explore — e.g. translate `f`, then follow its callees one by one — without re-running the whole pipeline.

We want to add an **on-demand translation mode** ("server mode") so that an analysis tool (potentially written in OCaml or another language) can:

1. Open a Charon session on a crate.
2. Request entry points by name (or by `DefId`-like handle).
3. Iteratively request more items as the analysis discovers them.
4. Get translated items back quickly, ideally with a warm cache.

## 2. Goals and non-goals

### Goals

- **Per-item translation** triggered by an explicit client request.
- **Caching**: never translate the same item twice within a session.
- **Language-agnostic client**: OCaml, Python, Rust, … must be able to drive it.
- **Low memory ceiling**: support evicting cold items to disk.
- **Minimal divergence** from batch mode: the same translation code should run; only the driver loop changes.
- **Forward-compatibility** with codebase evolution (the AST changes regularly).

### Non-goals (for the first iteration)

- Whole-crate passes (`reorder_decls`, `partial_monomorphization`, `sccs`...).
- Recompiling on source change. A session is bound to one fixed build.
- Multi-tenant / multi-crate within a single process. One server = one crate build.
- Cross-process item sharing or distributed translation.
- File-only translation (like `charon rustc`), the server feature is meant for intense workloads, so supporting `rustc` is irrelevant. We only care about the `cargo`-driven workflow.

## 3. Background — what already exists

Charon's translation pipeline is _already worklist-driven internally_; this is the key enabler.

- `charon/src/bin/charon-driver/translate/translate_crate.rs` documents the loop: items are
  registered as `TransItemSource`s and translated one at a time; the translation of an item
  may register more `TransItemSource`s, which get translated later.
- Items have a stable opacity model (`--include` / `--opaque` / `--exclude`) — _transparent_
  means "translate the body too", _opaque_ means "translate only the outer shell".
- Items have unique, name-based identities (`Names`, see `docs/usage.md`), addressable
  externally with the `NameMatcher` pattern syntax.
- The driver runs inside a rustc `Callbacks::after_expansion` hook, so the `TyCtxt` is alive
  exactly during translation. Today, after `after_expansion` returns, `TyCtxt` is gone and
  post-passes run on the standalone `TransformCtx`.
- Output already supports two formats: JSON and `postcard` (binary). Both have working
  OCaml bindings.

Implications:

- We do **not** need to redesign translation. We need to (a) keep `TyCtxt` alive across
  requests, and (b) expose the worklist to an external client.

## 4. High-level design

```
┌────────────────────────┐         RPC          ┌──────────────────────────┐
│  Analysis client       │  ──────────────►     │  charon server process   │
│  (OCaml / Rust / …)    │     stdin/stdout     │  ┌─────────────────────┐ │
│                        │     or UNIX socket   │  │ rustc + TyCtxt      │ │
│  - asks for items by   │                      │  │ (kept alive)        │ │
│    name / handle       │  ◄──────────────     │  │                     │ │
│  - receives postcard   │        item          │  │ Charon translator   │ │
│    or JSON chunks      │                      │  │  - worklist         │ │
│                        │                      │  │  - per-item cache   │ │
│                        │                      │  │  - optional on-disk │ │
└────────────────────────┘                      │  │    store            │ │
                                                │  └─────────────────────┘ │
                                                └──────────────────────────┘
```

A long-lived `charon serve` process holds the rustc compilation session open. Translation runs lazily on request. Each translated item is cached and returned to the client.

### 4.1 Session lifecycle

1. **`Open`** — client launches `charon serve <args> -- <cargo args>`. The server invokes rustc with the same setup as today (`RUSTC_WRAPPER=charon-driver` style), but at the `after_expansion` callback it does **not** run the worklist to exhaustion; instead it enters an RPC loop with `TyCtxt` borrowed from the callback frame.
2. **Requests** — client sends `get`, `resolve`, etc. (see §5.3).
3. **`Close`** — client sends `shutdown` and the server exits the RPC loop.

The whole RPC loop necessarily runs _inside_ `after_expansion`: rustc's `TyCtxt` is borrowed, not owned, so we cannot escape it without losing the compiler state. This is fine; rustc is happy to sit there indefinitely.

### 4.2 What is cached

For each translated `TransItemSource`:

- Its assigned `ItemId`.
- Its `Name` (kept in the existing `item_names` map of `TranslatedCrate`).
- Its translated AST node (`FunDecl`, `TypeDecl`, `GlobalDecl`, `TraitDecl`, `TraitImpl`), with all selected per-item passes already applied.
- Or if translation failed, the error that caused the failure (so subsequent requests for the same item fail fast without re-translating).

The set of post-translation passes is fixed at session start (chosen from CLI flags by the process that launches the server) and applied unconditionally to every item before it is inserted into the cache.

Dependencies are _not_ tracked separately either: every `ItemId` referenced by the cached AST node is, by construction, the set of items this one depends on. The client recovers them by walking the AST it just received.

### 4.3 Passes in server mode

Per-item passes (the vast majority — see `charon/src/transform/mod.rs`) are applied as soon as an item is fully translated. **Crate-global passes are not supported in server mode at all** — they are skipped unconditionally. This includes `reorder_decls`, `sccs`,
`partial_monomorphization`, `filter_invisible_trait_impls`, the post-transformation typecheck, and anything else that needs whole-crate visibility. Clients that need those should use batch mode.

The reasoning: any whole-crate pass is fundamentally incompatible with an open-ended worklist. Running it "on the visited subset at shutdown" gives output that _looks_ like batch output but has subtly different invariants (e.g. SCCs are wrong because items outside the visited subset can reorder the recursion structure); we prefer to be honest about not supporting them than to ship a half-working approximation.

The current `Pass` enum (`NonBody` / `UnstructuredBody` / `FusedUnstructuredBody` / `StructuredBody`) is **not** a per-item-vs-whole-crate axis: it classifies passes by the _shape_ of body they expect (none vs. unstructured vs. structured), not by whether they need the whole crate. A `NonBody` pass may still walk and mutate bodies; it just doesn't care whether they're ULLBC or LLBC. The per-item-vs-whole-crate distinction is orthogonal and must be added as new metadata on each pass.

## 5. Wire protocol

### 5.1 Transport

Recommended: **length-prefixed postcard frames over stdio** (4-byte big-endian length, then payload). Reasons:

- Postcard is dramatically smaller and faster than JSON for the AST types, which dominate the wire volume. OCaml bindings for postcard already exist (used by today's batch output), so the OCaml client side has no new decoder work.
- The same `serde`-derived (de)serializers used for the existing `.llbc` files are reused byte-for-byte — no second serialization path to maintain.

A JSON mode (newline-delimited) is offered as a fallback for debugging, scripting, and clients outside the OCaml/Rust ecosystem, selectable with `--format json`. The message _types_ are identical; only the encoding differs.

### 5.2 Item handles

Items are addressed exclusively by **`ItemId`** — the existing tagged-union from
`charon/src/ast/krate.rs`:

```rust 
pub enum ItemId {
    Type(TypeDeclId),
    TraitDecl(TraitDeclId),
    TraitImpl(TraitImplId),
    Fun(FunDeclId),
    Global(GlobalDeclId),
}
```

These IDs are server-assigned, valid for the lifetime of the session, and are exactly the IDs already embedded everywhere in the AST (return types, function calls, type references, etc.). Once the client has the AST of an item, it can read off the IDs of every dependency directly from the AST — no separate dependency listing is needed in the wire response.

The only operation that does _not_ take an `ItemId` is the bootstrap step:

- **`resolve(pattern)`** — translate a `NameMatcher` pattern to the set of `ItemId`s of items whose name matches. This is the only entry point that uses names, and exists solely so that the client can find its first set of `ItemId`s. Subsequent requests are pure `ItemId`-driven.

`ItemId`s are stable across `resolve` calls within a session: resolving the same pattern twice yields the same IDs, and `resolve` does not trigger translation — it only assigns IDs and records names. (This matches the existing invariant that name maps are populated for every registered item, even ones never fully translated; see `item_names` and `assoc_item_names` in `TranslatedCrate`.)

### 5.3 Message catalogue (sketch)

The protocol is purely client-driven.

| Method | Purpose |
| ------ | ------- |
| `crate()` | Return session-level constants: `crate_name: String`, `target_information: { triple -> TargetInfo }`, `charon_version: String`, `cli_options: CliOptions`. |
| `resolve(pattern)` | Resolve a `NameMatcher` pattern to a list of `ItemId`s. Does not translate. The pattern may also be a function attribute. |
| `get(ItemId)` | Return the AST node for an item, translating it (and applying the session's configured passes) on first request, otherwise serving it from the cache. |
| `name(ItemId)` | Return the `Name` of an item without translating it. |
| `file(FileId)` | Return the `File` entry for a given `FileId`. |
| `stats()` | Cache size, memory usage, etc. |
| `shutdown()` | End the session. |

Responses share an envelope with `id`, `ok`/`err`, and a payload.

The response payload for `get` is the bare AST node for that `ItemId` — a `TypeDecl`, `FunDecl`, `GlobalDecl`, `TraitDecl`, or `TraitImpl`. Any `ItemId`s appearing inside it (in `Ty`, `FnPtr`, `TraitRef`, etc.) are the item's dependencies; the client can request them next. Newly-encountered IDs are registered server-side and have an associated `Name` (via `name`) but no body yet.

`FileId`s embedded in spans are handles the client resolves the same way it resolves `ItemId`s: pull what you need via `file`. Most analysis clients never look at source-position information at all and would discard inlined file bytes; the pull model keeps the common case lean. The caveat is that the existing `charon-ml` deserializers (both JSON and postcard) assume files are populated _before_ spans are decoded (see the comment on `TranslatedCrate.files` in `krate.rs`). A server-mode client in `charon-ml` will either decode item payloads with a span-aware reader that consults a lazy `file` lookup, or pre-fetch every `FileId` referenced in the item before handing the bytes to the existing deserializer. Both are small extensions to the loader, not changes to the AST.

### 5.4 Encoding the AST

Reuse `charon_lib::export::CrateData` types as the on-the-wire representation. Each `get` response carries one item-shaped payload in the same shape as today's batch output, just sliced per-item. Clients can already deserialize this with the existing OCaml / Rust bindings — _that is the central reason this protocol is cheap to build_.

## 6. Disk-backed cache (future work)

Optional but recommended for large crates as a **memory-pressure release valve**, _not_ as a cross-session persistence layer.

- Each translated item is keyed by the server-assigned `ItemId`, which is stable for the lifetime of the session.
- On eviction, the item's postcard bytes are written to a per-session temp directory (e.g. `${CHARON_CACHE_DIR}/<pid>/<ItemId>`).
- On request for an evicted item, the file is mmap'd / read back instead of re-translating.
- The cache directory can be deleted on `shutdown`.

**Cross-session reuse is not supported.** rustc's `DefId`s are not stable across runs — the same source item may end up with a different `DefId` (and therefore a different `TransItemSource`) on the next invocation. Hashing the source path doesn't help: hax's `ItemRef` includes generic substitutions and other pieces of rustc state that have no stable serialization. We'd either need to re-resolve every cached item by `Name` on startup (defeating the point — re-resolving requires walking the crate graph through rustc anyway) or invest in a cache key derived from `Name` + a canonical, stable encoding of generic args.

The on-disk format is always postcard, independent of the wire encoding: it's compact, fast, and matches what `serde`-derived (de)serializers in Charon already produce. If the wire encoding is JSON, the server transcodes on the way out.

## 7. Background prefetch

The server can **speculatively translate the requested item's dependencies in the background** after each response is sent, so follow-up requests hit a warm cache instead of paying the rustc translation cost. This is a session-level setting, fixed at server startup, with no per-request override — the request API stays minimal (`get(id )`).

Configured by **`--prefetch-depth N`**:

- `--prefetch-depth 0`: no prefetch. The server replies and goes idle.
- **`--prefetch-depth 1`** (default): after replying, translate the full bodies of the served item's direct dependencies. Catches the common "I asked for foo, now I want its callees" pattern.
- `--prefetch-depth N` (N ≥ 2): same, but N hops deep.

Key semantics:

- **The response payload is unchanged.** A `get(foo)` returns _only_ `foo`. Prefetched items go into the cache, never into the response. If the client subsequently asks for `bar` (one of foo's callees), the call resolves from cache without going through rustc.
- **Prefetch runs on the RPC thread.** `TyCtxt` is `!Send`, so we can't spawn a real worker. "Background" here means "during the gap between responding to request K and receiving request K+1." The server starts prefetching as soon as the response is flushed and **does not** abort the moment the next request frame arrives. This means the client may get a slight delay on the next request if it arrives while prefetch is still running, but it also means we don't waste the work already done if the next request is for an item that was prefetched.
- **Errors during prefetch are swallowed.** A failure speculatively translating an item must not poison the session; it's logged and the item is marked "translation-failed" in the cache so a later real `get` for it returns the same error immediately. The delayed-bug / fatal-error wrapping from §8 applies to prefetch the same way it applies to user requests.

Going higher than `2` is rarely worth it in practice; the dependency graph fans out fast.

There is no prefetch time/item budget; we translate wholly all items N deep.

## 8. Risks and errors

**Delayed bugs and diagnostic state.** rustc's `DiagCtxt` accumulates "delayed bugs" (`delay_span_bug`), which are checked on session-finish and abort the process if any are unconsumed. It also has a global error counter; once an error is emitted, several queries short-circuit. For a server, this means:

- A translation that triggers a delayed bug will, if not drained, take down the process at shutdown.
- Once one request causes rustc to emit a real error, subsequent requests touching the same region of the crate may behave oddly.

Mitigation: per-request, wrap the translation in `catch_fatal_errors` (rustc already provides this), surface the error to the client as a normal RPC error, and explicitly drain/handle delayed bugs at the boundary between requests. Treat any rustc panic as "poisoned session" and refuse further requests — clients then reconnect to a fresh session, which will have to re-translate from scratch (the cache is session-scoped).

**Fatal errors and panics inside rustc.** `rustc_driver::catch_fatal_errors` uses `std::panic::catch_unwind`. This is what currently turns fatal compiler errors into `CharonFailure::RustcError`. In server mode we want the _same_ catching, but per-request: one bad item should not kill the session. Wrap each translation invocation individually. Document clearly that some rustc panics (stack overflow on deeply recursive types, ICEs) are still unrecoverable.

**Thread-local TyCtxt access.** `rustc_middle::ty::tls::with` uses thread-locals to make `TyCtxt` reachable from `Debug` impls etc. The RPC loop must run on the same thread that rustc gave us `TyCtxt` on; the IO can be on another thread (e.g. a reader thread feeding a channel) but the dispatch and translation must happen on the rustc thread. This is fine given we'd already serialize requests, but rules out e.g. "compute on a thread pool while the rustc thread does IO."

**Source / sysroot changes.** rustc may lazily load and re-stat sysroot or extern-crate metadata files. If those change during a session, behaviour is undefined. The server should fail fast on startup if it detects the sysroot toolchain doesn't match `rust-toolchain.template`, and clients should not edit the source tree while a session is open. Practically the same constraint as a normal `cargo build` — but worth stating explicitly.

**Long-running session ergonomics.**

- We should expose an `--idle-timeout` so a forgotten server doesn't sit holding memory  forever. Recommended default: no timeout (let the client manage), but offer it.
- A periodic "heartbeat" log line (RSS, items cached, requests served) is cheap and invaluable for debugging.

## 9. Impact on the codebase

Overall: **moderately isolated**, with one significant refactor.

### 9.1 New code (mostly additive)

- `charon/src/server/` (new module): RPC loop, message types, dispatcher, disk cache.
- New subcommand in `charon/src/bin/charon/cli.rs`: `charon serve …`.
- New driver entry point that, instead of draining the worklist, hands control to the RPC loop. This sits next to `run_rustc_driver` and reuses `CharonCallbacks` with a different post-`after_expansion` behaviour.

### 9.2 Existing code touched

- `transform/mod.rs`: tag each `Pass` as per-item / whole-crate / mixed. This axis is _not_ what the current `Pass` enum encodes — `NonBody` / `*Body` is about expected body shape, not scope, and `NonBody` passes still freely modify bodies. We need a new explicit classification, because some `NonBody` passes (e.g. `compute_short_names`) are fine to run on the visited subset, while others (e.g. `reorder_decls`, `sccs`, `partial_monomorphization`) genuinely need the whole crate. Estimated ~1 day.
- `bin/charon-driver/translate/translate_crate.rs`: factor the worklist loop into a function that takes a "fetch policy" (translate everything reachable / stop unless requested). Estimated ~2–3 days.
- `name_matcher/`: expose a `resolve_pattern_without_translating` API. Small, ~1 day.
- `export/`: add per-item serialization helpers, currently `CrateData` serializes everything at once. Estimated ~1 day.
- OCaml bindings: add `charon-ml` support for incrementally parsing a stream of per-item postcard frames, plus the lazy `file`/`name` lookup hook for span deserialization. The per-item types are already exposed by the existing postcard bindings. Estimated ~2 days.

### 9.3 Tests / CI

- Property test: for a fixed crate and pass configuration, server-mode result for items {A, B, C} must equal batch-mode result restricted to {A, B, C}, ignoring whole-crate passes.
- Restart test: kill the server mid-translation, launch a fresh one, request the same items by `Name`, get the same answers. (The new session starts cold; this just verifies the client-driven recovery path works end-to-end.)
