# rchicc Agent Guide

This document is for future coding agents working on `rchicc`, a Rust port of the *chibicc* teaching compiler. The crate turns a tiny C-like expression language into AT&T x86-64 assembly.

---

## Big Picture
- The executable lives in `src/main.rs` and calls `rchicc::generate_assembly`.
- The pipeline is linear and lives in `src/lib.rs`: `tokenizer -> parser -> codegen`.
- `error.rs` defines the user-facing diagnostic formatting.
- `ty.rs` contains the minimal type system used during parsing.
- `test.sh` is the authoritative regression suite; it builds the binary, compiles generated assembly with `gcc`, and checks exit codes.

---

## Module Notes

### Tokenizer (`src/tokenizer.rs`)
- Produces a `Vec<Token>` with an explicit `Eof` marker.
- Multi-character punctuators (`==`, `!=`, `<=`, `>=`) are matched first.
- Identifiers vs. keywords are disambiguated via `is_keyword`.
- Diagnostics rely on `CompileError::at`, so pass the source slice and byte offset.
- When adding new punctuators or keywords, update both the matching logic and `is_keyword`.

### Parser (`src/parser.rs`)
- Implements a recursive-descent parser that builds an AST (`AstNode`) and a single linked list of statements (`Stmt`).
- `ParserContext` interns locals, assigns stack offsets (16-byte aligned), and annotates AST nodes with `Type`.
- Expression helpers (`parse_assign`, `parse_equality`, etc.) follow a standard precedence-climbing pattern.
- `build_add` / `build_sub` are specialised to handle pointer arithmetic; touch `ty.rs` if you extend scalar types.
- Control-flow constructs (`if`, `for`, `while`) compile to AST nodes whose bodies reuse `Stmt` lists.
- Any new AST variant needs: parser construction, `ParserContext::annotate_type` handling, and codegen emission.

### Types (`src/ty.rs`)
- Currently supports `int` and pointers; both report a size of 8 bytes.
- `Type::with_decl_token` exists for future diagnostics but is unused right now.
- Extending the type system requires updating parser annotations, pointer arithmetic rules, and the code generator's load/store width assumptions.

### Code Generation (`src/codegen.rs`)
- Targets AT&T syntax, stack-based evaluation. Every expression leaves a value on the stack.
- Locals are addressed relative to `%rbp` using the offsets assigned in the parser.
- Labels (`.L.*`) are generated via a simple counter in `Codegen`.
- `emit_stmt_list` drops intermediate statement values unless `keep_final` is `true`; this is how blocks become expressions.
- Adding new AST kinds demands mirrored logic here (e.g., comparisons, logical ops, function calls).

### Errors (`src/error.rs`)
- Uses `snafu` for ergonomics; `CompileError::at` builds a caret-style diagnostic.
- If you surface richer errors, stay consistent with this formatting so the CLI and tests behave predictably.

---

## Running & Testing
- Preferred workflow:
  1. `cargo fmt` (if formatting changes are made).
  2. `cargo clippy --all-targets --all-features` to keep lints clean.
  3. `cargo test` is not defined; run `./test.sh` instead.
  4. `test.sh` accepts `BIN=/path/to/rchicc ./test.sh` if you need a different artifact.
- The script depends on `gcc` and `mktemp`. Keep tests fast (they run dozens of compile/execute cycles).
- When adding features, expand `test.sh` with both happy-path and failure examples; diagnostics are part of the UX.

---

## Coding Guidelines
- Follow the existing comment style: terse module-level docs and only clarify non-obvious code paths.
- Keep the pipeline pure: tokenizer should stay syntax-only; push semantic checks into the parser.
- Respect the stack-machine model in `codegen`—intermediate values must balance pushes/pops.
- When touching the type system, reason through pointer arithmetic (scale factors derive from `Type::size`).
- Prefer iterating on small, composable helpers rather than embedding logic into large functions.
- Use `rg` for discovery and keep edits confined to `src/` unless there is a compelling reason otherwise.

---

## Extensibility Tips
- **New operators**: Update tokenization, extend precedence parsing, set types via `ParserContext`, then teach `codegen` how to emit them.
- **New statement kinds**: Introduce a new `AstNode` variant, parse it into `Stmt`, adjust type annotation, and emit stack-balanced assembly.
- **Functions / multi-file**: You need a symbol table beyond the current `ParserContext` and call/return handling in `codegen`.
- **Diagnostics**: Use `CompileError::at` with byte offsets; calculators like `token.loc` already give you what you need.

---

## Quick Reference
- Entry point: `src/main.rs`
- Public API: `src/lib.rs:1`
- Token definition: `src/tokenizer.rs:15`
- AST definition: `src/parser.rs:20`
- Type annotation hub: `src/parser.rs:180`
- Code emitter: `src/codegen.rs:1`
- Test harness: `test.sh:1`

Keep this guide nearby when you spin up a new session—understanding the existing staging makes adding features far more pleasant. Happy hacking!
