---
name: sonowz-core-index
description: Use this whenever writing or refactoring code in any package within the sonowz-backend monorepo to ensure shared logic is reused.
---

# Sonowz Core Index

## Overview
This skill serves as a map and guide for `sonowz-core`, the central shared library of the `sonowz-backend` monorepo. Every package in this repository depends on `sonowz-core`. You must use the utilities defined here instead of reinventing them locally.

## Workflow

### 1. Essential Imports
Every module in a package should import its own package-specific prelude (e.g., `Sonowz.MyPackage.Imports`), which re-exports `Sonowz.Core.Imports`.

**What `Sonowz.Core.Imports` provides:**
- **Relude**: The custom prelude (with standard `mtl`-style State/Reader/Except hidden in favor of Polysemy).
- **Polysemy Core**: `Sem`, `Member`, `Embed`, `Final`, etc.
- **Common Effects**: `Error`, `Reader`, `State`, `AtomicState`.
- **Utilities**: `lengthText` (for `Text`), and the type-level list concatenator `(<>)`.

### 2. Standard Effect Stack (`Sonowz.Core.StdEff`)
- **`StdEff`**: The base effect row, typically `[StdLog]`.
- **`stdEffToWebHandler`**: Use this as the entry point for Servant handlers to run your `Sem` stack. It handles logging of `ServerError`s automatically.
- **`stdEffToIOFinal`**: The standard interpreter to reach `IO` while handling logging and unhandled exceptions.

### 3. Error & Exception Handling (`Sonowz.Core.Error.Effect`)
- **`webLiftIO`**: Use inside Servant handlers to wrap `IO` actions. It catches exceptions and maps them to `500 Internal Server Error` with the exception message in the body.
- **`runErrorAsLogging`**: Catch an `Error e` effect, log it to `StdLog`, and return `mempty`.
- **`mapErrorAs500`**: Specifically for Servant; maps any exception to `err500`.
- **`foreverCatch`**: Useful for long-running workers; runs an action in a loop and catches/logs any top-level exceptions to prevent thread death.

### 4. Database (Opaleye + PostgreSQL)
- **`Sonowz.Core.DB.CRUD`**: Provides `getCRUDQueries`. Use this to avoid writing boilerplate `list`, `read`, `create`, `update`, and `delete` functions for standard tables.
- **`Sonowz.Core.DB.Utils`**:
    - `maybeToException`: Use when a query MUST return a result (e.g., fetching by ID).
    - `updateAlternative`: Vital for implementing partial updates where `Nothing` fields should not overwrite existing data.
    - `putArg`: Helper for arrow-based queries to inject arguments.

### 5. Network & HTTP (`Sonowz.Core.HTTP.Effect`)
- **`HTTP` Effect**: Use `fetchURL` or `fetchWithRequest` instead of calling `http-client` directly.
- **`runHTTPIO`**: The standard interpreter using `http-client-tls`.

### 6. Web & Server (`Sonowz.Core.Web.Warp`)
- **`runAppWithAccessLog`**: The standard way to start a Warp server. It includes pre-configured, color-coded logging of requests (Method, Path, Status, Size).

## Caveats
- **No Implicit Prelude**: Always ensure `NoImplicitPrelude` is enabled (it's global in this repo) and the correct imports are used.
- **Opaleye Arrows**: Remember to add `{-# LANGUAGE Arrows #-}` when writing non-trivial DB queries.
- **Record Fields**: `DuplicateRecordFields` is enabled. Access fields via standard accessor functions, as `OverloadedRecordDot` is not used.
- **Exception Safety**: Prefer `catchAnyException` or `foreverCatch` for top-level loops to ensure system resilience.
- **DBEffects Type Alias**: `DBEffects` is a type alias for a list of effects (`Reader DBConnPool : Resource : Embed IO : StdEff`). When using it in `Members`:
    - **CORRECT**: `Members (Error ServerError : DBEffects) r`
    - **INCORRECT**: `Members '[Error ServerError, DBEffects] r` (This causes a kind mismatch error).

## References (File Paths)
- `sonowz-core/src/Sonowz/Core/Imports.hs`: Core re-exports
- `sonowz-core/src/Sonowz/Core/StdEff/Effect.hs`: Effect plumbing
- `sonowz-core/src/Sonowz/Core/Error/Effect.hs`: Error utilities
- `sonowz-core/src/Sonowz/Core/DB/CRUD.hs`: Generic DB operations
- `sonowz-core/src/Sonowz/Core/HTTP/Effect.hs`: HTTP effect definition
