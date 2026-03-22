# AGENTS.md

## Project Overview

Haskell monorepo with 11 packages (1 shared library `sonowz-core`, 10 executables).
Uses **Stack** (LTS-24.11 / GHC 9.10.2), **Nix** for dev environment, **Polysemy** for effects,
**Relude** as custom prelude, **Servant** for HTTP APIs, **Opaleye** + PostgreSQL for database.

## Build Commands

```bash
# Build everything
# Note: NEVER clean build
stack build

# Build a single package
stack build sonowz-core
stack build sonowz-web

# Build and install all executables to ./bin/
stack install --local-bin-path=bin
```

## Linting and Formatting

There is no pre-commit hook. Both `hlint` and `ormolu` are available in the Nix dev shell (`nix develop`). Tell the user to add pre-commit for agent development.

## Testing

There are **no automated test suites** in this project. Do not add any. When it compiles and runs idle without crashing, it works. To run an executable:
```bash
# Run example
stack run sonowz-web -- --port 8080
```

## Package Structure

Each package follows this layout:
```
sonowz-<name>/
  package.yaml              # hpack config (generates .cabal)
  src/
    Main.hs                 # entry point (executables only)
    Sonowz/<Name>/
      Imports.hs            # package-specific prelude (re-exports Core)
      Env.hs                # runtime environment / config record
      App.hs                # application wiring / server setup
      <Domain>/
        Types.hs            # domain types
        Effect.hs           # Polysemy effect definition
        DB/
          Types.hs          # Opaleye table types (uses TemplateHaskell)
          Queries.hs        # database queries (uses Arrows)
      Web/
        <Feature>.hs        # Servant API handlers
```

Dependency graph: all packages depend on `sonowz-core`. Additionally:
`sonowz-web` -> `sonowz-auth`; `sonowz-news-combinator` and `sonowz-stock-noti` -> `sonowz-noti`.

## Code Style

### Language and Extensions

- **Language version:** `GHC2024` (all packages)
- **Default extensions** (in every `package.yaml`): `ApplicativeDo`, `DefaultSignatures`,
  `DeriveAnyClass`, `DerivingVia`, `DuplicateRecordFields`, `FunctionalDependencies`,
  `MonadComprehensions`, `NoImplicitPrelude`, `OverloadedLists`, `OverloadedStrings`,
  `ParallelListComp`, `PartialTypeSignatures`, `RecordWildCards`, `StrictData`,
  `TypeFamilies`, `ViewPatterns`
- **Per-file pragmas** -- only add when needed beyond defaults:
  - `{-# LANGUAGE TemplateHaskell #-}` -- for `makeSem` (Polysemy) and `makeAdaptorAndInstance` (Opaleye)
  - `{-# LANGUAGE Arrows #-}` -- for Opaleye query arrow notation
  - `{-# LANGUAGE QuasiQuotes #-}` -- for URI literals
  - `{-# LANGUAGE NoStrictData #-}` -- in DB `Types.hs` files for lazy Opaleye polymorphic records

### GHC Options (all packages)

`-O2 -threaded -flate-specialise -fspecialise-aggressively -fplugin=Polysemy.Plugin -W -Wno-partial-type-signatures`

### Custom Prelude

`NoImplicitPrelude` is enabled globally. Every module must import its package prelude:
```haskell
import Sonowz.<Package>.Imports
```
This re-exports `Relude` (with mtl items hidden) + `Polysemy` core modules + `StdEff`.
Use `pass` instead of `pure ()` (Relude convention, enforced by HLint).

### Import Style

- **Post-qualified imports** (modern syntax):
  ```haskell
  import Data.Text qualified as T
  import Control.Exception.Safe qualified as E
  ```
- **Import ordering:** external libraries first, then `Sonowz.Core.*`, then package prelude, then local modules.
- Prefer selective imports over blanket opens of large modules.

### Naming Conventions

- **Modules:** `Sonowz.<PackageName>.<Domain>.<Subdomain>` hierarchy
- **Types:** `PascalCase`. Suffix with role when applicable: `*Dto`, `*WriteDto`, `*Table`, `*Config`, `*Env`
- **Functions:** `camelCase`
- **Effect interpreters:** `run<EffectName><Strategy>` (e.g., `runHTTPIO`, `runMQueueDBNoti`)
- **DB query functions:** descriptive verbs (`getKeyValue`, `insertNotification`, `selectOneNotification`)
- **Opaleye query helpers:** `q` prefix (`qSelectByUid`, `qInsertKeyValue`)
- **CLI parsers:** `p` prefix (`pConfig`, `pWarpPort`, `pPGSConnectInfo`)
- **Effect type aliases:** `*Effects` suffix (`DBEffects`, `HandlerEffects`, `WorkerEffects`)

### Type Annotations

- All exported/top-level functions must have type signatures.
- Partial type signatures (`_` wildcard) are acceptable for complex Polysemy effect rows:
  ```haskell
  stdEffToWebHandler :: Sem _ a -> Handler a
  ```
- `Members '[Effect1, Effect2] r =>` syntax for effect constraints.

### Records

- **`RecordWildCards`** is the primary style for construction and pattern matching:
  ```haskell
  return StdMessage {..}
  runApp Env {..} = ...
  ```
- `DuplicateRecordFields` is enabled; multiple types share field names like `uid`, `key`, `value`.
- `OverloadedRecordDot` is NOT used. Access fields via traditional accessor functions.

### Deriving

Use explicit deriving strategies:
```haskell
newtype Uid = Uid Int
  deriving (Eq, Show, Read)
  deriving newtype (ToField, FromField)
  deriving (Num, ToJSON, FromJSON) via Int

newtype DatabaseException = DatabaseException Text
  deriving (Show)
  deriving anyclass (Exception)
```
Standalone deriving for orphan-adjacent instances:
```haskell
deriving via Int instance DefaultFromField SqlInt4 Uid
```

### Effect System (Polysemy)

- Define effects as GADTs with `makeSem`:
  ```haskell
  data HTTP m a where
    FetchURL :: URI -> HTTP m Text
  makeSem ''HTTP
  ```
- Compose effect stacks with `&` (reverse application):
  ```haskell
  action & runHTTPIO & runReader config & embedToFinal & runFinal @IO
  ```
- Concatenate effect lists with the `<>` type operator:
  ```haskell
  type HandlerEffects = [Reader Env, HTTP, Error ServerError] <> DBEffects
  ```

### Error Handling

- Model errors as Polysemy `Error` effects: `Error ServerError`, `Error HttpException`
- Exception types are newtypes deriving `Exception`:
  ```haskell
  newtype ParseException = ParseException Text
    deriving (Show) deriving anyclass (Exception)
  ```
- Use combinators from `Sonowz.Core.Error.Effect`: `runErrorAsLogging`, `mapErrorAs500`,
  `unsafeErrorToIO`, `catchAnyException`, `foreverCatch`
- Bridge IO exceptions via `fromExceptionSem` and `safe-exceptions`
- In web handlers, use `webLiftIO` to wrap IO, converting exceptions to `ServerError`

### Database Layer (Opaleye)

- Polymorphic record pattern for table types (each field is a type parameter):
  ```haskell
  data KVS c1 c2 c3 = KVS { uid :: c1, key :: c2, value :: c3 }
  ```
- Use `{-# LANGUAGE NoStrictData #-}` in DB `Types.hs` for lazy fields.
- Queries use arrow notation (`proc ... -> do ... returnA -< ...`).
- TH: `makeAdaptorAndInstance` for Opaleye/Profunctor instances.

### Module Exports

Prefer explicit export lists on all modules. Exceptions: some `DB/Types.hs` files export everything.
