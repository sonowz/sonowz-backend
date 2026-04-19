---
name: webapp-develop
description: Trigger this skill when adding or modifying web interfaces, Servant endpoints, API handlers, or setting up web servers in the Sonowz backend monorepo. Use this when the user asks to "add an endpoint", "setup a web server", or "modify API handlers".
---

# Webapp Development

## Overview
This skill guides the implementation of Servant-based web interfaces within the Sonowz monorepo, ensuring consistency with established patterns for Polysemy effect integration and multi-threaded execution.

## Prerequisites
- `servant-server`, `warp`, and `aeson` dependencies in `package.yaml`.
- Familiarity with `sonowz-core` web utilities (`Sonowz.Core.Web.*`).

## Workflow

1.  **Module Separation**:
    *   Separate background worker logic if it already exists (e.g., `*Worker.hs`) from web interface logic (`Web.hs`).
    *   Web logic should reside in `Sonowz.<Package>.App.Web`.

2.  **API Definition**:
    *   Define the Servant API type using standard Servant combinators.
    *   Use `WebAppEnv` for configuration (port, root URI) and `Env` for shared resources (DB pools, etc.).

3.  **Handler Implementation**:
    *   Implement handlers using `Sem r` effects.
    *   Use `ServerT <API_Type> (Sem r)` for type-safe handlers.
    *   **Crucial**: Use `webLiftIO` (from `Sonowz.Core.StdEff.Effect`) instead of `liftIO` when executing `IO` actions within handlers. This ensures `IO` exceptions are caught and returned as `500 Internal Server Error`.

4.  **Natural Transformation (nt)**:
    *   Implement `runWithEffects` to convert `Sem` to Servant `Handler`.
    *   Standard stack: `Sem '[Error ServerError, StdLog, Embed IO, Final IO] x -> Handler x`.
    *   Order: `runReader envPgConnection` -> `embedToFinal` -> `stdEffToWebHandler`.

5.  **Main Wiring**:
    *   In `Main.hs`, use `pWarpPort` to generate `WebAppEnv`.
    *   Use `forkIO` to run background workers (when exists) so the main thread can stay responsive or run the web server.
    *   Run the web server using `runServer webEnv env` (or equivalent).

## Caveats

*   **Concurrency**: Prefer `forkIO` for long-running background workers to keep the web server running in the main thread, as seen in `sonowz-news-combinator`.
*   **Exception Handling**: Never use `liftIO` directly in handlers if the action can throw exceptions. Always prefer `webLiftIO` to prevent unhandled exceptions from crashing the handler thread and to provide feedback to the client.
*   **Imports**: Always import the `Resource` type from `Polysemy.Resource` when defining the `nt` type signature, or GHC will fail to resolve the type.
*   **Dependencies**: Explicitly check `package.yaml` for `warp` and `servant-server`; they may be missing in packages that were previously worker-only.
*   **Effect Stack**: The `stdEffToWebHandler` expects a specific effect stack. Ensure `embedToFinal` is called before it if the stack includes `Embed IO`.
*   **Module Naming**: Follow the convention `Sonowz.<PackageName>.App.Web` for web application modules.

## References
*   `sonowz-news-combinator/src/Main.hs`: Reference for `forkIO` and `WebAppEnv` setup.
*   `sonowz-news-combinator/src/Sonowz/NewsCombinator/App/Web.hs`: Reference for `Web.hs` structure and `nt` implementation.
