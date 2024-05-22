# Sonowz Backend

Here are all Haskell application codes used in my personal server.

# Architecture

This monorepo contains packages (named as `sonowz-*`) where each of them is either compiled to an executable or used as a library in the rest of packages.
The packages form MSA (MicroService Architecture), and currently all IPCs among executables are implemented with PostgreSQL, in a [Message Queue](sonowz-core/src/Sonowz/Core/MessageQueue/Effect.hs) form.
All packages are maintained using one same GHC version, specified in `stack.yaml`.

# TODOs

- Use `NamedRoutes` when `servant` version 19.x lands
- Update to GHC 9.10
  - Use `GHC2024` language edition
  - Adapt to [exception backtrace](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0330-exception-backtraces.rst)
- Fix auto binary compilation in the source tree
- opaleye
  - map to domain model in APIs of news-combinator and stock-noti
  - update declarations of `ToFields` and `FromFields`
