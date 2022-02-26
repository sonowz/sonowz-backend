# Sonowz Backend

Here are all Haskell application codes used in my personal server.

# Architecture

This monorepo contains packages (named as `sonowz-*`) where each of them is either compiled to an executable or used as a library in the rest of packages.
The packages form MSA (MicroService Architecture), and currently all IPCs among executables are implemented with PostgreSQL.
All packages are maintained using one same GHC version, specified in `stack.yaml`.

# TODOs

- Use `OverloadedRecordDot` extension when GHC version 9.x lands
- Use `NamedRoutes` when `servant` version 19.x lands
