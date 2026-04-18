---
name: hoogle-search
description: Use this skill whenever you need to find library functions, local functions, or understand how to use a specific type in the current Haskell project. 
---

# hoogle-search

## Overview
This skill provides a fast and accurate way to search for Haskell functions, types, and modules within the context of the current project's Stack environment. It uses `stack hoogle`, which is scoped to the project's Stackage snapshot and all local packages in the monorepo.

## Workflow

### 1. Basic Search
Search for a function by name or type signature:
```bash
stack hoogle -- "Text -> ByteString"
```
Or search for a specific function:
```bash
stack hoogle -- "mapMaybe"
```

### 2. Scoping Results
To filter results by package, use the `+` and `-` operators:
- **Include/Focus on a package:** `stack hoogle -- "Text -> Int +sonowz-core"`
- **Exclude a package:** `stack hoogle -- "Text -> Int -servant"`

### 3. Controlling Result Count
By default, it shows a limited number of results. To see more:
```bash
stack hoogle -- "--count=20 [a] -> Int -> a"
```

## Caveats
- **Invalid Flags:** Do NOT use `stack hoogle --generate`. In current versions of Stack, this option may be invalid. `stack` typically manages the Hoogle database automatically or via `--rebuild`.
- **Search Scope:** The search is strictly limited to the packages defined in the project's `stack.yaml` and the libraries in the current resolver (e.g., LTS 24.11).
- **Preference over context7:** For Haskell projects using Stack, `stack hoogle` is preferred over `context7` because it understands the exact versions of dependencies and local module exports used in the repository.

## References
- **Local Packages:** This repository contains 11 packages (e.g., `sonowz-core`, `sonowz-web`, `sonowz-auth`).
- **Standard Prelude:** The project uses `Relude` as a custom prelude, often re-exported through package-specific `Imports.hs` modules.
