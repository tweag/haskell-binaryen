# `binaryen`

A fork of [`binaryen`](https://github.com/WebAssembly/binaryen), adding Haskell
raw bindings. This package is a part of the
[`asterius`](https://github.com/tweag/asterius) Haskell-to-WebAssembly compiler
project, yet it may also be useful to other WebAssembly-related Haskell projects
as well.

## Building and using

The custom `Setup.hs` script calls `cmake` and `make` to build `libbinaryen.a`
and the `binaryen` executables, use the `MAKEFLAGS` environment variable to pass
additional flags to `make` (e.g. `-jN`). A simple `stack build` should work
fine; for `cabal` users, `hpack` needs to be run on `package.yaml` to generate
`binaryen.cabal`.

[`Bindings.Binaryen.Raw`](https://github.com/tweag/binaryen/blob/asterius/src/Bindings/Binaryen/Raw.hs)
contains 1-to-1 raw bindings to the `binaryen` C
[API](https://github.com/tweag/binaryen/blob/asterius/binaryen/src/binaryen-c.h).
Executables which use this module will automatically link against `libstdc++`
and `libbinaryen`.

It's also possible to run the `binaryen` executables like `wasm-opt`; use
`Paths_binaryen.getBinDir` to get the executable location.
