# Haskell bindings for [binaryen][binaryen]

[`Bindings.Binaryen.Raw`](https://github.com/tweag/binaryen/blob/master/src/Bindings/Binaryen/Raw.hs)
contains 1-to-1 raw bindings to the `binaryen` C
[API](https://github.com/tweag/binaryen/blob/master/binaryen/src/binaryen-c.h).
Executables which use this module will automatically link against `libstdc++`
and `libbinaryen`.

It's also possible to run the `binaryen` executables like `wasm-opt`; use
`Paths_binaryen.getBinDir` to get the executable location.

[binaryen]: https://github.com/WebAssembly/binaryen
