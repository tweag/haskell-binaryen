# Haskell bindings for [binaryen][binaryen]

[Binaryen][binaryen] is a compiler and toolchain infrastructure library for
WebAssembly, written in C++. This package defines complete bindings to the
[Binaryen C API][binaryen-api], and can be useful to building WebAssembly
toolchains in Haskell.

[binaryen]: https://github.com/WebAssembly/binaryen
[binaryen-api]: https://github.com/WebAssembly/binaryen/blob/master/src/binaryen-c.h

## Versioning

Binaryen evolves fast and can introduce C API changes in every release. Before
building the Haskell bindings, please check the Binaryen version and make sure
it's no less than the minimum version listed below.

| Haskell bindings version | Minimum Binaryen version |
|--------------------------|--------------------------|
| 0.0.1.0                  | version_91               |


## How to build

This package relies on the system-provided Binaryen library. As long as that's
available, a simple `stack build` or `cabal build` command should work.

`Nix`-based build is also supported. Install [Stack][stack] and [Nix][nix].
Then,

```shell
$ stack --nix build
```

[nix]: https://nixos.org/nix/
[stack]: https://docs.haskellstack.org/en/stable/README/
