# Haskell bindings for [binaryen][binaryen]

![](https://github.com/tweag/haskell-binaryen/workflows/pipeline/badge.svg?branch=master)

[Binaryen][binaryen] is a compiler and toolchain infrastructure library for
WebAssembly, written in C++. This package defines bindings to the [Binaryen C
API][binaryen-api].

## Versioning

Starting from `0.0.5.0`, we bundle the C++ sources of `binaryen` with this
package, so the package can be built and used as long as the C++ toolchain is
present in the build environment. The `system-binaryen` Cabal flag defaults to
`False`, but it can be manually enabled to link against the system-wide
`binaryen` library and avoid building the bundled C++ library.

| Haskell bindings version | Bundled Binaryen version |
| ------------------------ | ------------------------ |
| 0.0.6.\*                 | version_99               |

Older versions of this package links against the system-wide `binaryen` library,
so before building the package, please check the Binaryen version and make sure
it's no less than the minimum version listed below.

| Haskell bindings version | Minimum Binaryen version |
| ------------------------ | ------------------------ |
| 0.0.1.\*                 | version_91               |
| 0.0.2.\*                 | version_94               |
| 0.0.3.\*                 | version_96               |
| 0.0.4.\*                 | version_97               |
| 0.0.5.\*                 | version_98               |

## How to build

A simple `stack build` or `cabal build` command should work.

For Windows `cabal` users: run the build in a msys2 mingw64 shell.

[`nix`][nix]-based build is also supported:

```shell
$ nix-shell --pure --run "cabal v2-run binaryen:test"
$ nix-shell --pure --run "cabal v2-run binaryen:test --flags=system-binaryen --extra-include-dirs=\$binaryenIncludeDir --extra-lib-dirs=\$binaryenLibDir"
```

Use the [`asterius`][cachix-asterius] Cachix bucket for reusing binary cache
built on CI.

[binaryen]: https://github.com/WebAssembly/binaryen
[binaryen-api]: https://github.com/WebAssembly/binaryen/blob/master/src/binaryen-c.h
[cachix-asterius]: https://app.cachix.org/cache/asterius
[nix]: https://nixos.org/nix
