# Haskell bindings for [binaryen][binaryen]

[Binaryen][binaryen] is a compiler and toolchain infrastructure
library for WebAssembly, written in C++. This package defines
bindings to the [Binaryen API][binaryen-api].

[binaryen]: https://github.com/WebAssembly/binaryen
[binaryen-api]: https://github.com/WebAssembly/binaryen/blob/master/src/binaryen-c.h

## How to build

Install [Stack][stack] and [Nix][nix]. Then,

```shell
$ stack --nix build
```

[nix]: https://nixos.org/nix/
[stack]: https://docs.haskellstack.org/en/stable/README/
