{ pkgs ? import ./nix/nixpkgs.nix { } }:

with pkgs;

mkShell {
  name = "haskell-binaryen";
  buildInputs = [
    binaryen
    cabal-install
    haskell.compiler.ghc8102
    nix
    stack
  ];
}
