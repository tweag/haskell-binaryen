{pkgs ? import ./nixpkgs.nix {} }:

with pkgs;

mkShell {
  name = "haskell-binaryen";
  buildInputs = [
    binaryen
    cabal-install
    haskell.compiler.ghc8101
    nix
    stack
  ];
}
