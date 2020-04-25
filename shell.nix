{pkgs ? import ./nixpkgs.nix {} }:

with pkgs;

mkShell {
  name = "haskell-binaryen";
  buildInputs = [ nix stack ];
}
