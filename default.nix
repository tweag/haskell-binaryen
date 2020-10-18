{ sources ? import ./nix/sources.nix { }
, binaryenOverlay ? import ./nix/binaryenOverlay.nix
, haskellNix ? import sources.haskell-nix { }
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2009
, nixpkgsArgs ? haskellNix.nixpkgsArgs // { overlays = haskellNix.nixpkgsArgs.overlays ++ [ binaryenOverlay ]; }
, pkgs ? import nixpkgsSrc nixpkgsArgs
, ghc ? "ghc8102"
, systemBinaryen ? true
}: pkgs.haskell-nix.cabalProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "haskell-binaryen";
    src = ./.;
  };
  compiler-nix-name = ghc;
  modules = [{
    packages.binaryen.configureFlags = (
      if systemBinaryen
      then [
        "--flags=system-binaryen"
        "--extra-include-dirs=${pkgs.binaryen}/include"
        "--extra-lib-dirs=${pkgs.binaryen}/lib"
      ]
      else [ "--flags=-system-binaryen" ]
    );
  }];
}
