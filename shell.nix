{ sources ? import ./nix/sources.nix { }
, binaryenOverlay ? import ./nix/binaryenOverlay.nix
, haskellNix ? import sources.haskell-nix { }
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2009
, nixpkgsArgs ? haskellNix.nixpkgsArgs // { overlays = haskellNix.nixpkgsArgs.overlays ++ [ binaryenOverlay ]; }
, pkgs ? import nixpkgsSrc nixpkgsArgs
, ghc ? "ghc8102"
, hsPkgs ? import ./default.nix { inherit pkgs ghc; }
}: hsPkgs.shellFor {
  packages = ps: with ps; [
    binaryen
  ];

  withHoogle = true;

  tools = {
    brittany = "latest";
    cabal-install = "latest";
    ghcid = "latest";
    hlint = "latest";
    ormolu = "latest";
  };

  buildInputs = with pkgs.haskellPackages; [
    pkgs.binaryen
  ];

  exactDeps = true;

  binaryenIncludeDir = "${pkgs.binaryen}/include";

  binaryenLibDir = "${pkgs.binaryen}/lib";
}
