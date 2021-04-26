{ sources ? import ./nix/sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import sources.nixpkgs (haskellNix.nixpkgsArgs // {
    overlays = haskellNix.nixpkgsArgs.overlays ++ [
      (import ./nix/binaryen.nix)
    ];
  })
, ghc ? "ghc8104"
}: pkgs.haskell-nix.cabalProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "haskell-binaryen";
    src = ./.;
  };
  compiler-nix-name = ghc;
}
