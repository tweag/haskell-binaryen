let
  sources = import ./nix/sources.nix { };
  binaryenOverlay = self: super:
    {
      binaryen = super.binaryen.overrideAttrs (oldAttrs: {
        version = "97";
        src = sources.binaryen;
        patches = [ ];
      });
    };
in
args: import (sources.nixpkgs) (args // { overlays = [ binaryenOverlay ]; })
