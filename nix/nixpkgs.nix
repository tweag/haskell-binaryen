let
  sources = import ./sources.nix { };
  binaryenOverlay = import ./binaryenOverlay.nix;
in
args: import (sources.nixpkgs) (args // { overlays = (args.overlays or [ ]) ++ [ binaryenOverlay ]; })
