let
  sources = import ./sources.nix { };
in
self: super:
{
  binaryen = super.binaryen.overrideAttrs (oldAttrs: {
    version = "97";
    src = sources.binaryen;
    patches = [ ];
  });
}
