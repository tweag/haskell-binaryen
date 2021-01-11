let
  sources = import ./sources.nix { };
in
self: super:
{
  binaryen = super.binaryen.overrideAttrs (oldAttrs: {
    version = "99";
    src = sources.binaryen;
    patches = [
      ./binaryen-3481-fix.patch
    ];
  });
}
