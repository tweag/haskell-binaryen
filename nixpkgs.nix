let
  rev = "4855aa62fa13052fb1a3daea68971503ab07a744";
  sha256 = "17sk264qw397zzw12x11ry5vj9qidgbmjsyj544ysg857b4qq9sj";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
