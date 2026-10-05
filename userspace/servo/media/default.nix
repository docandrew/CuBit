# The caller supplies the repository-pinned nixpkgs package set.
# Static musl-ABI archives are finally linked with CuBit libc; native tests
# establish compatibility separately. Nothing here enables browser authority.
{ pkgs }:
let
  build = name: import (./. + "/${name}.nix") { inherit pkgs; };
  support = build "static-support";
in {
  glib = build "glib-static";
  core = build "gstreamer-static";
  base = build "gst-base-static";
  good = build "gst-good-static";
  bad = build "gst-bad-static";
  vpx = build "vpx-static";
  opus = build "opus-static";
  inherit (support) ffi pcre;
  zlib = pkgs.pkgsCross.musl64.zlib.static;
}
