# Explicit WebM/VP8/VP9 components for CuBit's CPU video path.
{ pkgs }:
let
  cross = pkgs.pkgsCross.musl64;
  glib = import ./glib-static.nix { inherit pkgs; };
  gst = import ./gstreamer-static.nix { inherit pkgs; };
  base = import ./gst-base-static.nix { inherit pkgs; };
  vpx = import ./vpx-static.nix { inherit pkgs; };
in cross.stdenv.mkDerivation {
  pname = "penny-gst-good-static-probe";
  version = cross.gst_all_1.gst-plugins-good.version;
  src = cross.gst_all_1.gst-plugins-good.src;
  nativeBuildInputs = (with pkgs; [ meson ninja pkg-config python3 ]) ++ [ pkgs.glib ];
  buildInputs = [ glib gst base vpx cross.pcre2 cross.libffi cross.zlib ];
  postPatch = "patchShebangs scripts gst ext";
  mesonFlags = [
    "--default-library=static" "--buildtype=release" "-Dauto_features=disabled"
    "-Dvpx=enabled" "-Dmatroska=enabled" "-Dorc=disabled"
    "-Dglib_debug=disabled"
  ];
  NIX_CFLAGS_COMPILE = "-ffunction-sections -fdata-sections";
  doCheck = false;
}
