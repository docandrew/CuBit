# CPU media conversion, mixing and codecs; no device backend or ORC JIT.
{ pkgs }:
let
  cross = pkgs.pkgsCross.musl64;
  glib = import ./glib-static.nix { inherit pkgs; };
  gst = import ./gstreamer-static.nix { inherit pkgs; };
  opus = import ./opus-static.nix { inherit pkgs; };
in cross.stdenv.mkDerivation {
  pname = "penny-gst-base-static-probe";
  version = cross.gst_all_1.gst-plugins-base.version;
  src = cross.gst_all_1.gst-plugins-base.src;
  nativeBuildInputs = (with pkgs; [ meson ninja pkg-config python3 bison flex ]) ++ [ pkgs.glib ];
  buildInputs = [ opus glib gst cross.pcre2 cross.libffi cross.zlib ];
  postPatch = "patchShebangs scripts gst gst-libs";
  mesonFlags = [
    "--default-library=static" "--buildtype=release" "-Dauto_features=disabled"
    "-Dopus=enabled" "-Daudioconvert=enabled" "-Daudioresample=enabled" "-Daudiomixer=enabled" "-Dvolume=enabled"
    "-Dapp=enabled" "-Dvideoconvertscale=enabled" "-Dtypefind=enabled"
    "-Dplayback=enabled" "-Dorc=disabled" "-Dglib_debug=disabled"
  ];
  NIX_CFLAGS_COMPILE = "-ffunction-sections -fdata-sections";
  doCheck = false;
}
