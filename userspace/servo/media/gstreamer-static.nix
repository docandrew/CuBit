# Static core only; no decoder plugins or production browser enablement.
{ pkgs }:
let
  cross = pkgs.pkgsCross.musl64;
  glib = import ./glib-static.nix { inherit pkgs; };
in cross.stdenv.mkDerivation {
  pname = "penny-gstreamer-static-probe";
  version = cross.gst_all_1.gstreamer.version;
  src = cross.gst_all_1.gstreamer.src;
  nativeBuildInputs = with pkgs; [ meson ninja pkg-config python3 bison flex glib ];
  buildInputs = [ glib cross.pcre2 cross.libffi cross.zlib ];
  postPatch = "patchShebangs scripts gst libs";
  mesonFlags = [
    "--default-library=static" "--buildtype=release"
    "-Dtests=disabled" "-Dexamples=disabled" "-Dbenchmarks=disabled"
    "-Dintrospection=disabled" "-Ddoc=disabled" "-Dptp-helper=disabled"
    "-Dlibunwind=disabled" "-Dlibdw=disabled" "-Ddbghelp=disabled"
    "-Dglib_debug=disabled" "-Dbash-completion=disabled"
    "-Dtools=disabled" "-Dnls=disabled" "-Dcoretracers=disabled"
  ];
  NIX_CFLAGS_COMPILE = "-ffunction-sections -fdata-sections";
  doCheck = false;
}
