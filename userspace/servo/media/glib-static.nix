# Compatibility probe: musl-ABI static libraries for subsequent CuBit linking.
# Building this derivation alone does not establish native CuBit support.
{ pkgs }:
let
  cross = pkgs.pkgsCross.musl64;
in cross.stdenv.mkDerivation {
  pname = "penny-glib-static-probe";
  version = cross.glib.version;
  src = cross.glib.src;
  nativeBuildInputs = with pkgs; [ meson ninja pkg-config python3 gettext perl ];
  buildInputs = with cross; [ libffi zlib pcre2 ];
  postPatch = "patchShebangs tools glib gobject gio";
  postInstall = ''patchShebangs --build "$out/bin"'';
  mesonFlags = [
    "--default-library=static" "--buildtype=release"
    "-Dtests=false" "-Dinstalled_tests=false" "-Ddocumentation=false"
    "-Dintrospection=disabled" "-Dnls=disabled" "-Dman-pages=disabled"
    "-Dselinux=disabled" "-Dlibmount=disabled" "-Dlibelf=disabled"
    "-Dsysprof=disabled" "-Ddtrace=disabled" "-Dsystemtap=disabled"
    "-Dglib_debug=disabled"
  ];
  NIX_CFLAGS_COMPILE = "-ffunction-sections -fdata-sections";
  doCheck = false; # Native CuBit runtime checks are separate, not host tests.
}
