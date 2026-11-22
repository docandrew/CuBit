# GstPlay and WebRTC API libraries used by Servo. No network/capture plugins.
{ pkgs }:
let
  cross = pkgs.pkgsCross.musl64;
  glib = import ./glib-static.nix { inherit pkgs; };
  gst = import ./gstreamer-static.nix { inherit pkgs; };
  base = import ./gst-base-static.nix { inherit pkgs; };
in cross.stdenv.mkDerivation {
  pname = "penny-gst-bad-static-probe";
  version = cross.gst_all_1.gst-plugins-bad.version;
  src = cross.gst_all_1.gst-plugins-bad.src;
  patches = [ ./gstplay-disabled-audio.patch ];
  nativeBuildInputs = (with pkgs; [ meson ninja pkg-config python3 ]) ++ [ pkgs.glib ];
  buildInputs = [ glib gst base cross.pcre2 cross.libffi cross.zlib ];
  postPatch = ''
    patchShebangs scripts gst gst-libs ext
    # Explicit terminal disposal joins GstPlay's thread before Rust releases
    # native references. GObject can invoke dispose again at final unref.
    substituteInPlace gst-libs/gst/play/gstplay.c --replace-fail \
      '  gst_bus_set_flushing (self->api_bus, TRUE);' \
      '  if (self->api_bus) gst_bus_set_flushing (self->api_bus, TRUE);'
    # GstPlay 1.28.5 reports failure when a valid selection is unchanged.
    substituteInPlace gst-libs/gst/play/gstplay.c --replace-fail \
      'GST_DEBUG_OBJECT (self, "Stream selection did not change");' \
      'GST_DEBUG_OBJECT (self, "Stream selection did not change"); ret = TRUE;'
  '';
  mesonFlags = [
    "--default-library=static" "--buildtype=release" "-Dauto_features=disabled"
    "-Dorc=disabled" "-Dglib_debug=disabled"
  ];
  NIX_CFLAGS_COMPILE = "-ffunction-sections -fdata-sections";
  doCheck = false;
}
