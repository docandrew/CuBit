# Build this JSON artifact and use its values for Cargo's native dependencies.
# Store references keep every declared archive and pkg-config input reachable.
{ pkgs }:
let
  media = import ./. { inherit pkgs; };
  cross = pkgs.pkgsCross.musl64;
  lib = pkgs.lib;
  archives = with media; [
    "${base}/lib/gstreamer-1.0/libgstapp.a"
    "${base}/lib/gstreamer-1.0/libgstopus.a"
    "${opus}/lib/libopus.a"
    "${base}/lib/gstreamer-1.0/libgstaudioconvert.a"
    "${base}/lib/gstreamer-1.0/libgstaudioresample.a"
    "${base}/lib/gstreamer-1.0/libgstaudiomixer.a"
    "${base}/lib/gstreamer-1.0/libgstvolume.a"
    "${good}/lib/gstreamer-1.0/libgstmatroska.a"
    "${good}/lib/gstreamer-1.0/libgstvpx.a"
    "${vpx}/lib/libvpx.a"
    "${zlib}/lib/libz.a"
    "${base}/lib/libgstpbutils-1.0.a"
    "${base}/lib/libgstaudio-1.0.a"
    "${base}/lib/libgstriff-1.0.a"
    "${base}/lib/libgsttag-1.0.a"
    "${base}/lib/libgstapp-1.0.a"
    "${base}/lib/libgstvideo-1.0.a"
    "${core}/lib/libgstbase-1.0.a"
    "${core}/lib/libgstreamer-1.0.a"
    "${glib}/lib/libgobject-2.0.a"
    "${glib}/lib/libgmodule-2.0.a"
    "${glib}/lib/libglib-2.0.a"
    "${ffi}/lib/libffi.a"
    "${pcre.out}/lib/libpcre2-8.a"
    "${core}/lib/gstreamer-1.0/libgstcoreelements.a"
    "${base}/lib/gstreamer-1.0/libgstplayback.a"
    "${base}/lib/gstreamer-1.0/libgsttypefindfunctions.a"
    "${base}/lib/gstreamer-1.0/libgstvideoconvertscale.a"
  ];
  # Preserve the proven linker ordering, including repeated search directories.
  flags = map (a: "-Lnative=${builtins.dirOf a}") archives
    ++ [ "-C" "link-arg=-Wl,--start-group" ]
    ++ lib.concatMap (a: [ "-C" "link-arg=${a}" ]) archives
    ++ [ "-C" "link-arg=-Wl,--end-group" ];
  pcInputs = with media; [ core base bad glib ] ++ (with cross; [
    zlib.dev libffi.dev pcre2.dev bzip2.dev freetype.dev libpng.dev brotli.dev
  ]);
in pkgs.writeText "penny-media-environment.json" (builtins.toJSON {
  schema = 1;
  inherit archives;
  rustflags = lib.concatStringsSep " " flags;
  pkg_config_path = lib.concatStringsSep ":" (lib.concatMap (p:
    [ "${p}/lib/pkgconfig" "${p}/share/pkgconfig" ]) pcInputs);
})
