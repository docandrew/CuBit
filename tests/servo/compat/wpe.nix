# WEB-001 compatibility survey: WPE WebKit (the stock release, not
# Playwright's patched WebKit) with its bubblewrap sandbox, WPEWebDriver and
# a headless MiniBrowser. nixpkgs packages WebKitGTK only; this is that
# derivation built as the WPE port of the same release (shared sources,
# nixpkgs's store-path bubblewrap patch). BWRAP_EXECUTABLE and
# DBUS_PROXY_EXECUTABLE are compiled in as exact store paths: the host's
# AppArmor profile grants user namespaces to that bwrap alone
# (tools.nix passthru.sandboxPaths).
{ pkgs }:
let
  version = "2.52.6";
in
pkgs.webkitgtk_6_0.overrideAttrs (old: {
  pname = "wpewebkit";
  inherit version;
  name = "wpewebkit-${version}";
  src = pkgs.fetchurl {
    url = "https://wpewebkit.org/releases/wpewebkit-${version}.tar.xz";
    sha256 = "13l5bjjv4cfri6lw741mpbpg0bsl1zzk1wihypyvf98nfprgxfmj";
  };
  patches = old.patches ++ [ ./wpe-minibrowser-user-script.patch ];
  outputs = [ "out" "dev" ];
  # gtk4 (no longer propagated) brought JPEG, PNG and zlib to the GTK build.
  buildInputs = old.buildInputs ++ [ pkgs.libwpe pkgs.libwpe-fdo pkgs.libdrm pkgs.woff2 pkgs.libjpeg pkgs.libpng pkgs.zlib ];
  propagatedBuildInputs = [ pkgs.libsoup_3 pkgs.glib ];
  cmakeFlags = [
    "-DPORT=WPE"
    "-DENABLE_INTROSPECTION=OFF"
    "-DENABLE_DOCUMENTATION=OFF"
    "-DENABLE_EXPERIMENTAL_FEATURES=OFF"
    "-DENABLE_BUBBLEWRAP_SANDBOX=ON"
    "-DBWRAP_EXECUTABLE=${pkgs.lib.getExe pkgs.bubblewrap}"
    "-DDBUS_PROXY_EXECUTABLE=${pkgs.lib.getExe pkgs.xdg-dbus-proxy}"
    "-DENABLE_MINIBROWSER=ON"
    "-DENABLE_WEBDRIVER=ON"
    "-DENABLE_WPE_PLATFORM=ON"
    "-DENABLE_WPE_PLATFORM_HEADLESS=ON"
    "-DENABLE_WPE_PLATFORM_DRM=OFF"
    "-DENABLE_WPE_PLATFORM_WAYLAND=OFF"
    # No GBM: headless pages render into shared memory, so WebDriver
    # screenshots need no GPU device (the survey user has no /dev/dri
    # rights, and the sandboxed web process gets none). libdrm stays: 2.52
    # does not build without it (its DRM_FORMAT constants).
    "-DUSE_GBM=OFF"
    "-DENABLE_WPE_QT_API=OFF"
  ];
  postFixup = "";
  passthru = { };
  meta = old.meta // {
    description = "Web content rendering engine, WPE port";
    mainProgram = "WPEWebDriver";
    homepage = "https://wpewebkit.org/";
    pkgConfigModules = [ ];
  };
})
