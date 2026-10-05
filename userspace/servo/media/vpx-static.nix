# Pinned static codec library, linked against CuBit libc by the native probe.
{ pkgs }:
let
in pkgs.pkgsCross.musl64.libvpx.overrideAttrs (old: {
  pname = "penny-vpx-static-probe";
  dontDisableStatic = true;
  outputs = [ "out" "dev" ];
  postInstall = "";
  # libvpx passes compiler-driver switches (-m64) to LD during configure.
  preConfigure = (old.preConfigure or "") + ''
    export LD="$CC"
  '';
  configureFlags = builtins.filter (flag: !(builtins.elem flag [
    "--enable-shared" "--enable-examples" "--enable-install-bins"
    "--enable-webm-io"
  ])) old.configureFlags ++ [
    "--disable-shared" "--enable-static" "--disable-examples"
    "--disable-install-bins" "--disable-webm-io"
  ];
  NIX_CFLAGS_COMPILE = (old.NIX_CFLAGS_COMPILE or "") + " -ffunction-sections -fdata-sections";
})
