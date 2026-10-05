{ pkgs }:
pkgs.pkgsCross.musl64.libopus.overrideAttrs (old: {
  pname = "penny-opus-static";
  dontDisableStatic = true;
  mesonFlags = (old.mesonFlags or []) ++ [ "--default-library=static" "--buildtype=release" ];
  NIX_CFLAGS_COMPILE = (old.NIX_CFLAGS_COMPILE or "") + " -ffunction-sections -fdata-sections";
})
