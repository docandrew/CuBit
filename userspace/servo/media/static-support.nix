{ pkgs }:
let
  cross = pkgs.pkgsCross.musl64;
in {
  ffi = cross.libffi.overrideAttrs (old: {
    dontDisableStatic = true;
    configureFlags = (old.configureFlags or []) ++ [ "--enable-static" "--disable-shared" ];
    NIX_CFLAGS_COMPILE = "-ffunction-sections -fdata-sections";
  });
  pcre = cross.pcre2.overrideAttrs (old: {
    dontDisableStatic = true;
    configureFlags = (old.configureFlags or []) ++ [
      "--enable-static" "--disable-shared" "--disable-jit" "--disable-jit-sealloc"
      "--disable-pcre2-16" "--disable-pcre2-32"
    ];
    NIX_CFLAGS_COMPILE = "-ffunction-sections -fdata-sections";
  });
}
