# Linux-hosted build baseline only; not the CuBit target/runtime environment.
{ nixpkgs ? (builtins.getFlake (toString ../..)).inputs.nixpkgs }:
let
  pkgs = import nixpkgs { system = "x86_64-linux"; };
  clangLink = pkgs.linkFarm "mesa-host-clang-link" [{
    name = "lib/libclang-cpp.so";
    path = "${pkgs.llvmPackages.libclang.lib}/lib/libclang-cpp.so.${pkgs.lib.versions.majorMinor pkgs.llvmPackages.libclang.version}";
  }];
in pkgs.mkShell {
  LDFLAGS = "-L${clangLink}/lib";
  LIBRARY_PATH = "${clangLink}/lib";
  packages = with pkgs; [
    meson ninja pkg-config gcc bison flex glslang
    (python3.withPackages (p: [ p.mako p.pyyaml p.packaging p.ply ]))
    libdrm expat zlib zstd
    llvmPackages.llvm llvmPackages.libclang llvmPackages.libclang.lib
    spirv-llvm-translator spirv-tools
  ];
}
