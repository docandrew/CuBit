{
  description = "CuBitOS development and QEMU environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.doomgeneric = {
    url = "github:ozkl/doomgeneric/dcb7a8dbc7a16ce3dda29382ac9aae9d77d21284";
    flake = false;
  };
  inputs.stb = {
    url = "github:nothings/stb/f58f558c120e9b32c217290b80bad1a0729fbb2c";
    flake = false;
  };

  outputs = { self, nixpkgs, doomgeneric, stb }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
    in {
      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
        in {
          default = pkgs.mkShell {
            packages = with pkgs; [
              alire
              binutils
              cvc5
              cpio
              e2fsprogs
              expat
              freedoom
              gperf
              gnat15
              gnatprove
              gnumake
              grub2
              ibm-plex
              libjpeg_turbo
              libpng
              perl
              pkg-config
              python3Packages.pillow
              qemu
              SDL2
              xorriso
              yasm
              zlib
              z3
            ];

            shellHook = ''
              export DOOMGENERIC_SRC="${doomgeneric}"
              export DOOM_WAD="${pkgs.freedoom}/share/games/doom/freedoom1.wad"
              export STB_SRC="${stb}"
              export IBM_PLEX_SANS_FONT="${pkgs.ibm-plex}/share/fonts/truetype/IBMPlexSans-Regular.ttf"
              export IBM_PLEX_MONO_FONT="${pkgs.ibm-plex}/share/fonts/truetype/IBMPlexMono-Regular.ttf"
              # Hosted developer tools built through Alire/GPRBuild do not
              # automatically consume Nix's compiler and linker flags.
              export C_INCLUDE_PATH="${pkgs.SDL2.dev}/include''${C_INCLUDE_PATH:+:$C_INCLUDE_PATH}"
              export LIBRARY_PATH="${pkgs.SDL2}/lib''${LIBRARY_PATH:+:$LIBRARY_PATH}"
              export LD_LIBRARY_PATH="${pkgs.SDL2}/lib''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
              echo "CuBit development shell"
              echo "  Build:  make -C kernel world"
              echo "  Test:   tests/headless/run.sh --test desktop-display"
              echo "  Run:    make -C kernel run-desktop"
            '';
          };
        });
    };
}
