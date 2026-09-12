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
  inputs.sameboy = {
    url = "github:LIJI32/SameBoy/213a12ce93d66b105a113debd9396306066a7cfc";
    flake = false;
  };
  # Pinned CBOR source for hosted evaluation and the native userspace control
  # app's bounded wire codec. Never linked into the kernel.
  inputs.cbor_ada = {
    url = "github:b-erdem/cbor_ada/ce9897cdd80dea21112c59b80a5c42f2921f59f0";
    flake = false;
  };

  outputs = { self, nixpkgs, doomgeneric, stb, cbor_ada, sameboy }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
    in {
      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
          # Freestanding math library: no Linux TLS canary or fortified libc
          # dependency. Native CuBit supplies process isolation/ELF runtime.
          sameboyMath = pkgs.openlibm.overrideAttrs (previous: {
            hardeningDisable = (previous.hardeningDisable or []) ++
              [ "stackprotector" "fortify" ];
            postInstall = (previous.postInstall or "") + ''
              mkdir -p $out/share/licenses/openlibm
              cp LICENSE.md $out/share/licenses/openlibm/
              # Preserve the individual BSD/ISC copyright notices too.
              find src amd64 bsdsrc -type f \( -name '*.c' -o -name '*.h' -o -name '*.S' \) \
                -exec cp --parents {} $out/share/licenses/openlibm/ \;
            '';
          });
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
              librsvg
              nodejs
              sameboyMath
              perl
              pkg-config
              python3Packages.pillow
              qemu
              rgbds
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
              export SAMEBOY_SRC="${sameboy}"
              export SAMEBOY_LIBM="${sameboyMath}/lib/libopenlibm.a"
              export SAMEBOY_LIBM_NOTICES="${sameboyMath}/share/licenses/openlibm"
              export CBOR_ADA_SRC="${cbor_ada}"
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
