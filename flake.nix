{
  description = "CuBitOS development and QEMU environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.rust-overlay = {
    url = "github:oxalica/rust-overlay";
    inputs.nixpkgs.follows = "nixpkgs";
  };
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
  # SPARKTLS and its SPARK dependencies, compiled in place for CuBit's
  # userspace runtime by userspace/lib/tls/sparktls_cubit.gpr. To build
  # against local checkouts, use e.g.
  #   nix develop --override-input sparktls path:../sparktls
  inputs.sparktls = {
    url = "github:docandrew/sparktls/f7ea3a5eba7538e88f485f5ed252a5bd98364206";
    flake = false;
  };
  inputs.sparktlscrypto = {
    url = "github:docandrew/sparktlscrypto/b89c8bee8013498ac9008f92f4fd5480740e60df";
    flake = false;
  };
  inputs.sparkx509 = {
    url = "github:docandrew/sparkx509/ba9c37170911a3ef564472187f83a6b38dac8fb2";
    flake = false;
  };
  inputs.sparkentropy = {
    url = "github:docandrew/sparkentropy/f707e61678576b4748c040d645b8ed427a28f8c8";
    flake = false;
  };
  inputs.sparkmlkem = {
    url = "github:docandrew/sparkmlkem/5fbd0c9ae7a498f4bd5350547ebaffba381156fa";
    flake = false;
  };
  inputs.sparknacl = {
    url = "github:rod-chapman/SPARKNaCl/49e3bddf092561ce2b74c134a35acff91a2da9a4";
    flake = false;
  };
  inputs.libkeccak = {
    url = "github:damaki/libkeccak/f33be1c8120196d90f600b14d669b11d16149bf9";
    flake = false;
  };

  outputs = { self, nixpkgs, rust-overlay, doomgeneric, stb, cbor_ada, sameboy,
              sparktls, sparktlscrypto, sparkx509, sparkentropy, sparkmlkem,
              sparknacl, libkeccak }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
    in {
      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ rust-overlay.overlays.default ];
          };
          # The overlay revision in flake.lock pins this stable toolchain and
          # its prebuilt freestanding core/alloc. No rustup or Linux libc in
          # native CuBit executables; host tools still use the Linux target.
          cubitRust = pkgs.rust-bin.stable.latest.default.override {
            targets = [ "x86_64-unknown-none" ];
            # Native std/Turso bring-up builds the pinned library sources;
            # existing no_std apps continue using prebuilt core/alloc.
            extensions = [ "rust-src" ];
          };
          # One directory of the pinned SPARK crates for sparktls_cubit.gpr.
          sparkCrates = pkgs.linkFarm "cubit-spark-crates" [
            { name = "sparktls"; path = sparktls; }
            { name = "sparktlscrypto"; path = sparktlscrypto; }
            { name = "sparkx509"; path = sparkx509; }
            { name = "sparkentropy"; path = sparkentropy; }
            { name = "sparkmlkem"; path = sparkmlkem; }
            { name = "sparknacl"; path = sparknacl; }
            { name = "libkeccak"; path = libkeccak; }
          ];
          cubitRustVendor = pkgs.rustPlatform.importCargoLock {
            lockFile = ./userspace/rust/Cargo.lock;
          };
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
              cubitRust
              e2fsprogs
              expat
              freedoom
              gperf
              gnat15
              gnatprove
              gnumake
              grub2
              ibm-plex
              jemalloc
              jq
              mimalloc
              gperftools
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
              export CUBIT_RUST_VENDOR="${cubitRustVendor}"
              # Hosted allocator references only, never native CuBit linkage.
              export CUBIT_BENCH_MIMALLOC="${pkgs.mimalloc}/lib/libmimalloc.so"
              export CUBIT_BENCH_JEMALLOC="${pkgs.jemalloc}/lib/libjemalloc.so"
              export CUBIT_BENCH_TCMALLOC="${pkgs.gperftools}/lib/libtcmalloc.so"
              export PYTHONTZPATH="${pkgs.tzdata}/share/zoneinfo"
              export DOOMGENERIC_SRC="${doomgeneric}"
              export DOOM_WAD="${pkgs.freedoom}/share/games/doom/freedoom1.wad"
              export STB_SRC="${stb}"
              export SAMEBOY_SRC="${sameboy}"
              export SAMEBOY_LIBM="${sameboyMath}/lib/libopenlibm.a"
              export SAMEBOY_LIBM_NOTICES="${sameboyMath}/share/licenses/openlibm"
              export CBOR_ADA_SRC="${cbor_ada}"
              export CUBIT_SPARK_CRATES="${sparkCrates}/"
              # Mozilla root set for tls.svc's trust store (tools/pem_bundle_to_der.py).
              export CUBIT_CA_BUNDLE="${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
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
