{
  description = "CuBitOS development and QEMU environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.doomgeneric = {
    url = "github:ozkl/doomgeneric/dcb7a8dbc7a16ce3dda29382ac9aae9d77d21284";
    flake = false;
  };

  outputs = { self, nixpkgs, doomgeneric }:
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
              freedoom
              gnat15
              gnatprove
              gnumake
              grub2
              qemu
              xorriso
              yasm
              z3
            ];

            shellHook = ''
              export DOOMGENERIC_SRC="${doomgeneric}"
              export DOOM_WAD="${pkgs.freedoom}/share/games/doom/freedoom1.wad"
              echo "CuBit development shell"
              echo "  Build:  make -C kernel world"
              echo "  Test:   tests/headless/run.sh --test desktop-display"
              echo "  Run:    make -C kernel run-desktop"
            '';
          };
        });
    };
}
