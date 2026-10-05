# Entry point for nix build --impure --file ... --argstr repo REPOSITORY.
{ repo }:
let
  flake = builtins.getFlake "path:${repo}";
  pkgs = import flake.inputs.nixpkgs { system = "x86_64-linux"; };
in import ./environment.nix { inherit pkgs; }
