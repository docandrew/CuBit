# Hosted Vulkan oracle only, not CuBit GPU execution.
let
  flake = builtins.getFlake (toString ../..);
  pkgs = import flake.inputs.nixpkgs { system = "x86_64-linux"; };
in pkgs.mkShell {
  inputsFrom = [ flake.devShells.x86_64-linux.default ];
  packages = with pkgs; [ vulkan-headers vulkan-loader mesa vulkan-validation-layers ];
  MESA_DRIVER_ROOT = "${pkgs.mesa}";
  VK_LAYER_PATH = "${pkgs.vulkan-validation-layers}/share/vulkan/explicit_layer.d";
  shellHook = ''
    export C_INCLUDE_PATH="${pkgs.vulkan-headers}/include''${C_INCLUDE_PATH:+:$C_INCLUDE_PATH}"
    export LIBRARY_PATH="${pkgs.vulkan-loader}/lib''${LIBRARY_PATH:+:$LIBRARY_PATH}"
    export LD_LIBRARY_PATH="${pkgs.vulkan-loader}/lib''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
  '';
}
