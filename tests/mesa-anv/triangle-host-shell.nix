# Linux lavapipe oracle only; never a CuBit hardware acceleration result.
{ nixpkgs ? (builtins.getFlake (toString ../..)).inputs.nixpkgs }:
let pkgs = import nixpkgs { system = "x86_64-linux"; };
in pkgs.mkShell {
  packages = with pkgs; [ gcc pkg-config vulkan-headers vulkan-loader mesa
                         glslang spirv-tools python3 vulkan-validation-layers ];
  MESA_DRIVER_ROOT = "${pkgs.mesa}";
  VK_LAYER_PATH = "${pkgs.vulkan-validation-layers}/share/vulkan/explicit_layer.d";
}
