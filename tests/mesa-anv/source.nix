# Source-audit baseline, not a CuBit Mesa build or a production version choice.
builtins.fetchTarball {
  url = "https://archive.mesa3d.org/mesa-26.2.3.tar.xz";
  sha256 = "sha256-vhoX4anFe68PNpkOsdtme1fnGSCmKit+dyNYq6ox9AM=";
}
