# Web fonts for tests/servo/run-shaping.py, from the nixpkgs revision the
# repository's flake.lock pins (the same bytes on every host):
#   nix build --no-link --print-out-paths -f tests/servo/shaping/fonts.nix
let
  lock = builtins.fromJSON (builtins.readFile ../../../flake.lock);
  locked = lock.nodes.nixpkgs.locked;
  nixpkgs = builtins.fetchTree { inherit (locked) type owner repo rev narHash; };
  pkgs = import nixpkgs { system = "x86_64-linux"; };
  noto = "${pkgs.noto-fonts}/share/fonts/noto";
in
pkgs.linkFarm "penny-shaping-fonts" [
  { name = "Amiri-Regular.ttf"; path = "${pkgs.amiri}/share/fonts/truetype/Amiri-Regular.ttf"; }
  { name = "NotoSansArabic.ttf"; path = "${noto}/NotoSansArabic.ttf"; }
  { name = "NotoSansDevanagari.ttf"; path = "${noto}/NotoSansDevanagari.ttf"; }
  { name = "NotoSansHebrew.ttf"; path = "${noto}/NotoSansHebrew.ttf"; }
  { name = "NotoSansThai.ttf"; path = "${noto}/NotoSansThai.ttf"; }
  # Outline emoji (with its ZWJ-sequence ligatures): Servo does not use
  # Noto Color Emoji's bitmap-only (CBDT) faces as a web font today.
  { name = "NotoEmoji.ttf"; path = "${pkgs.noto-fonts-monochrome-emoji}/share/fonts/noto/NotoEmoji.ttf"; }
  { name = "FiraCode-VF.ttf"; path = "${pkgs.fira-code}/share/fonts/truetype/FiraCode-VF.ttf"; }
  { name = "EBGaramond12-Regular.ttf"; path = "${pkgs.eb-garamond}/share/fonts/truetype/EBGaramond12-Regular.ttf"; }
]
