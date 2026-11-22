# WEB-001 compatibility survey: the host-side tools, from the nixpkgs revision
# the repository's flake.lock pins (so a rerun gets the same reference
# browsers). Build with:
#
#     nix build --no-link --print-out-paths -f tests/servo/compat/tools.nix
#
# - Python with Playwright 1.61 drives the references: Playwright's Firefox
#   (Gecko) and the stock nixpkgs Chromium over CDP. Pillow and NumPy
#   compare screenshots.
# - passt gives the CuBit guest an L2 virtio-net link to the host's network
#   (DHCP, DNS forwarding, TCP/UDP translated to host sockets), not QEMU's
#   user-mode (slirp) stack. It is the stock nixpkgs passt, self-sandboxed:
#   it creates user and network namespaces, so on a host that restricts
#   unprivileged user namespaces (Ubuntu's apparmor_restrict_unprivileged_userns)
#   the host's AppArmor profile must grant `userns` to this exact binary
#   (passthru.sandboxPaths; tests/servo/compat/wpe-sandbox.sh writes the
#   profile). Without it passt refuses to start rather than run unsandboxed.
# - wpe (wpe.nix): stock WPE WebKit, WPEWebDriver and a headless MiniBrowser,
#   its web and network processes in WebKit's bubblewrap sandbox (bwrap from
#   the same exact-path profile). Never run with the sandbox disabled.
# - mesa: EGL for WPE's headless platform on a host that is not NixOS.
let
  lock = builtins.fromJSON (builtins.readFile ../../../flake.lock);
  locked = lock.nodes.nixpkgs.locked;
  nixpkgs = builtins.fetchTree {
    inherit (locked) type owner repo rev narHash;
  };
  pkgs = import nixpkgs { system = "x86_64-linux"; };
  passt = pkgs.passt;
  wpe = import ./wpe.nix { inherit pkgs; };
  python = pkgs.python3.withPackages (p: [ p.playwright p.pillow p.numpy ]);
in
pkgs.buildEnv {
  name = "penny-compat-tools";
  paths = [ python passt pkgs.chromium pkgs.imagemagick ];
  passthru = {
    playwrightBrowsers = pkgs.playwright-driver.browsers;
    chromium = pkgs.chromium;
    inherit passt wpe;
    mesa = pkgs.mesa;
    # libsoup's TLS (GIO's TLS backend) for WPE's network process.
    glib-networking = pkgs.glib-networking;
    # The binaries that create user namespaces, by exact store path: the
    # host's AppArmor profile names these and nothing else. passt re-executes
    # itself as passt.avx2 on CPUs with AVX2 (pasta is a link to passt).
    sandboxPaths = {
      passt = "${passt}/bin/passt";
      passtAvx2 = "${passt}/bin/passt.avx2";
      bwrap = pkgs.lib.getExe pkgs.bubblewrap;
      dbusProxy = pkgs.lib.getExe pkgs.xdg-dbus-proxy;
    };
  };
}
