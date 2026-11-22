# WEB-001 survey: the environment for passt and WPE WebKit (sourced by
# run-survey.sh and wpe-sandbox.sh, from tools.nix's pinned outputs).
# - PASST: the exact passt binary; the host's AppArmor profile grants user
#   namespaces to this store path only (wpe-sandbox.sh writes it).
# - WPE_ROOT, WPE_BWRAP: WPE WebKit and the bwrap it was built to run (the
#   other path the profile names).
# - EGL from the pinned Mesa: WPE's headless platform renders with EGL, and
#   a host that is not NixOS has no /run/opengl-driver.
# The sandbox is never disabled: no WEBKIT_DISABLE_SANDBOX* variable is set
# here, and drive_wpe.py refuses to run when one is.
here_wpe=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
wpe_paths=$(nix eval --impure --raw -f "$here_wpe/tools.nix" sandboxPaths \
  --apply 's: "${s.passt} ${s.bwrap}"')
read -r PASST WPE_BWRAP <<<"$wpe_paths"
WPE_ROOT=$(nix build --no-link --print-out-paths -f "$here_wpe/tools.nix" wpe)
wpe_mesa=$(nix build --no-link --print-out-paths -f "$here_wpe/tools.nix" mesa)
export PASST WPE_ROOT WPE_BWRAP
# libsoup finds TLS through GIO modules (glib-networking): an unwrapped
# Nix MiniBrowser has none ("TLS support is not available").
wpe_gio=$(nix build --no-link --print-out-paths -f "$here_wpe/tools.nix" glib-networking)
export GIO_EXTRA_MODULES=$wpe_gio/lib/gio/modules
# Skia renders on the CPU into shared-memory buffers: headless snapshots
# (WebDriver screenshots) then need no GBM device, and the sandboxed web
# process needs no GPU access (the survey user has no /dev/dri rights).
export WEBKIT_SKIA_ENABLE_CPU_RENDERING=1
export __EGL_VENDOR_LIBRARY_FILENAMES=$wpe_mesa/share/glvnd/egl_vendor.d/50_mesa.json
export GBM_BACKENDS_PATH=$wpe_mesa/lib/gbm LIBGL_DRIVERS_PATH=$wpe_mesa/lib/dri
unset here_wpe wpe_paths wpe_mesa wpe_gio
