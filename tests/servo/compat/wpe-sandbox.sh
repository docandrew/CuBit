#!/usr/bin/env bash
# WEB-001 survey: the AppArmor profile that lets the survey's sandboxed
# tools create user namespaces on a host that restricts them (Ubuntu 24.04's
# kernel.apparmor_restrict_unprivileged_userns=1), and its check.
#
#     wpe-sandbox.sh profile    print the profile for tools.nix's binaries
#     wpe-sandbox.sh verify     check the installed profile does its job
#
# The profile names exact Nix store paths (tools.nix passthru.sandboxPaths),
# never wildcards: any user can add a store path a wildcard would match.
# It grants `userns` to WPE WebKit's bwrap and to passt (and its re-exec as
# passt.avx2), and strips every capability from whatever they execute (the
# bwrap-userns-restrict pattern; see the profile's header). Installing it
# needs root:
#
#     wpe-sandbox.sh profile > cubit-survey.apparmor
#     sudo install -m 644 cubit-survey.apparmor /etc/apparmor.d/cubit-survey
#     sudo apparmor_parser -r /etc/apparmor.d/cubit-survey
#
# A nixpkgs update changes the paths: print and install the profile again.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)

paths() {
  nix eval --impure --raw -f "$here/tools.nix" sandboxPaths \
    --apply 's: "${s.bwrap} ${s.passt} ${s.passtAvx2}"'
}

profile() {
  local bwrap passt avx2
  read -r bwrap passt avx2 <<<"$(paths)"
  # What bwrap and passt themselves may do (upstream's bwrap profile).
  local rules='  allow file rwlkm /{**,},
  allow network,
  allow unix,
  allow ptrace,
  allow signal,
  allow mqueue,
  allow io_uring,
  allow userns,
  allow mount,
  allow umount,
  allow pivot_root,
  allow dbus,'
  cat <<EOF
# CuBit WEB-001 compatibility survey (tests/servo/compat/wpe-sandbox.sh).
# User namespaces for exactly these Nix store binaries, by exact path, in
# the pattern of AppArmor's bwrap-userns-restrict (Ubuntu 25.04 and later;
# 24.04 lacks it): the binary itself may create a user namespace and use
# capabilities in it to set up its sandbox, and everything it executes is
# stacked with cubit_survey_unpriv, which denies every capability. So no
# process started through these binaries gets the capabilities the
# kernel.apparmor_restrict_unprivileged_userns restriction withholds;
# everything else stays under that restriction.
#   bwrap: WPE WebKit's sandbox (its BWRAP_EXECUTABLE). WebKit's web
#     process and xdg-dbus-proxy run inside bwrap, so in the child profile.
#   passt, passt.avx2: the survey's passt in socket mode (QEMU's network),
#     which sandboxes itself in its own namespaces. passt re-executes itself
#     as passt.avx2 on AVX2 CPUs (that exec keeps passt's rights); pasta is
#     a symlink to passt, and a command pasta starts is in the child profile.
abi <abi/4.0>,
include <tunables/global>

profile cubit_wpe_bwrap $bwrap flags=(attach_disconnected,mediate_deleted) {
  allow capability,
$rules
  # Stacked: bwrap sets no-new-privs before it executes the sandboxed program.
  allow pix /** -> &cubit_wpe_bwrap//&cubit_survey_unpriv,
}

profile cubit_survey_passt $passt flags=(attach_disconnected,mediate_deleted) {
  allow capability,
$rules
  allow px $avx2 -> cubit_survey_passt_avx2,
  allow pix /** -> &cubit_survey_passt//&cubit_survey_unpriv,
}

profile cubit_survey_passt_avx2 $avx2 flags=(attach_disconnected,mediate_deleted) {
  allow capability,
$rules
  allow pix /** -> &cubit_survey_passt_avx2//&cubit_survey_unpriv,
}

# No capability, in or out of a user namespace, for anything started
# through the binaries above (and anything those start).
profile cubit_survey_unpriv flags=(attach_disconnected,mediate_deleted) {
$rules
  allow pix /** -> &cubit_survey_unpriv,
  audit deny capability,
}
EOF
}

verify() {
  local failed=0
  work=$(mktemp -d)
  trap 'rm -rf "$work"' EXIT
  pass() { echo "PASS $*"; }
  fail() { echo "FAIL $*"; failed=1; }
  . "$here/wpe-env.sh"
  echo "host: $(uname -n), $(sysctl -n kernel.apparmor_restrict_unprivileged_userns 2>/dev/null \
    | sed 's/^/apparmor_restrict_unprivileged_userns=/')"
  echo "bwrap: $WPE_BWRAP"
  echo "passt: $PASST"
  for name in WEBKIT_DISABLE_SANDBOX_THIS_IS_DANGEROUS WEBKIT_DISABLE_SANDBOX; do
    if [ -n "${!name+set}" ]; then fail "$name is set"; fi
  done

  # 1. An unprofiled binary is still denied.
  if unshare -rn true 2>"$work/unshare.err"; then
    fail "unshare -rn true succeeded: unprofiled user namespaces are not restricted"
  else
    pass "unprofiled 'unshare -rn true' denied ($(tr -d '\n' <"$work/unshare.err" | head -c 80))"
  fi

  # 2. What the profile grants does not leak: a program started through
  #    the pinned bwrap, even with every capability requested, or by pasta,
  #    runs in the no-capability child profile. The first run of each is
  #    the control (the program does run there).
  local box=("$WPE_BWRAP" --unshare-user --uid 0 --gid 0 --cap-add ALL
             --ro-bind / / --dev /dev --proc /proc --tmpfs /tmp --)
  if "${box[@]}" true 2>"$work/bwrap.err"; then
    pass "bwrap runs a program in a user namespace (control)"
    for attempt in "unshare -n true" "unshare -rn true" "mount -t tmpfs none /tmp"; do
      if "${box[@]}" $attempt 2>"$work/bwrap.err"; then
        fail "bwrap --cap-add ALL: '$attempt' succeeded inside: capabilities leak"
      else
        pass "bwrap --cap-add ALL: '$attempt' refused inside ($(tr -d '\n' <"$work/bwrap.err" | head -c 80))"
      fi
    done
  else
    fail "bwrap: $(head -c 300 "$work/bwrap.err")"
  fi
  local pasta
  pasta="$(dirname "$PASST")/pasta"
  if "$pasta" --config-net -q -- true 2>"$work/pasta.err"; then
    for attempt in "unshare -n true" "unshare -rn true" "mount -t tmpfs none /tmp"; do
      if "$pasta" --config-net -q -- $attempt 2>"$work/pasta.err"; then
        fail "pasta -- '$attempt' succeeded: capabilities leak"
      else
        pass "pasta -- '$attempt' refused ($(tr -d '\n' <"$work/pasta.err" | head -c 80))"
      fi
    done
  else
    # The survey never has pasta start a command (only passt's socket
    # mode), so this is no failure; there is just nothing to check.
    echo "INFO pasta did not start a command ($(tr -d '\n' <"$work/pasta.err" | head -c 80))"
  fi

  # 3. passt as the survey runs it (socket mode) sandboxes itself.
  "$PASST" -f -q -s "$work/passt.socket" >"$work/passt.log" 2>&1 &
  local passt_pid=$!
  for _ in $(seq 50); do [ -S "$work/passt.socket" ] && break; sleep 0.1; done
  if [ -S "$work/passt.socket" ] && kill -0 "$passt_pid" 2>/dev/null \
     && [ "$(readlink /proc/$passt_pid/ns/user)" != "$(readlink /proc/self/ns/user)" ]; then
    pass "passt (survey mode) is up in its own user namespace ($(readlink /proc/$passt_pid/exe))"
  else
    fail "passt: $(head -c 300 "$work/passt.log")"
  fi
  kill "$passt_pid" 2>/dev/null || true
  wait "$passt_pid" 2>/dev/null || true

  # 4. WPE WebKit loads a page sandboxed: its web process runs under the
  #    pinned bwrap in another user namespace (drive_wpe.py --check).
  local tools
  tools=$(nix build --no-link --print-out-paths -f "$here/tools.nix")
  if "$tools/bin/python3" -I "$here/drive_wpe.py" --check "$work" >"$work/wpe.log" 2>&1; then
    pass "WPE: $(tail -n 1 "$work/wpe.log")"
  else
    fail "WPE: $(tail -n 5 "$work/wpe.log")"
  fi
  return $failed
}

case "${1:-}" in
  profile) profile ;;
  verify) verify ;;
  *) echo "usage: $0 profile|verify" >&2; exit 2 ;;
esac
