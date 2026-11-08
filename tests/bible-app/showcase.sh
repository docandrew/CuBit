#!/usr/bin/env bash
# The Bible's showcase screenshots (APP-005), from the demo image
# (make-demo.sh) booted in QEMU/KVM with no display, driven by keys over the
# monitor, captured with screendump at native resolution:
#   1  John 3 in the KJV, Literata, 3:16 chosen (tinted) and in view
#   2  Psalm 23 in the BSB
#   3  the Apps menu's Reference group (Dictionary, Bible)
#   4  the translation list open (F4) over John 3:16
#   5  John 3 in the Clementine Vulgate, 3:16 chosen
# Run under the shared build lock (it boots QEMU):
#   flock coordination/build.lock nix develop -c tests/bible-app/showcase.sh [DEMO_DIR] [OUT_DIR] [WxH]
set -euo pipefail
demo="${1:-/home/doc/cubit-build-tmp/bible-demo}"
out="${2:-/home/doc/cubit-build-tmp/bible-app/showcase}"
size="${3:-1920x1080}"
mkdir -p "$out"
work="$(mktemp -d "${TMPDIR:-/tmp}/bible-showcase.XXXXXX")"
serial="$work/serial.log"
monitor="$work/monitor.sock"
qemu=0
boot() {
  cp --reflink=auto "$demo/cubit-bible-demo.img" "$work/disk.img"   # the demo disk stays clean
  rm -f "$serial" "$monitor"
  qemu-system-x86_64 -accel kvm -machine q35 -cpu Broadwell,vendor=GenuineIntel -smp 4 -m "${SHOWCASE_MEMORY:-4G}" \
    -cdrom "$demo/cubit-bible-demo.iso" \
    -drive file="$work/disk.img",if=none,id=nvme0,format=raw -device nvme,serial=cubitnvme,drive=nvme0 \
    -vga none -device "virtio-vga,xres=${size%x*},yres=${size#*x}" -device virtio-tablet-pci \
    -display none -serial file:"$serial" -monitor unix:"$monitor",server,nowait \
    -qmp unix:"$work/qmp.sock",server,nowait -no-reboot &
  qemu=$!
}
trap 'kill $qemu 2>/dev/null || true; cp "$serial" "$out/serial-$size.log" 2>/dev/null || true; rm -rf "$work"' EXIT
wait_for() {
  for _ in $(seq 600); do grep -aqF "$1" "$serial" 2>/dev/null && return 0; sleep 0.1; done
  echo "showcase: did not see: $1" >&2; exit 1
}
keys() {
  for key in "$@"; do printf 'sendkey %s\n' "$key"; sleep 0.15; done | nc -U -q 1 "$monitor" >/dev/null
}
shot() {
  sleep 2   # every frame settled
  printf 'screendump "%s"\n' "$work/$1.ppm" | nc -U -q 1 "$monitor" >/dev/null
  sleep 1
  python3 -c "import sys; from PIL import Image; Image.open(sys.argv[1]).save(sys.argv[2])" \
    "$work/$1.ppm" "$out/$1-$size.png"
  echo "showcase: $out/$1-$size.png"
}
# A boot whose early NVMe reads fail ("NVMe: Read failed", programs "not
# found"; a CuBit storage flake, reported) is retried, at most 3 times.
for attempt in 1 2 3; do
  boot
  for _ in $(seq 600); do
    grep -aqF "bible: window ready" "$serial" 2>/dev/null && break
    grep -aqF "init spawn failed" "$serial" 2>/dev/null && break
    sleep 0.1
  done
  grep -aqF "bible: window ready" "$serial" 2>/dev/null && break
  echo "showcase: boot $attempt did not open the Bible; retrying" >&2
  cp "$serial" "$out/serial-$size-failed-$attempt.log"
  kill $qemu 2>/dev/null || true
  wait $qemu 2>/dev/null || true
done
wait_for "bible: window ready"
wait_for "bible: showing John 1 (KJV, Literata)"
sleep 2
# The pointer out of the way, over the wallpaper's right edge: a QMP
# absolute tablet event (0..32767 on each axis).
printf '%s\n' '{"execute":"qmp_capabilities"}' \
  '{"execute":"input-send-event","arguments":{"events":[{"type":"abs","data":{"axis":"x","value":32300}},{"type":"abs","data":{"axis":"y","value":6000}}]}}' \
  | nc -U -q 1 "$work/qmp.sock" >/dev/null
keys j o h n spc 3 shift-semicolon 1 6 ret
wait_for "bible: showing John 3 (KJV, Literata)"
shot 1-john-3-16-kjv-literata
keys p s spc 2 3 ret tab
wait_for "bible: showing Psalm 23 (BSB, Literata)"
shot 2-psalm-23-bsb-literata
keys tab tab tab
wait_for "bible: showing Psalm 23 (KJV, Literata)"
# The Apps menu: its last category row (Up wraps), then its submenu.
keys meta_l up right
shot 3-reference-menu
keys esc esc
keys j o h n spc 3 shift-semicolon 1 6 ret
keys f4
shot 4-translations-open
keys down down down ret
wait_for "bible: showing John 3 (VUL, Literata)"
shot 5-john-3-16-vulgate-literata
