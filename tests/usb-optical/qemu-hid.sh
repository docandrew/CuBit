#!/usr/bin/env bash
set -euo pipefail
if [[ "${1:-}" == --version ]]; then
    exec qemu-system-x86_64 "$@"
fi
: "${CUBIT_USB_MONITOR:?Set by run-hid.sh}"
storage_args=()
mouse_port=1
if [[ -n "${CUBIT_USB_CD:-}" ]]; then
    mouse_port=2
    storage_args=(-drive "file=$CUBIT_USB_CD,if=none,id=usbcdimage,format=raw,media=cdrom,readonly=on"
        -device usb-bot,id=usbcd,bus=usbcheck.0,port=1
        -device scsi-cd,bus=usbcd.0,lun=0,drive=usbcdimage)
fi
exec qemu-system-x86_64 "$@" \
    -device qemu-xhci,id=usbcheck \
    "${storage_args[@]}" \
    -device "usb-mouse,bus=usbcheck.0,port=$mouse_port" \
    -monitor "unix:$CUBIT_USB_MONITOR,server,nowait"
