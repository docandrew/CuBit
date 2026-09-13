# Mixer control admission regression

From the repository root, using the Linux-hosted runtime:

```sh
nix develop -c bash -lc 'cd kernel && alr exec -- gprbuild -P ../tests/mixer-control/control.gpr && ../tests/mixer-control/build/main'
```

Executes the same pure admission function used before production mixer control
dispatch. Checks owner isolation, inactive and oversized stream indices, every
message length, malformed flags/reserved fields, unsupported formats and all
16.16 gain values through the first out-of-range value. Assertions and overflow
checks are enabled in this host test only. No GNATprove result is claimed.

Master-control tests additionally enumerate every wire length, all 0..101 levels
and mute values 0..2, deny every ordinary PID-valued tag (0..65535), and reject
malformed/reserved fields. They exercise the production admission function with
the distinct control tag; they do not simulate or prove the kernel stamp itself.

The ownership table is built from the service's private stream records, not
from client-controlled ring memory. Admission compares the kernel-stamped
authority tag; passing an arbitrary stream index does not authorize its control.
This test does not prove kernel tag authenticity, grant revocation, PID reuse,
shared-ring safety or mixer scheduling. Those remain separate boundaries.

Native integration: `tests/usb-optical/run-live.py --sameboy --sameboy-audio`
checks actual client→mixer→HDA PCM and volume/mute/pause behavior.
