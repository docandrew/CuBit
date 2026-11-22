# Audio volume control: initial implementation and next boundary

SameBoy now uses the existing native mixer stream path, with per-stream volume
and mute. The C-facing adapter is a small, serialized, single-stream facade over
`CuBit.Audio`; it does not grant device access or introduce a separate transport.
DOOM's existing audio engine is unchanged.

## Enforcement

An audio producer may change or close only streams identified by its existing
kernel-stamped authority tag. The mixer checks ownership and message shape
before converting stream indices or fixed-point gain values. Inactive indices,
unsupported formats and out-of-range gains are rejected. The currently supported
format is output-only, 48 kHz, stereo S16LE. An unavailable hardware path rejects
open rather than returning a ring nobody will consume.

HDA period events are accepted only from the registered HDA process with its
matching kernel-stamped tag and expected message shape. Normal audio clients
cannot claim ownership of a DMA period just by sending its message label.

These are targeted fixes, not a claim that the complete mixer or shared-memory
protocol has been formally verified. Further review must include shared ring
fields, stream/process lifetime, cleanup on process death and tag/PID reuse.
SameBoy now explicitly releases its stream and window before returning from
`main`: the current C CRT does not run libc `atexit` handlers on that path.
Forced termination/crash cleanup still needs service-side lifetime handling;
cooperative cleanup is not a substitute for it.

## System-wide volume and media keys

`CuBit.Audio_Control` is a separate master-control client (slot 26). The virtual
manifest role `mixer-control` maps to the actual mixer endpoint with a distinct
kernel-stamped authority tag. Ordinary playback endpoints cannot exercise its
get/set operations, even by forging the tag in message memory. Procmgr currently
grants this declared request only on its trusted system-startup path, not normal
Apps/OP_SPAWN launches. This is an initial installation-policy rule, not the final
interactive policy engine or a claim of signed service discovery.

The native desktop has a taskbar volume popup, slider and mute button. It consumes
Set-1 extended mute/down/up keys itself; focused apps retain only their own
stream controls. USB HID consumer-page support is a separate driver task.
Master attenuation is applied after summing streams and before final clipping.
Gain ramps by at most 256/65536 per stereo frame (about 5.3 ms full-scale), with
identical gain on left and right. Codec gain stays at unity. The desktop displays
acknowledged mixer state rather than assuming a control request succeeded.

Still pending:

- Extract the desktop popup into shared toolkit audio controls; replace the
  simple speaker geometry with themed artwork and add keyboard slider navigation
  and accessibility labels.
- Measure transition clicks and end-to-end latency on hardware; a bounded gain
  ramp does not by itself prove audible click-freedom.
- Persist the selected level through Config with an explicit write authority;
  do not add unrelated broad filesystem access to audio clients.

For this increment: SameBoy F8 toggles mute, F9/F10 adjust its stream by 5%,
initially 70%. Pause/reset/ROM changes discard its old queued audio. Playback
prefills 32 ms and preserves partial-write suffixes. Physical laptop listening
tests and end-to-end latency measurements are still required.

The taskbar QEMU regression captured the original test tone at approximately
2017 RMS sample units at master 100%, 1008 at master 50% (both media keys and
popup slider), zero while master-muted, and 2017 after restoration. App-local
volume/mute/pause tests passed independently. These are digital PCM amplitude
checks, not acoustic measurements or a formal safety/latency guarantee.
