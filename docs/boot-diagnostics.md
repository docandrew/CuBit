# Boot diagnostics and renderer handoff

CuBit does not need a VT, TTY, terminal emulator or scrolling graphical console
to explain a failed boot. The bootstrap framebuffer has one fixed diagnostic
panel. Normal boot never waits for someone to read it.

## Implemented scope

The panel retains the current kernel initialization step, last completed step,
latest complete diagnostic line, and first kernel fatal error. Content is bounded
to 96 printable ASCII characters per row; long lines are clipped, control bytes
sanitized. A failure freezes the diagnostic content. The old bitmap glyphs remain
appropriate here: this renderer needs neither allocation nor a font service.

Only the panel rectangle is cleared once. Subsequent updates repaint the affected
text row, not the entire framebuffer. There is no backbuffer, scrolling or mode
change. Small framebuffer modes are clipped; normal modes use larger glyphs.
The obsolete `Video.VGA` scrolling/backbuffer implementation was removed. Legacy
GRUB text-mode diagnostics (`Video.EGA`) remain a separate fallback.

Setup runs after memory mapping and CPU-local interrupt state are ready. Earlier
failures remain serial-only on graphical boots. Secondary CPUs initialize their
CPU-local state before submitting diagnostic text. There is no delay added to
normal boot to make the panel readable.

Raw diagnostic text is best effort: concurrent producers may interleave and a
busy renderer may drop a write. It is not structured evidence of service health.
Only explicit kernel stage/failure operations set those fields. In particular,
a driver printing "failed" does not automatically latch a typed fatal error.
The existing serial output remains; this bounded panel is not a full log store.
Routine demand-page faults are silent on both outputs. Rejected accesses,
protection violations and kernel-fatal errors retain their diagnostic reporting;
entering the page-fault handler is not itself evidence of a failure.

## Ownership and retirement

The graphical renderer has three states: unavailable, active, permanently retired.
Two already-authorized boundaries retire it:

- MAPFB, after authority validation but **before the first userspace page mapping**.
- Publishing a nonzero GPU_IS_PRIMARY value, before a native boot GPU starts.

Retirement first closes new admission, then acquires the renderer lock to drain
an admitted writer. Every writer checks admission again under that lock and
issues an x86 store fence before releasing it (important for write-combined
memory). Retirement clears its render pointer before returning. Setup, text and
panic paths cannot reactivate a retired renderer. This is conservative: failure
of the subsequent mapping or native driver does not reopen kernel graphics.

Paints hold an IRQ-masking lock and perform no allocation, IPC or serial output.
Normal diagnostic writes and panic use one nonblocking acquisition attempt;
panic cannot wait on or recursively enter a broken painter. This does not turn
panic reporting into an NMI-safe or globally coordinated multi-CPU crash logger.

After retirement, panic remains serial-only on normal graphical boots. No unsafe
attempt is made to revive a firmware framebuffer after native modesetting. The
display broker's native path does not map a firmware buffer merely to suppress
console output anymore.

## Assurance and tests

`Boot_Panel` and `Boot_Font` are pure SPARK. Contracts cover irreversible lifecycle,
inactive/failure preservation, first-error retention and bounded text operations;
GNATprove also checks glyph indexing. `Boot_Framebuffer` separately proves numeric
admission and pixel-offset bounds. These are sequential properties, not proofs of
firmware truth, concurrent ownership, device visibility or the entire kernel.

`Boot_Diagnostics` is an explicitly unproved hardware adapter: address overlays,
atomic admission flags, raw locking and CPU fences live here. Its bounded state
is owned by the renderer. `Boot_Output` provides hardware-independent routing,
installed once by the boot CPU before APs or userspace start. This breaks TextIO's
hardware initialization dependency cycle without disabling elaboration checks.
Retirement also latches if no renderer was installed; no future setup can reopen it.

Run in the Nix environment:

```sh
nix develop -c make -C kernel test-boot-panel prove-boot-panel
nix develop -c python3 tests/boot-panel/native.py
nix develop -c python3 tests/boot-panel/native.py --panic
```

Hosted tests enumerate 137,256 model transitions, exercise clipping/sanitization
and glyphs, and use the actual renderer with RAM framebuffers and mocked mapping/
locks. They check padded pitches, boundary canaries, row-only damage, busy panic,
sticky first failure, and no writes after retirement at four resolutions. Mock
locking tests control flow, not SMP exclusion. The native test uses a private
four-CPU QEMU image without userspace, leaving the panel visible for a screenshot.
Its `--panic` variant uses GDB to enter the real last-chance handler after graphics
setup with a known failure string, checking a stable fatal stop and capturing the
retained panel. Neither fixture modifies the on-disk production kernel. Normal
desktop/native-output regressions separately exercise takeover.

Validated on 2026-09-22, all through Nix:

- Panel/font SPARK report: 17 checks, zero justified or unproved checks.
- Hosted panel/renderer tests and 17,427 framebuffer admission cases passed;
  the separate framebuffer proof passed too.
- Four-CPU native panel, injected fatal panel, ten hostile framebuffer boot
  fixtures, and legacy text-mode boot passed.
- `desktop-display` and `CUBIT_TEST_BOOT_HANDOFF=1 desktop-dual-output` passed,
  including cross-output drag, maximize, wallpaper/cursor restoration and the
  Settings display view. This is native CuBit in QEMU/KVM, not a Linux UI preview.
- Production `kernel/cubit_kernel.iso` rebuilt. Physical laptop validation is
  still needed; none of these results establishes a hardware latency bound.

## Later work

- A compact, versioned QR diagnostic capsule for physical/headless boot
  troubleshooting. It must contain only bounded non-secret status metadata,
  preserve a readable text panel, be independently checksummed, and be tested
  by decoding the native screenshot. It complements—not replaces—serial and a
  future authority-gated retained boot log.
- Typed userspace boot milestones/fatal results, rather than interpreting strings.
- A retained boot log exposed with appropriate read authority to a diagnostic app.
- Elapsed time/current-step timeout visibility without imposing a boot delay.
- An explicit boot-output identity and exclusive hardware lease instead of the
  GPU_IS_PRIMARY heuristic; general modeset transactions and DMA quiescence.
- A separately owned recovery display path, if graphical post-takeover panic
  reporting becomes necessary. Never silently reacquire a stale framebuffer.
