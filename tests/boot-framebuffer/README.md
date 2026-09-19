# Boot framebuffer admission

The pure `Boot_Framebuffer` core validates the data layout, size budget and
page-rounded physical mapping before any graphics adapter consumes it. Kernel
console setup, memory reservation, framebuffer sysinfo and MAPFB now derive
from one published boot descriptor rather than independently interpreting raw
Multiboot geometry. This is not yet a multi-output registry or native GPU driver.

## Accepted backend and bounds

- Direct BGRX8888: depth 32, red 16/8, green 8/8, blue 0/8; positive dimensions
  <=65535, pitch divisible by 4 and covering a complete row.
- The current kernel/UI transfer backend admits at most 16 MiB and requires
  at least 9x26 pixels for the emergency console. The generic numeric core
  supports caller-supplied budgets up to 128 MiB; that is not an implemented
  larger desktop-buffer protocol.
- Text mode is exactly the existing B8000/80x25/pitch 160/depth 16 adapter. Its
  RGB tail is ignored and no userspace graphics mapping is published.
- Physical addresses must fit both the software direct-map region and the
  CPU-advertised physical-address width. Unknown CPU width does not acquire a
  compatibility default for graphics. This is numeric eligibility, not proof
  that an address decodes to real display memory.
- Mapping spans include leading/trailing partial pages. The whole span is
  reserved, excluded from kernel/stack/loader/module storage and ACPI/NVS/bad
  regions, and excluded from ordinary RAM mappings before WC mapping. RAM-backed
  firmware framebuffers remain supported; they need not be marked MMIO in the
  firmware map. A byte span fitting the limit is insufficient if its final
  page does not fit.

The descriptor is published only after the existing boot admission completes.
Unaligned physical addresses map from the aligned base and return the original
byte offset to userspace. MAPFB retains existing authority checks; a descriptor
is not a capability. The kernel pixel-offset helper now uses pitch rather than
width*4. Glyph indexing also now stays inside its cell, and failed backbuffer
allocation is checked before installing renderer callbacks.

## Evidence

```sh
nix develop -c make -C kernel test-boot-framebuffer prove-boot-framebuffer
nix develop -c make -C kernel cubit_kernel
nix develop -c python3 tests/boot-framebuffer/check-codegen.py
nix develop -c python3 tests/boot-framebuffer/native.py
nix develop -c bash tests/multiboot-entry/prove.sh
nix develop -c bash tests/multiboot-entry/test.sh
```

GNATprove discharges 44 checks, none unproved, assumed or justified. The contracts
establish successful descriptor consistency, pitch/extent preservation, mapping
alignment/coverage, physical and budget limits, and in-range pixel offsets for
valid coordinates. A small signed geometry helper separates numeric reasoning
from modular wire-field decoding. There are no runtime assertion dependencies
in the optimized kernel object; `Valid` is Ghost and erased. Decode uses 80 bytes
of stack and pixel addressing 8; the raw boot adapter remains below 2 KiB.

Hosted tests pass 17,427 descriptor cases and check 3,981,312 pixel positions over
every page offset and several row paddings. They include exact/non-page-aligned
limits, zero/overflow/oversized dimensions, short/misaligned pitch, mask/depth
mutations, text admission and budget edges.

Native fixtures use real GRUB plus a GDB stop at Ada entry to inject hostile
descriptors, without production test hooks. Eight rejected cases must stop
before allocator admission with one preserved panic and stable terminal output.
The CPU-width fixture explicitly disables host-width passthrough and advertises
36 physical bits; a framebuffer at 64 GiB must be rejected despite fitting the
software direct-map window.
Two synthetic RAM-backed cases (including unaligned/pitched memory) must complete
mapping, console initialization and ACPI setup. They test real memory writes,
not the existence of a physical display at those RAM addresses. All fixture
staging and logs live in a fresh `/tmp/cubit-fb.*` directory.

The native test revealed GRUB's RGB union alignment discrepancy. The upstream
entry adapter now admits 118 raw bytes and normalizes the masks into the 116-byte
internal representation. Its separate 11-check proof and 169,416-case assembly/
snapshot suite pass; see [entry evidence](../multiboot-entry/README.md).

Final native validation on 2026-09-14 also passed the module rejection fixtures,
legacy text/ACPI boot, four-CPU desktop/DOOM interaction, virtio display grants,
and USB-only LiveCD desktop/app loading at 1920x1080. The final laptop ISO is
`kernel/cubit_laptop_usb.iso`, SHA-256
`dbb46cd9602b9363ac67686aad461a0772aea271d73acf2809029a0172950f6a`.
The final USB screenshots/serial log are under `/tmp/cubit-usb-live.jrmq0dnu`;
test logs are `/tmp/cubit-fb-final-{native,proof,usb}.log`.
These are QEMU regressions; this round has not been tested on the physical laptop.

## Remaining trusted boundaries

These are numeric/codec proofs, not proofs of MMIO, CPUID correctness, firmware
truth, cache/MTRR behavior, framebuffer backing, the entire text renderer, page
table mutation or DMA. Native mapping tests supplement them. CPU physical width
does not establish device decoding or memory-encryption-key configuration.
Usable RAM can be reserved for scanout, but a lying firmware descriptor cannot
be identified as such purely from its numerical shape.

MAPFB remains a singleton device mapping with existing lifetime, partial mapping
failure and SMP/TLB semantics; it is not a new exclusive display lease. Native
driver takeover must quiesce writers and retire kernel panic access to obsolete
scanout mappings. Multi-output geometry, exclusive handoff, dynamic modesetting
and Vulkan are planned in [display architecture](../../docs/display-outputs-and-scaling.md).
