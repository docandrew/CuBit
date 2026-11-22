# Stable hit regions during partial repaint

A repair rectangle limits pixel writes, but must not remove controls elsewhere
in the window from the retained hit map. Canvas now carries an independent input
clip; ordinary nested clips constrain both, while repair clips constrain painting.
Translated views preserve both clips. Menus and combo popups use stable input
geometry while retaining clipped drawing.

Run from the repository Nix environment (requires the existing host font library):

```sh
python3 tests/ui-repair-hit-map/run.py --output /absolute/new/test-directory
```

The runner snapshots inputs and reuses the existing menu and combo suites.
It checks hit-map equivalence, nested/empty clips, translated views, and untouched
pixel sentinels at 100%, 125%, and 200% DPI. This is hosted functional evidence,
not a proof or native GPU test.

Native paired evidence is recorded in `native-paired.json`: at 100% DPI, the
repaired Files app scrolls a 72-entry directory to row 17, then accepts Refresh
by pointer and F5 by keyboard. The unrepaired UI reaches row 17 but times out
waiting for the same Refresh click. The interaction runners differ only in the
Files artifact path. Frozen kernel/runtime/libc/Mesa dependencies are explicit;
no physical Intel performance or higher-DPI native result is claimed.
The app and three channel helpers retain current explicit IPC deadline arguments.

## Native 125% DPI and minimum window size

`native-scaled125.json` records Settings applying 125% on a 1024×768 output,
then maximized Files scrolling, pointer Refresh and keyboard F5. The initial
Files pixels are identical before and after the change. The preceding run
with the original size contract leaves the scrollbar beyond the output and
fails the same scroll check. This is software CuBit evidence, not hardware
presentation timing or a formal proof.

UI.App.Open now accepts optional `minimum_width` and `minimum_height` client
dimensions separately from the initial size. Zero preserves existing behavior;
minima exceeding the initial size are rejected before IPC. Fixed-size windows
retain their initial dimensions. Files keeps its 860×540 initial size and opts
into a 480×280 minimum so DPI reconfiguration can keep controls on-screen.

## Native 200% DPI

`native-scaled200.json` records successful scrolling, pointer Refresh and F5
after Settings selects 200%, with the actual framebuffer checked as 1600×1200.
A private boot fixture changes only the Multiboot1 preferred width/height fields;
all other kernel bytes match the recorded seed. This uses software composition
and the firmware framebuffer. Production images and Virtio DMA limits were
unchanged. Earlier rejected attempts (insufficient Virtio scanout budget and
unmodified boot resolution) are retained and are not counted as DPI coverage.
Native menu interaction remains a separate open gate.

## Native menu interaction

The [native menu fixture](native-menu/README.md) now passes pointer and keyboard
selection after an actual8×8 partial repaint at100%DPI. Removing only the repair
loses the item hit region and fails the paired native assertion. This closes the
menu-repair integration check; it does not claim native scaled-menu or physical
GPU validation.
