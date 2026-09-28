# NetSurf browser shell

`netsurf.app` is a CuBit.UI application written in Ada that embeds the NetSurf
engine. NetSurf fetches, lays out and paints pages. Everything around the page
is native CuBit UI: Back, Forward and Reload/Stop buttons, the location field,
the vertical scrollbar, the status bar and the window title.

- `main.adb` holds the layout, rendering, input routing and the location
  field, which uses `CuBit.UI.Editor`.
- `browser_engine.ads/.adb` hold the C imports of the engine API and the
  C-convention callbacks that record engine state for the shell.
- The engine is `userspace/c/netsurf/netsurf-embed-cubit.c`, linked in as
  `netsurf-engine.o` (see `userspace/c/netsurf/README.md`).

## How the page is embedded

The page area is a `CuBit.UI.Surfaces.Surface`:

- `Surfaces.View` gives NetSurf a canvas limited to that rectangle of the
  window buffer. There is no copy, and NetSurf cannot draw outside it.
- `Surfaces.Route` translates window input into page coordinates. It handles
  pointer capture during drags, a single leave event, and keyboard focus.
- `Controls.Add_Surface` registers the page for hit testing and the cursor
  shape without harness-driven repaints. NetSurf reports its own damage.

NetSurf's scheduler runs through `CuBit.UI.App.Run`'s deadline hooks.
`Next_Deadline` returns the engine's next timer (fetch polling runs every
10 ms while a fetch is active), and `On_Deadline` runs it. Input waits are
bounded by that deadline, so the engine never busy-waits.

## Keys

| Key | Action |
| --- | --- |
| Ctrl+L | Focus the location field |
| Enter | Navigate to the location field's text (location field focused) |
| Esc | Restore the current URL (location field focused) |
| Esc | Stop loading (page focused) |
| F5, Ctrl+R | Reload |
| Alt+Left, Alt+Right | Back, Forward |

When the page does not use a key, arrows and Page Up/Down scroll the view.

## Authority

See `manifest.ccl`. The shell requests:

- the desktop service;
- outbound TCP with DNS, for `http:`;
- tls.svc with a `*:1-65535` scope, for `https:`.

The launch must be approved for network use. procmgr installs the TLS scope
only for a network-approved launch.

## Tests

- `make -C kernel ui-surfaces-test` runs hosted unit tests of the Surface
  widget (placement, clipping, input routing).
- `tests/headless/run.sh --test netsurf-https` runs live in CuBit under QEMU.
  It boots the shell with its homepage on a loopback HTTPS fixture. The test
  passes only when the shell reports `netsurf: native shell ready` and the
  fixture served the page over a verified TLS session through tls.svc.

This regression covers startup, fetching and painting. Input handling in the
shell (clicks, the location field, scrolling) has no automated test yet.
