# CCL themes and native Settings

Settings is a desktop-owned appearance page in **Apps → Settings**, not a
privileged third-party application. It offers Alloy Light/Dark, the embedded
Cubes/Cubie wallpapers or solid backgrounds, Fill/Fit/Center, and Apply/Revert. Tab/Shift+Tab
and Enter/Space also operate its controls. Revert discards unapplied edits.
Arbitrary image-file selection is not implemented yet.

## Config CCL

These are ordinary entries in `system.ccl` (or the live profile
`tests/hardware/system-live.ccl`), seeded by the existing CCL configuration path:

```lisp
(system-config v1
  (setting "desktop.appearance.theme.light"
    "(theme v1 (base alloy-light) (selection (rgb 40 120 138)))")
  (setting "desktop.appearance.theme.dark"
    "(theme v1 (base alloy-dark) (active-title-top (rgb 65 106 128)) (text (rgb 231 236 239)))"))
```

Merge those settings into the existing system-config form, rather than adding
a second form. The inner value is a CCL declaration, not a filename. The desktop
loads it at startup and reloads both entries on **Apply**. Previews use the loaded
palettes. Config edits made since the last load become visible after Apply.

Each declaration starts with `(theme v1 (base alloy-light))` or `alloy-dark` and
may override any semantic field once using `(field-name (rgb R G B))`:

`desktop`, `panel`, `face`, `edge`, `shadow`, `text`, `muted`, `accent`, `good`,
`danger`, `field`, `selection`, `selection-text`, `highlight`, `dark-shadow`,
`active-title-top`, `active-title-bottom`, `inactive-title-top`,
`inactive-title-bottom`.

Channels are decimal integers 0–255. Colors are opaque RGB; no alpha, geometry,
callbacks, imports, filesystem paths or authority declarations are accepted.
`#` comments and whitespace follow the common CCL declaration scanner. This
initial data profile deliberately does not evaluate expressions. The scanner
is shared with manifests/configurations; theme-specific schema validation lives
in `CuBit.UI.Theme_CCL` and can be statically linked by other consumers.

## Failure and authority boundaries

- Missing, malformed, unknown-version or oversized declarations fall back to
  the hardcoded Alloy palette for that selection. A custom palette cannot remove
  the built-in defaults. The maximum declaration is 1,024 bytes.
- Unknown fields, duplicate fields, invalid channels, incomplete forms and
  trailing input are rejected. The complete palette is installed only after
  successful validation; errors cannot publish a partial palette.
- Desktop requests Config access only under `desktop.appearance.`. Normal
  toolkit clients need no Config access: their desktop endpoint provides a
  read-only palette snapshot. There is no public appearance-write IPC verb.
- Four bounded inline replies carry the palette with one revision. Clients
  reject malformed, out-of-range, nonzero-padding or mixed-revision snapshots.
  Colors update on the application's existing UI dispatch thread, not via a
  background mutation. Custom renderers may retain their own palettes.
- Configuration notifications use the existing surface configuration path.
  Repaint reloads the palette, while same-size surfaces retain their buffers.
  Input resynchronization reloads it too, covering notification queue overflow.
- Painting performs no parsing, Config access or IPC. Applying settings
  invalidates the retained desktop scene to avoid stale-background artifacts.

Selected appearance preferences currently use one atomic versioned Config
entry (`desktop.appearance.v1`); theme declarations use the two keys above.
Config storage is not a promise of durable persistence: live images deliberately
have no `config.store`, and these settings do not write an internal disk or
force a Config-wide save. Missing preferences start with Alloy Light and Fill.

CCL data validation does not authenticate its publisher, authorize Config writes,
or prove that chosen colors are legible. Those are separate policy/UX concerns.
Nor does proving the parser establish correctness of the Config service,
compositor, rendering code, or physical framebuffer.

## Checks

All commands run from the repository root:

```sh
nix develop -c make -C kernel test-appearance prove-appearance
nix develop -c make -C kernel usb-live-iso
nix develop -c python3 tests/usb-optical/run-live.py --cpus 4 --settings
```

The hosted suite checks every channel value for every palette field, truncation,
single-byte mutations (including non-ASCII/NUL), rollback on rejection, and the
palette transfer codec. The native test drives Settings with keyboard input and
captures desktop/client screenshots while applying a theme.

The proof target includes the shared CCL declaration scanner, including bounded
comment scanning, as well as palette decoding and failure atomicity. It does not
claim that the entire CCL interpreter, toolkit or Config service is proven by
this target.

Validation (2026-09-14): 133 SPARK obligations discharged, zero unproved;
16,954 theme-loader cases plus palette-chunk rejection tests; 9 configuration
tests, 20 manifest tests, desktop protocol parity tests, and wallpaper bounds
tests passed. The final four-vCPU KVM USB-live run verified CCL loading at boot
and Apply, dark taskbar colors, and repaint of an already-open Files window.
Native Settings/Workbench screenshots were inspected; the Linux preview builds.
