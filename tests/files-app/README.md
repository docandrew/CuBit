# Hosted CuBit Files

This runs CuBit Files (docs/files-app.md) on Linux. It uses the same shared
units as the native app in `userspace/apps/files`: the policy units, the
queue client `Files_Queue` and the view `Files_View`. Only two parts are
replaced. `host/files_link.adb` stands in for the native channel glue.
`host/files_mock_service.adb` stands in for CuBit's filesystem service: it is
an Ada task that serves the real `CuBit.Filesystem_Queues` request/answer
rings and writes real `Directory.Page.V2` pages (`CuBit.Directory_Pages`)
into the transfer arena. Everything here is a Linux-hosted demonstration, not
live CuBit integration.

Always set TMPDIR outside /tmp first:

```sh
export TMPDIR=/home/doc/cubit-build-tmp
cd /home/doc/git/cubit
nix develop --command tests/files-app/run.sh            # scripted tests (188 checks)
nix develop --command tests/files-app/run.sh --prove    # SPARK proof, policy units, level 2
nix develop --command tests/files-app/run.sh --bench    # benchmarks (quiet host; optional size, default 1000000)
nix develop --command tests/files-app/run.sh --window   # interactive SDL2 window
nix develop --command tests/files-app/run.sh --window @host:0/home/doc/git/cubit @synthetic:1000000/
```

## Places served by the mock

| Place | What it is |
| --- | --- |
| `@host:0/...` | The host's `/`, read-only. |
| `@scratch:0/...` | `$FILES_SCRATCH`, or `/home/doc/cubit-build-tmp/files-scratch` by default. Read-write. |
| `@synthetic:N/` | N generated entries: one in ten is a folder, and folders hold 100 entries. No disk access. |

Paths are checked as the real service checks them: at most 4096 bytes, no
`.` or `..` components, and a known place. Anything else gets
`ACCESS_DENIED`. Mutations outside the scratch place get `READ_ONLY`.

## The interactive window

`files_window.adb` opens an SDL2 window (the Nix shell provides SDL2, and
QEMU uses it as well). It draws into the window surface and presents only the
damaged rectangle. Keyboard, text, wheel and pointer input are real. By
default it shows your home folder (read-only) on the left and a
100,000-entry synthetic folder on the right. F12 shows the timing overlay,
F10 quits. `FILES_WINDOW_SNAPSHOT=build/window.ppm` saves the window once
the panes settle and then exits, which checks the window path without a
person.

## Scripted tests

`files_tests.adb` runs `files_policy_tests` and `files_view_tests`.

- **Policy tests** compare the units against reference implementations:
  - natural name order, including 20,000 random pairs against a reference that does not use the sort prefix;
  - streamed and re-sorted orders, which must be sorted permutations, under every key and direction with random budgets;
  - a rule change in the middle of a sort, and cursor tracking;
  - filter results against a reference filter, including live first slices, refinement and widening;
  - viewport, marks;
  - the page decoder on good pages and hostile ones (bad version, `/` in a name, a cursor that does not advance, 2,000 random pages).
- **View tests** drive the view through the mock and the real queue. The
  fixture tree is created in the scratch place. They cover listing, natural
  order with folders first, Enter, Backspace landing on the folder you left,
  Alt+Left/Right history, type-ahead filter (refine, widen, Esc), marks
  (Insert, Shift+Down, Ctrl+A, Ctrl+I, Space), sorting (with the cursor
  following its entry), Tab, End and PgUp, click and double-click, and
  refused places. They write `build/files-*.ppm` screenshots.

## Benchmarks

`files_bench.adb` measures:

- listing 10k, 100k and 1M synthetic entries through the queue, until listed and sorted;
- full frames and key-to-frame-complete times at 1080p and 4K;
- re-sorting 1M entries;
- the first rows after each type-ahead keystroke.

Each pump gets the frame budget, 20,000 work units, which is about 0.5 to
2 ms on this host. Timing comes from `Ada.Real_Time`. Results are recorded in
docs/files-app.md under *Measurements*. They are hosted -O2 numbers with the
pixel loops at -O2 and the mock in the same process, so they are not CuBit
measurements.
