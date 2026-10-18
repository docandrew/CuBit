# Hosted CuBit Text

This runs CuBit Text (docs/text-editor.md) on Linux. The editor core
(`userspace/lib/text`) and the app's shell (`userspace/apps/text/text_shells`, instantiated in `host/text_shell.ads`)
are the same units the native app will use. Only the host is replaced:
`host/text_host_files.adb` implements `Text_Hosts.Host` with Linux files
under granted roots (the stand-in for the filesystem queue and its
filesystem-scope grants) and an in-memory clipboard (the stand-in for the
desktop's clipboard service). Everything here is a Linux-hosted
demonstration, not live CuBit integration.

Always set TMPDIR outside /tmp first:

```sh
export TMPDIR=/home/doc/cubit-build-tmp
cd /home/doc/git/cubit
nix develop --command tests/text-editor/run.sh              # core + view tests
nix develop --command tests/text-editor/run.sh --core 42    # core tests with another seed
nix develop --command tests/text-editor/run.sh --screens    # view tests + PNG screenshots
nix develop --command tests/text-editor/run.sh --prove      # SPARK, level 2, memory-capped
nix develop --command tests/text-editor/run.sh --mutate     # mutation checks
nix develop --command tests/text-editor/run.sh --bench      # benchmarks (quiet host; MiB, default 100)
nix develop --command tests/text-editor/run.sh --interface  # rewrite text.ccl-interface from the table
nix develop --command tests/text-editor/run.sh --window [--dark] [file...]
```

## Grants in the harness

| Name shown | Folder | Rights |
| --- | --- | --- |
| `@repo:0/...` | this repository | read only (saving says "read-only place") |
| `@scratch:0/...` | `/home/doc/cubit-build-tmp/text-editor/files` | read and write |

Names outside a granted folder, or with `.`/`..` components, are refused
("not granted"). Saves write `.name.text-save` beside the file, flush it and
rename it over the original (`rename(2)`, the hosted twin of
`Queue_Rename`). The host stand-in does not fsync; the native host flushes
through `Queue_Flush`.

## What the tests cover

- `text_core_tests` (randomized, seeded; `run.sh --core SEED`):
  - the piece table against a reference string over 6,000 random inserts
    and deletes (pieces of up to 16 KiB, a paste of 40 KB, deletes of 50
    KB), with `Consistent` checked after every edit;
  - line starts and `Line_Of` against a reference over a 300 KB text with CR
    LF and UTF-8;
  - undo and redo of 1,400 random multi-cursor commands against snapshots;
    typing coalescing; history at capacity;
  - selection sets (sort, merge) against a coverage reference;
  - UTF-8 decoding against a plain decoder (200,000 sequences) and cluster
    rules (combining marks, flags, ZWJ, skin tones, CR LF, invalid bytes);
  - wrap rows and column/index round trips;
  - literal search against a naive search (20,000 cases) and the Pike VM
    against Python's `re` on 20,000 random patterns (`regex_reference.py`
    writes the corpus). Where a repeated group can match empty, backtracking
    engines end the loop on an empty iteration and a Pike VM prefers the
    consuming path (as RE2 does); there only whether and where a match starts
    are compared. `(a*)*b` over 16 KiB ends at once;
  - the lexers on directed cases and 20,000 random lines in every language
    and state;
  - marks: protected regions refuse edits, host edits undo;
  - the `text` CCL interface descriptor equals the command table.
- `text_view_tests`: the app through its shell. Opening real files, typing
  repaints its own row (damage), find, palette (ranking, CCL calls), go to
  symbol, multiple cursors, Ctrl+D, atomic save and a read-only refusal,
  diagnostics and generated regions, modal keys, F1, other languages, split
  views, Alt+drag column selection, double click, a 1 MiB single line.
  `--screens` writes `build/screen-*.ppm` and converts them to PNG.

## Benchmarks

`text_bench` writes synthetic files (an Ada-like file and a single-line
file of the requested size) and measures, at 1080p and 4K: open to first
frame and to fully indexed, keypress to frame complete for typing, cursor
moves, page down, wheel scroll and full repaints, literal and regex
search, select-all-delete and its undo, a 16 MiB paste and its undo,
typing at 10,000 cursors, and the single line's jumps and typing. Results
are in docs/text-editor.md ("Measurements"): Linux-hosted, -O3 -gnatp,
rendering into memory, not CuBit measurements.
