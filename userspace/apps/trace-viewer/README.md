# Desktop trace viewer

The development desktop's Apps menu includes **Desktop trace**. It opens a
saved, checked archive at `@nvme:0/work/desktop-trace.cubittrace`, read-only.
The viewer does not start a capture or change trace producer settings.

Build through the standard target inside Nix:

```sh
flock --exclusive --nonblock coordination/build.lock \
  nix develop -c make -C kernel trace-viewer
```

`desktop-session-content`, the development disk payload and `world` include
this application. Refresh the development image/configuration to see the new
menu entry; building an executable alone does not update an already running
desktop. The separate USB/laptop hardware profile is not updated yet: it needs
capture-storage selection suitable for that platform rather than this fixed
NVMe path. No existing hardware image is replaced by the source integration.

Use N/P to change pages, arrows to select an event, R to reread the file, and
Esc to close. The five lanes show input dequeue, surface acceptance, client
draw, submission, and completion. Completion spans cover recorded submission
to collection, not display latch or physical photons. The page interval is
shown in microseconds. Event identity is queried through CCL's `trace` and
`trace-detail` interfaces without narrowing 64-bit identifiers.

Only a fully checked archive is displayed. Loss is labelled, incomplete or
corrupt files show a warning, and missing files or uncertain I/O show an
unavailable message. After uncertain I/O, close and reopen the application;
the quarantined reader instance cannot be reused. Loading is asynchronous,
with one 4 KiB transfer page and two-second per-request deadlines. No animation
or periodic idle repaint is introduced. The bounded archive supports at most
4096 events, retaining a 64-event page; page changes reread/validate the file.

The current capture writer is the native integration fixture documented in
`tests/compositor/trace-native/README.md`; ordinary on-device start/stop/save
controls are still outstanding. The viewer is not yet a CPU-stack flame graph,
and collected QEMU timings are not a supported-hardware latency benchmark.

## Validation

Native QEMU runs 242 and 243 exercise navigation, selection, reload, exact
return-page pixels and incomplete-capture rejection. Run 248 additionally
launches through the real Apps menu (no startup entry), then repeats navigation
and reload. Run 251 strengthens this by waiting for rendered desktop pixels and
confirming that the Apps menu is visibly open before selecting; its screenshot
was inspected. The earlier 248 menu screenshot was premature (bootstrap),
although the queued launch and subsequent viewer checks passed. Its isolated configuration places the same launch entry first for
a deterministic keyboard selection. The normal configuration places it after
Logs. No new process-manager exception or filesystem privilege was needed.

The Makefile recipe and staged-file dependency were executed in private build
249 using explicitly frozen runtime, fonts, and manifest compiler seeds. Its
binary is byte-identical to the native-tested application. This is not a claim
that a full current `make world` or the USB hardware image has been rebuilt.
SPARK policy and foreign-interface proof boundaries, 43,144 hosted checks and
87 proved checks are documented in `tests/observatory-metrics/archive-io/`.
The native application event loop and filesystem FFI body are regression-tested,
not SPARK-proved. Evidence is recorded alongside this application.
