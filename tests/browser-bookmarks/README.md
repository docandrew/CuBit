# Penny bookmarks

The native fixture uses two disposable VM boots against one private disk. It
opens Ctrl+D, saves a bookmark, closes the browser, boots again, edits the same
bookmark, and verifies both snapshot generations and exact favicon pixels.
It also captures Help → About Penny. The user's disk is never used.

Run from the repository root with the shared build lock:

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c bash -c \
  'bash userspace/servo/build-cubitshell.sh && python3 tests/browser-bookmarks/run-native.py'
```

Hosted model/dialog tests live in `bookmarks.gpr`. They exercise bounded storage,
folder cycles, nonempty-folder deletion, truncated/malformed input, failed-save
retry, pointer actions, stale-window rejection and opening saved addresses.

The current store holds 64 bookmarks/folders, with ASCII titles and HTTP(S)
addresses. Alternating checksummed snapshots in `/Bookmarks/browser-0.dat` and
`browser-1.dat` retain the last valid generation during a failed write. The
native serializer writes into a fixed buffer rather than returning a large
unconstrained Ada string through the bounded secondary stack.

This fixture verifies bookmark persistence, not session/tab restoration.
