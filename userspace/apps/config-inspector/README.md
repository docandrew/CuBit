# Config Inspector

A native, read-only Config browser, available from **Apps → Config Inspector**.
It uses the common tree view, Bluecurve stock icons, splitter and scrollbars.
The namespace model is independent of rendering and IPC.

Select a leaf to read its stored value. Up/Down and Home/End navigate; Left/Right
collapse and expand; clicking the disclosure box does the same. F5 or Refresh
reloads the snapshot while preserving selection and expansion. The divider is
draggable and both panes have scrollbars.

The executable manifest explicitly requests global non-secret Config **read**,
not write or administration. Every read is authorized by the service. There is
no filesystem, network, or implied superuser authority. Only Machine exists
today; profile/context management and schema-typed values remain future work.
The protocol's 1024-byte result limit is reported rather than silently
truncating. Failed refresh keeps the last complete tree marked stale.

Build/run the normal desktop in Nix:

```sh
nix develop -c make -C kernel run-desktop
```

This is a CuBit application, not a Linux preview. See
`tests/config-inspection/README.md` for hosted and native regression tests.
