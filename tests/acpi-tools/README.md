# Declared source pins

`pins.py` uses Python 3's standard library. Nominate catalogs explicitly:

```sh
python3 -I tests/acpi-tools/pins.py --check tests/acpi-host-bridge-watch/sources.json
python3 -I tests/acpi-tools/pins.py --write tests/acpi-bootstrap-driver/source-manifest.json tests/acpi-bootstrap-driver/closure.json
python3 -I tests/acpi-tools/pins.py --write --remove /inputs=shared/removed.ads --add /inputs=shared/replacement.ads tests/acpi-example/sources.json
python3 -I tests/acpi-tools/test_pins.py
```

Default root is the repository containing this script; `--root DIR` selects
an explicit private snapshot. `--check` never writes and exits 1 when a
nominated manifest would change. Missing files, invalid paths/digests, cycles,
duplicate JSON keys, symbolic links and invalid edits exit 2. `--write`
plans and validates all nominated files before writing any of them. Each
replacement is atomic; an I/O failure during publication can leave a partial
multi-file update. Coordinate shared writes under the repository's build lock.

Supported file references are recursive `{path, sha256}` and
`{repository_path, sha256}` rows (extra classification fields are preserved),
path-to-SHA-256 maps, and paired fields such as `engine`/`engine_sha256` or
`manifest_path`/`manifest_sha256`. Thus source-manifest maps and per-main
closure source rows use the same command. A nominated manifest depending on
another nominated manifest hashes its prospective bytes, regardless of
argument order. Self hashes and dependency cycles are refused. No-op updates
preserve exact bytes and file modes. Changed JSON uses two spaces, original
key order and a final newline; repeated runs are deterministic.

Paths normally resolve from the repository root. Top-level `helpers` and
`assets` and `fixtures` path rows resolve from the catalog directory; paths beginning with `tests/`,
`userspace/`, `shared/`, `tools/` or `docs/` still resolve from the root.
`{name, sha256}` fixture rows resolve under the catalog's `fixtures/`.
`--base /JSON/pointer=repository/directory` explicitly changes the base for
a subtree (longest matching pointer wins). External tool paths require
`--allow-external`. The script rejects symlink files and parents, including
for external tools; nominate their actual regular-file paths.

`--add` and `--remove` use JSON pointers to a row list or path-to-hash map,
and require one manifest. List additions create `path`/`sha256` rows; supply
any suite-required `kind` or other classification explicitly before refresh.
No source discovery, missing-file deletion or dependency inference occurs.
Directory inventories and semantic selection lists remain reviewed inputs.

Expected transcript digests, inline AML/wrapper hashes, tool-name-only
digests without a file reference, counts, verdicts and historical evidence
are preserved. This utility regenerates declared file input pins only. It
does not regenerate fixtures, discover actual compiler dependencies or make
stale evidence current. Review source changes, run the affected hosted suite,
and attach its fresh receipt before presenting new pins as tested.
