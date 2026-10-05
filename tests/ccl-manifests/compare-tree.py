#!/usr/bin/env python3
"""Compile every manifest.ccl in the tree with two manifest tools and require
identical results: the assembled sections, the exit status and the generated
Ada bindings, for each service catalog (docs/ccl-typed-manifests.md).

    compare-tree.py BASELINE_TOOL CANDIDATE_TOOL [--convert CONVERTER]

With --convert, the candidate compiles CONVERTER's rewrite of each manifest
(the typed form) instead of the manifest itself. Copies under
.build-workspaces/ and build directories are skipped.
"""
import argparse
import pathlib
import subprocess
import sys
import tempfile

ROOT = pathlib.Path(__file__).resolve().parents[2]
CATALOGS = sorted((ROOT / "userspace/ccl/catalogs").glob("*.ccl"))
SKIPPED_PARTS = {".build-workspaces", "build", "build-tmp", "node_modules", ".git"}


def manifests():
    for path in sorted(ROOT.rglob("manifest.ccl")):
        if not SKIPPED_PARTS.intersection(path.relative_to(ROOT).parts):
            yield path


def compile_with(tool, catalog, manifest, scratch):
    bindings = scratch / "bindings.ads"
    bindings.unlink(missing_ok=True)
    run = subprocess.run([str(tool), str(catalog), str(manifest), "--ada-output", str(bindings)],
                         capture_output=True, text=True)
    generated = bindings.read_text() if bindings.exists() else ""
    return run.returncode, run.stdout, generated


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("baseline")
    parser.add_argument("candidate")
    parser.add_argument("--convert")
    options = parser.parse_args()
    compared = accepted = 0
    mismatches = []
    with tempfile.TemporaryDirectory() as directory:
        scratch = pathlib.Path(directory)
        for manifest in manifests():
            source = manifest
            if options.convert:
                converted = subprocess.run([options.convert, str(manifest)], capture_output=True, text=True)
                if converted.returncode != 0:
                    mismatches.append((manifest, "-", "converter failed: " + converted.stderr.strip()))
                    continue
                source = scratch / "typed-manifest.ccl"
                source.write_text(converted.stdout)
            for catalog in CATALOGS:
                old = compile_with(options.baseline, catalog, manifest, scratch)
                new = compile_with(options.candidate, catalog, source, scratch)
                compared += 1
                accepted += old[0] == 0
                if old != new:
                    what = ("status %d vs %d" % (old[0], new[0]) if old[0] != new[0]
                            else "sections differ" if old[1] != new[1] else "bindings differ")
                    mismatches.append((manifest, catalog.name, what))
    for manifest, catalog, what in mismatches:
        print("MISMATCH %s [%s]: %s" % (manifest.relative_to(ROOT), catalog, what))
    print("%d manifest/catalog pairs compared (%d accepted by the baseline), %d mismatches"
          % (compared, accepted, len(mismatches)))
    return 1 if mismatches else 0


if __name__ == "__main__":
    sys.exit(main())
