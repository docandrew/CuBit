#!/usr/bin/env python3
"""Inventory the resolved host graph; NOT a license or native-code certification."""
import json
import sys

metadata = json.load(sys.stdin)
nodes = {node["id"]: node for node in metadata["resolve"]["nodes"]}
packages = sorted(
    (package for package in metadata["packages"] if package["id"] in nodes),
    key=lambda package: (package["name"], package["version"]),
)
print("# Resolved Linux-hosted dependency inventory\n")
print(f"{len(packages)} packages including the experiment. Declared licenses only; review still required.\n")
for package in packages:
    features = ", ".join(nodes[package["id"]]["features"]) or "none"
    print(f"- {package['name']} {package['version']}: {package.get('license') or 'NOT DECLARED'}; features: {features}")

by_name = {package["name"]: package for package in packages}
assert not ({"mimalloc", "libmimalloc-sys", "libsqlite3-sys", "simsimd", "ring", "aws-lc-sys"} & by_name.keys()), "unexpected native-code dependency"
assert "pure-rust" in nodes[by_name["aegis"]["id"]]["features"], "C AEGIS backend enabled"
print("\nSelected-feature checks passed. Linux std/libc are still host dependencies.")
print("The cc build helper remains in AEGIS's graph; its build script returns before C compilation with pure-rust enabled.")
