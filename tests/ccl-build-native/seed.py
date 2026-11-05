#!/usr/bin/env python3
"""Seed a Storehouse with one step's result from another store, by identity.

    seed.py STORE LOG LABEL OUT

STORE is a Linux ccl-build store, LOG the output of the build that ran the
step there ("ran  <12 hex digits>  <label>"), LABEL the step's label. Copies
the step's action record (actions/<step identity>) and every output entry it
names (files/ or executables/<identity>) into OUT, laid out as a store.

This is binary substitution by identity (docs/ccl-build.md, "What we keep
from Nix"), done here by the test's image builder: ccl-build on CuBit finds
the step answered, because its step identity is the same on both hosts (the
same tool, arguments and input contents). Each entry is re-hashed by
ccl-build on first use, so a seed that does not match its name is refused.
"""
import hashlib
import os
import shutil
import sys

ACTION_TAG = "cubit.action"
ACTION_FIELDS = 4
OUTPUT_FIELDS = 3
SHORT_IDENTITY = 12
KIND_DIRECTORIES = {0: "files", 1: "executables"}  # Storehouse.Entry_Kind'Pos


class Reader:
    """Canonical CBOR (RFC 8949 4.2.1): unsigned, bytes, text, arrays."""

    def __init__(self, data):
        self.data = data
        self.at = 0

    def head(self):
        initial = self.data[self.at]
        self.at += 1
        major, info = initial >> 5, initial & 0x1F
        if info < 24:
            return major, info
        width = {24: 1, 25: 2, 26: 4, 27: 8}[info]
        value = int.from_bytes(self.data[self.at:self.at + width], "big")
        self.at += width
        return major, value

    def expect(self, major):
        got, value = self.head()
        if got != major:
            raise ValueError(f"major type {got}, expected {major}")
        return value

    def unsigned(self):
        return self.expect(0)

    def raw(self, major):
        length = self.expect(major)
        piece = self.data[self.at:self.at + length]
        self.at += length
        return piece

    def text(self):
        return self.raw(3).decode()

    def digest(self):
        return self.raw(2).hex()

    def array(self):
        return self.expect(4)


def outputs_of(record):
    r = Reader(record)
    if r.array() != ACTION_FIELDS or r.text() != ACTION_TAG:
        raise ValueError("not an action record")
    r.unsigned()  # version
    step = r.digest()
    result = []
    for _ in range(r.array()):
        if r.array() != OUTPUT_FIELDS:
            raise ValueError("not an output")
        path, kind, identity = r.text(), r.unsigned(), r.digest()
        result.append((path, KIND_DIRECTORIES[kind], identity))
    return step, result


def main():
    store, log, label, out = sys.argv[1:5]
    prefix = None
    with open(log) as lines:
        for line in lines:
            fields = line.rstrip("\n").split("  ", 2)
            if len(fields) == 3 and fields[0] == "ran" and fields[2] == label:
                prefix = fields[1]
    if prefix is None or len(prefix) != SHORT_IDENTITY:
        sys.exit(f"seed: {log} has no run of {label!r}")
    actions = [n for n in os.listdir(os.path.join(store, "actions")) if n.startswith(prefix)]
    if len(actions) != 1:
        sys.exit(f"seed: {len(actions)} action records start with {prefix}")
    with open(os.path.join(store, "actions", actions[0]), "rb") as f:
        record = f.read()
    step, outputs = outputs_of(record)
    if step != actions[0]:
        sys.exit("seed: the action record names another step")
    os.makedirs(os.path.join(out, "actions"), exist_ok=True)
    with open(os.path.join(out, "actions", step), "wb") as f:
        f.write(record)
    for path, directory, identity in outputs:
        source = os.path.join(store, directory, identity)
        with open(source, "rb") as f:
            if hashlib.sha256(f.read()).hexdigest() != identity:
                sys.exit(f"seed: {source} does not match its name")
        os.makedirs(os.path.join(out, directory), exist_ok=True)
        shutil.copyfile(source, os.path.join(out, directory, identity))
        print(f"seed: {path} {directory}/{identity}")
    print(f"seed: step {step} ({label})")


if __name__ == "__main__":
    main()
