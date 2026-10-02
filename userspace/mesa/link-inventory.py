"""Map GNU ld archive extraction evidence to Meson compilation records.

Audit aid, not a license classifier. Does not execute compilation commands.
Direct link inputs, included headers and generator inputs require separate review.
"""
import argparse
import hashlib
import json
from pathlib import Path


def extracted_members(text):
    lines = text.splitlines()
    if not lines or lines[0] != "Archive member included to satisfy reference by file (symbol)":
        raise ValueError("unrecognized GNU ld map header")
    members = []
    for line in lines[1:]:
        if line.startswith(("Merging object attributes", "Merging program properties",
                            "Discarded input sections")):
            break
        if not line or line[0].isspace():
            continue
        if not line.startswith("/"):
            raise ValueError(f"unsupported archive-member line: {line}")
        members.append(line)
    else:
        raise ValueError("truncated archive extraction table")
    if not members:
        raise ValueError("empty archive extraction table")
    return sorted(set(members))


def inventory(map_text, commands, source_root, build_root):
    outputs = {}
    for entry in commands:
        directory = Path(entry["directory"])
        if not directory.is_absolute():
            raise ValueError("compilation directory must be absolute")
        output = str((directory / entry["output"]).resolve())
        source = (directory / entry["file"]).resolve()
        if output in outputs and outputs[output] != source:
            raise ValueError(f"ambiguous compilation output: {output}")
        outputs[output] = source
    records = []
    for member in extracted_members(map_text):
        record = {"member": member}
        source = outputs.get(member)
        if source is None:
            if Path(member).is_relative_to(build_root):
                raise ValueError(f"Mesa archive member has no compilation record: {member}")
            record["kind"] = "unmapped-runtime-or-other"
        else:
            if source.is_relative_to(source_root):
                record["kind"] = "upstream-source"
            elif source.is_relative_to(build_root):
                record["kind"] = "generated-source"
            else:
                record["kind"] = "external-source"
            record["source"] = str(source)
            record["sha256"] = hashlib.sha256(source.read_bytes()).hexdigest()
        records.append(record)
    return {"scope": "extracted archive members, not retained-section or license audit",
            "members": records,
            "counts": {kind: sum(r["kind"] == kind for r in records)
                       for kind in sorted({r["kind"] for r in records})}}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("build", type=Path)
    parser.add_argument("source", type=Path)
    args = parser.parse_args()
    build = args.build.resolve()
    result = inventory((build / "native-mesa-cube.map").read_text(),
                       json.loads((build / "compile_commands.json").read_text()),
                       args.source.resolve(), build)
    print(json.dumps(result, indent=2, sort_keys=True))


if __name__ == "__main__":
    main()
