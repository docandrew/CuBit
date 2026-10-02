"""Read-only packed ADS layout audit against a supplied upstream fwif header.

Usage: nix develop -c python3 tests/intel-gpu/check-ads-abi.py HEADER
Accepts only the simple declarations used by the pinned Linux v6.16 structs;
unknown syntax fails rather than silently inventing an ABI. No network fetch.
"""
import pathlib
import re
import sys


def layout(text, name, sizes, constants):
    match = re.search(r"struct\s+" + re.escape(name) + r"\s*\{(.*?)\}\s*__packed\s*;", text, re.S)
    if not match:
        raise ValueError(f"missing packed struct {name}")
    body = re.sub(r"/\*.*?\*/", "", match[1], flags=re.S)
    fields, cursor = {}, 0
    for declaration in body.split(";"):
        declaration = declaration.strip()
        if not declaration:
            continue
        field = re.fullmatch(r"(u8|u16|u32|struct\s+\w+)\s+(\w+)\s*((?:\[\w+\])*)", declaration)
        if not field:
            raise ValueError(f"unsupported declaration: {declaration}")
        kind, member, dimensions = field.groups()
        size = sizes[kind]
        for dimension in re.findall(r"\[(\w+)\]", dimensions):
            size *= int(dimension) if dimension.isdecimal() else constants[dimension]
        if member in fields:
            raise ValueError(f"duplicate field {member}")
        fields[member] = (cursor, size)
        cursor += size
    return fields, cursor


def main(path):
    text = pathlib.Path(path).read_text()
    constants = {}
    for name in ("GUC_MAX_ENGINE_CLASSES", "GUC_MAX_INSTANCES_PER_CLASS",
                 "GUC_GENERIC_GT_SYSINFO_MAX", "GUC_CAPTURE_LIST_INDEX_MAX"):
        match = re.search(r"(?:#define\s+" + name + r"\s+|\b" + name + r"\s*=\s*)(\d+)\b", text)
        if not match:
            raise ValueError(f"missing constant {name}")
        constants[name] = int(match[1])
    sizes = {"u8": 1, "u16": 2, "u32": 4}
    _, sizes["struct guc_mmio_reg_set"] = layout(text, "guc_mmio_reg_set", sizes, constants)
    fields, total = layout(text, "guc_ads", sizes, constants)
    expected = {
        "reg_state_list": (0, 4096), "reserved0": (4096, 4),
        "scheduler_policies": (4100, 4), "gt_system_info": (4104, 4),
        "reserved1": (4108, 4), "control_data": (4112, 4),
        "golden_context_lrca": (4116, 64), "eng_state_size": (4180, 64),
        "private_data": (4244, 4), "reserved2": (4248, 4),
        "capture_instance": (4252, 128), "capture_class": (4380, 128),
        "capture_global": (4508, 8), "wa_klv_addr_lo": (4516, 4),
        "wa_klv_addr_hi": (4520, 4), "wa_klv_size": (4524, 4),
        "reserved": (4528, 44),
    }
    if fields != expected or total != 4572:
        raise ValueError(f"ADS ABI mismatch: {fields}, size={total}")
    _, system_size = layout(text, "guc_gt_system_info", sizes, constants)
    if system_size != 640:
        raise ValueError(f"system-info size changed: {system_size}")
    for member, (offset, size) in fields.items():
        print(f"{member}: offset={offset} bytes={size}")
    # Ensure unexpected syntax cannot be silently dropped by the parser.
    try:
        layout(text.replace("u32 reserved0;", "u64 reserved0;"), "guc_ads", sizes, constants)
    except ValueError:
        pass
    else:
        raise ValueError("parser accepted unsupported field type")
    print("ADS ABI PASS: 4572-byte header, 264-byte capture block, 640-byte system info")


if __name__ == "__main__":
    if len(sys.argv) != 2:
        raise SystemExit(__doc__)
    main(sys.argv[1])
