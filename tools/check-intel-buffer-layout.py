#!/usr/bin/env python3
"""Build-time check of the native Intel arena against the actual kernel limit."""
from pathlib import Path
import re


def number(source, pattern):
    source = re.sub(r"--[^\n]*", "", source)
    match = re.search(pattern, source, re.IGNORECASE)
    if not match:
        raise ValueError("unrecognized constant definition; update this checker")
    return int(match[1].replace("_", ""))


def validate(order, maximum, capacity, blocks=1):
    if order > maximum:
        raise ValueError(f"Intel arena order {order} exceeds kernel maximum {maximum}")
    if blocks <= 0 or capacity != blocks * 4096 * (1 << order):
        raise ValueError("Intel arena capacity does not match its DMA extents")


def require_definition(source, pattern):
    if not re.search(pattern, re.sub(r"--[^\n]*", "", source), re.IGNORECASE):
        raise ValueError("unrecognized derived layout definition; update this checker")


if __name__ == "__main__":
    root = Path(__file__).resolve().parents[1]
    kernel = (root / "kernel/src/config.ads").read_text()
    driver = (root / "userspace/services/intel-gpu/intel_gpu_buffer_backing.ads").read_text()
    maximum = number(kernel, r"MAX_BUDDY_ORDER\s*:\s*constant\s*:=\s*([\d_]+)\s*;")
    extents = (root / "userspace/services/intel-gpu/intel_gpu_physical_extents.ads").read_text()
    order = number(extents, r"Allocation_Order\s*:\s*constant\s*:=\s*([\d_]+)\s*;")
    blocks = 1 + number(extents, r"subtype\s+Block_Index\s+is\s+Natural\s+range\s+0\s*\.\.\s*([\d_]+)\s*;")
    require_definition(driver, r"Capacity\s*:\s*constant\s+Unsigned_64\s*:=\s*Intel_GPU_Physical_Extents\.Capacity\s*;")
    require_definition(extents, r"Block_Bytes\s*:\s*constant\s+Unsigned_64\s*:=\s*4096\s*\*\s*2\s*\*\*\s*Allocation_Order\s*;")
    require_definition(extents, r"type\s+Addresses\s+is\s+array\s*\(Block_Index\)\s+of\s+Unsigned_64\s*;")
    require_definition(extents, r"Capacity\s*:\s*constant\s+Unsigned_64\s*:=\s*Unsigned_64\s*\(Addresses'Length\)\s*\*\s*Block_Bytes\s*;")
    capacity = blocks * 4096 * (1 << order)
    # Regression: the formerly accepted driver-local layout must fail here.
    try:
        validate(13, 12, 32 * 1024 * 1024)
    except ValueError:
        pass
    else:
        raise AssertionError("unsupported-order regression was not rejected")
    validate(order, maximum, capacity, blocks)
    print(f"Intel DMA layout PASS: {blocks} x order {order}, {capacity // (1024 * 1024)} MiB, kernel maximum {maximum}")
