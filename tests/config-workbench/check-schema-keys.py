#!/usr/bin/env python3
"""Pin bootstrap descriptors to the checked-in schema declarations."""
import hashlib
from pathlib import Path
import re

root = Path(__file__).resolve().parents[2]
body = (root / "userspace/ccl/native/ccl_native_execution.adb").read_text()
for constant, filename in (
    ("Value_Key", "value"), ("Read_Key", "read"), ("Interface_Key", "collection")
):
    block = re.search(rf"{constant}\s*:.*?:=\s*\[(.*?)\];", body, re.S)
    assert block, constant
    words = re.findall(r"16#([0-9a-fA-F_]+)#", block[1])
    encoded = b"".join(int(word.replace("_", ""), 16).to_bytes(8, "big") for word in words)
    definition = root / f"userspace/ccl/interfaces/workbench-config-{filename}.schema"
    assert encoded == hashlib.sha256(definition.read_bytes()).digest(), constant
print("Config Workbench bootstrap schema keys: PASS")
