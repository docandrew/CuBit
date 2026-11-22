"""Negative controls for the native retirement evidence checker."""
from pathlib import Path
import runpy

check = runpy.run_path(str(Path(__file__).with_name("check-storage-budget.py")))["check"]
allocations = "\n".join(
    f"desktop: pixel storage request= 3145728 charged= {n} limit= 134217728"
    for n in (3145728, 6291456, 9437184, 12582912, 15728640)
)
retirement = "desktop: renderer targets retired\ndesktop: output readers retired= 0\n"
releases = "\n".join(
    f"desktop: pixel storage released= 3145728 charged= {n}"
    for n in (12582912, 9437184, 6291456, 3145728, 0)
)
end = "\ndesktop: pixel teardown charged= 0\n"
valid = allocations + "\n" + retirement + releases + end
assert check(valid, False, retired=True) == (15728640, 134217728)
invalid = [
    valid.replace("desktop: renderer targets retired", "renderer unknown"),
    valid.replace("desktop: output readers retired= 0", "readers unknown"),
    allocations + "\n" + releases + retirement + end,
    valid.replace("released= 3145728 charged= 9437184", "released= 3145728 charged= 6291456"),
    valid.replace("released= 3145728 charged= 0", "released= 1 charged= 0"),
    valid.replace("desktop: pixel storage released= 3145728 charged= 0", ""),
    valid + "desktop: pixel storage released= 3145728 charged= 0\n",
    end + valid.removesuffix(end),
    valid + "desktop: output retirement uncertain\n",
]
for case in invalid:
    try:
        check(case, False, retired=True)
    except ValueError:
        pass
    else:
        raise AssertionError("accepted invalid retirement evidence")
print(f"storage evidence: PASS valid teardown and {len(invalid)} negative controls")
