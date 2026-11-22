#!/usr/bin/env python3
"""A project that lists CCL sources by name must list what they `with`.

Several native programs (devmgr, the setting-codec proof) compile a chosen
subset of userspace/ccl/src through an explicit Source_Files list rather
than the whole directory. When a shared unit such as ccl-setting_kinds.ads
gains a `with` (a new interface unit), every such list must gain that unit
too, or the program fails only at bind time in a full world build (the
User-Agent change in 63b38a425 broke devmgr this way).

The hazard is the shared catalogue units every setting codec depends on
(ccl-setting_kinds.ads and friends), which grow a `with` whenever a typed
setting is added. For every .gpr whose explicit list names one of them,
follow that unit's `with` clauses (transitively, within userspace/ccl/src)
and require each one to be named too. Units a project gets from a `with`ed
project are out of scope; this is about explicit lists only. Exit 1 listing
every gap.
"""
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
CCL_SRC = ROOT / "userspace" / "ccl" / "src"
WITH = re.compile(r"^\s*(?:limited\s+|private\s+)*with\s+([A-Za-z0-9_.,\s]+);", re.M | re.I)
SKIP_PARTS = {"build", "obj"}
# The shared units whose `with` lists grow with the settings catalogue.
SHARED = {"ccl-setting_kinds.ads", "ccl-setting_codecs.ads"}


def unit_file(unit: str) -> pathlib.Path:
    return CCL_SRC / (unit.lower().replace(".", "-") + ".ads")


def ccl_withs(spec: pathlib.Path) -> set[str]:
    text = spec.read_text(errors="replace")
    names = set()
    for group in WITH.findall(text):
        for name in group.split(","):
            name = name.strip()
            if name.lower().startswith("ccl.") and unit_file(name).exists():
                names.add(unit_file(name).name)
    return names


def closure(start: set[str]) -> set[str]:
    seen, work = set(), list(start)
    while work:
        name = work.pop()
        if name in seen or not (CCL_SRC / name).exists():
            continue
        seen.add(name)
        work.extend(ccl_withs(CCL_SRC / name))
    return seen


def main() -> int:
    gaps = []
    checked = 0
    for gpr in sorted(ROOT.rglob("*.gpr")):
        if SKIP_PARTS & set(gpr.relative_to(ROOT).parts):
            continue
        listed = set(re.findall(r'"(ccl-[A-Za-z0-9_\-]+\.ads)"', gpr.read_text(errors="replace")))
        listed = {n for n in listed if (CCL_SRC / n).exists()}
        if not listed:
            continue
        roots = listed & SHARED
        if not roots:
            continue
        checked += 1
        missing = sorted(closure(roots) - listed)
        for name in missing:
            gaps.append(f"{gpr.relative_to(ROOT)}: missing {name}")
    for gap in gaps:
        print(f"CCL-SOURCE-LISTS: {gap}")
    print(f"CCL-SOURCE-LISTS: {'FAIL' if gaps else 'PASS'} {checked} projects, {len(gaps)} gaps")
    return 1 if gaps else 0


if __name__ == "__main__":
    sys.exit(main())
