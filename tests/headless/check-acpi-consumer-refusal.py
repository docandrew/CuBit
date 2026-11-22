#!/usr/bin/env python3
"""Extra evidence for the ordinary-family-refusal real ACPI shutdown run."""
import re
import sys
from pathlib import Path


def check(text):
    def need(ok, why):
        if not ok:
            raise ValueError(why)
    refusals = list(re.finditer(r"acpi: sample refusal family=BATTERY isolated=TRUE runtime=IDLE\b", text))
    need(len(refusals) == 1, "missing or repeated isolated battery refusal")
    need(re.search(r"acpi: battery guard=(INVENTORY_LIMIT|BUDGET_EXHAUSTED)\b", text),
         "missing bounded complete-inventory refusal guard")
    for marker in ["acpi: lid OPEN", "acpi: AC ONLINE", "power: prepare to sleep PREPARED"]:
        need(marker in text, "missing continued service evidence: " + marker)
    sci = text.find("acpi: SCI 1 fixed=[PWRBTN ]")
    prepare = text.find("power: prepare to sleep PREPARED")
    need(refusals[0].start() < sci < prepare, "power SCI and S5 preparation did not follow family isolation")
    for marker in ["acpi: stage consumer fail", "runtime-quarantine", "acpi: stage serve fail"]:
        need(marker not in text, "ordinary refusal stopped the shared service: " + marker)
    return "isolated battery refusal kept lid/AC facts and subsequent SCI/S5 preparation"


def main():
    try:
        text = Path(sys.argv[1]).read_text(errors="replace").replace("\r", "")
        print("PASS: " + check(text))
        return 0
    except (IndexError, OSError, ValueError) as error:
        print("acpi-consumer-refusal: " + str(error), file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
