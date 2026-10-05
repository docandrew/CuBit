"""Check native instrumented Desktop evidence (not GPU completion truth)."""
import argparse
from pathlib import Path
import re
import source_retirement_fixture


def check(serial, mode):
    if mode == "source-retirement":
        source_retirement_fixture.check(serial)
        return
    if "FAIL completion fixture:" in serial:
        raise ValueError("Desktop fixture assertion failed")
    if mode in ("delayed", "retry"):
        deferred = re.findall(r"PASS begin fixture: deferred damage and ownership output=\s*(\d+)", serial)
        if sorted(deferred) != ["0", "1"]:
            raise ValueError("missing or duplicate deferred-start evidence for two outputs")
        held = re.findall(r"completion fixture: held output=\s*(\d+)", serial)
        completed = re.findall(
            r"PASS completion fixture: retained writer and fresh damage output=\s*(\d+) polls=\s*(\d+)", serial)
        fresh = re.findall(r"PASS completion fixture: fresh damage rendered output=\s*(\d+)", serial)
        if sorted(held) != ["0", "1"] or sorted(fresh) != ["0", "1"]:
            raise ValueError("missing or duplicate held/next-frame evidence for two outputs")
        if sorted(output for output, _ in completed) != ["0", "1"]:
            raise ValueError("missing or duplicate completion evidence")
        if any(int(polls) < 3 for _, polls in completed):
            raise ValueError("completion did not span enough event-loop polls")
        if "renderer completion uncertain" in serial or "injecting unsafe" in serial:
            raise ValueError("unexpected unsafe completion")
        if mode == "retry":
            for marker in ("restored damage and retained front", "recaptured frame published"):
                outputs = re.findall(r"PASS retry fixture: " + marker + r" output=\s*(\d+)", serial)
                if sorted(outputs) != ["0", "1"]:
                    raise ValueError("missing or duplicate retry evidence: " + marker)
    elif mode == "unsafe":
        match = re.search(r"completion fixture: injecting unsafe output=\s*[01]", serial)
        if not match:
            raise ValueError("no unsafe injection")
        if "desktop: renderer completion uncertain; writer retained" not in serial[match.end():]:
            raise ValueError("Desktop did not take its uncertain-completion exit")
        if "PASS completion fixture:" in serial or "desktop: asynchronous presentation active" in serial:
            raise ValueError("frame published despite first-frame unsafe completion")
    else:
        raise ValueError("unknown mode")


def self_test():
    source_retirement_fixture.self_test()
    good = "".join(
        f"PASS begin fixture: deferred damage and ownership output= {n}\n"
        f"completion fixture: held output= {n}\n"
        f"PASS completion fixture: retained writer and fresh damage output= {n} polls= 3\n"
        f"PASS completion fixture: fresh damage rendered output= {n}\n"
        for n in range(2))
    check(good, "delayed")
    retried = good + "".join(
        f"PASS retry fixture: {marker} output= {n}\n"
        for n in range(2) for marker in
        ("restored damage and retained front", "recaptured frame published"))
    check(retried, "retry")
    for bad_retry in (good, retried.replace("recaptured frame published", "missing"),
                      retried + "PASS retry fixture: restored damage and retained front output= 0\n"):
        try:
            check(bad_retry, "retry")
        except ValueError:
            continue
        raise AssertionError("bad retry trace accepted")
    bad = ["", good.replace("output= 1", "output= 0"), good.replace("polls= 3", "polls= 2"),
           good + "FAIL completion fixture: fresh damage lost", good + "renderer completion uncertain",
           good.replace("PASS begin fixture:", "missing begin fixture:")]
    unsafe = ("completion fixture: injecting unsafe output= 0\n"
              "desktop: renderer completion uncertain; writer retained\n")
    check(unsafe, "unsafe")
    for value, mode in [(text, "delayed") for text in bad] + [
            ("", "unsafe"), (unsafe.splitlines()[0], "unsafe"),
            (unsafe + "desktop: asynchronous presentation active", "unsafe")]:
        try:
            check(value, mode)
        except ValueError:
            continue
        raise AssertionError(f"negative control accepted: {mode}: {value!r}")
    print("PASS completion oracle: three positive cases, twelve negative controls")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("serial", nargs="?", type=Path)
    parser.add_argument("--mode", choices=("delayed", "unsafe", "retry", "source-retirement"), default="delayed")
    parser.add_argument("--self-test", action="store_true")
    args = parser.parse_args()
    if args.self_test:
        self_test()
    else:
        if args.serial is None:
            parser.error("serial log required")
        check(args.serial.read_text(errors="replace"), args.mode)
        print(f"PASS native Desktop completion observations: {args.mode}")
