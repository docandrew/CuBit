#!/usr/bin/env python3
"""WEB-001: upstream WPT pass rates from wpt.fyi, the engine baseline.

    wpt_baseline.py --out DIR

Servo's nightly run nearest after the pinned commit (UPSTREAM_REVISION's
date, PINNED_DATE), plus the nearest aligned run (every product on one WPT
revision): Servo, WPE WebKit, Safari (WebKit), Chrome, Firefox. Subtest
pass rates for the directories the survey cares about (DIRECTORIES) and
overall. Writes DIR/wpt-baseline.md and .json.
"""
import argparse
import collections
import gzip
import json
import pathlib
import urllib.request

PINNED_DATE = "2026-09-25T12:08:04Z"
SEARCH_UNTIL = "2026-10-02T00:00:00Z"
DIRECTORIES = ("dom", "html", "css", "fetch", "url", "encoding")
PRODUCTS = ("servo", "wpewebkit", "safari", "chrome", "firefox")
API = "https://wpt.fyi/api/runs"
TIMEOUT_SECONDS = 120


def get_json(url):
    with urllib.request.urlopen(url, timeout=TIMEOUT_SECONDS) as response:
        data = response.read()
    if data[:2] == b"\x1f\x8b":
        data = gzip.decompress(data)
    return json.loads(data)


# summary_v2: {"s": status, "c": [subtests passed, subtests]}. Reftests and
# crashtests have no subtests (c = [0, 0]); their status is the result.
PASS, HARNESS_OK = "P", "O"


def rates(summary):
    """Per top-level directory (and all): subtests passed (a reftest counts
    as one subtest), and tests fully passing."""
    passed, total = collections.Counter(), collections.Counter()
    tests_passed, tests = collections.Counter(), collections.Counter()
    for path, result in summary.items():
        status = result.get("s")
        counts = result.get("c") or [0, 0]
        if counts[1] == 0:
            counts = [int(status == PASS), 1]
            whole = status == PASS
        else:
            whole = counts[0] == counts[1] and status in (PASS, HARNESS_OK)
        top = path.strip("/").split("/")[0]
        for key in (top, "all"):
            passed[key] += counts[0]
            total[key] += counts[1]
            tests[key] += 1
            tests_passed[key] += whole
    return {key: {"passed": passed[key], "total": total[key],
                  "rate": passed[key] / total[key] if total[key] else None,
                  "tests": tests[key], "tests_passed": tests_passed[key],
                  "test_rate": tests_passed[key] / tests[key] if tests[key] else None}
            for key in (*DIRECTORIES, "all")}


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--out", type=pathlib.Path, required=True)
    options = parser.parse_args()
    options.out.mkdir(parents=True, exist_ok=True)
    servo = get_json(f"{API}?product=servo&label=master&max-count=20&from={PINNED_DATE}&to={SEARCH_UNTIL}")
    nearest = min(servo, key=lambda r: r["time_start"])
    aligned = get_json(f"{API}?products={','.join(PRODUCTS)}&aligned&label=master&max-count=1"
                       f"&from={PINNED_DATE}&to={SEARCH_UNTIL}")
    if not any(r["browser_name"] == "wpewebkit" for r in aligned):
        aligned = get_json(f"{API}?products={','.join(p for p in PRODUCTS if p != 'wpewebkit')}"
                           f"&aligned&label=master&max-count=1&from={PINNED_DATE}&to={SEARCH_UNTIL}")
    runs = [("Servo nightly nearest the pinned commit", nearest)] + [
        (f"aligned {r['revision']}", r) for r in aligned]
    results = []
    for label, run in runs:
        summary = get_json(run["results_url"])
        results.append({"label": label, "product": run["browser_name"], "version": run["browser_version"],
                        "wpt_revision": run["revision"], "date": run["time_start"][:10],
                        "results_url": run["results_url"], "rates": rates(summary)})
    (options.out / "wpt-baseline.json").write_text(json.dumps(results, indent=1))
    header = "| Run | Product | Version | WPT | " + " | ".join(DIRECTORIES) + " | all |"
    lines = ["# WPT baseline (wpt.fyi)", "",
             "Each cell: subtests passed / tests with every subtest passing.", "",
             f"Pinned Servo commit dated {PINNED_DATE}.", "", header,
             "|" + "---|" * (len(DIRECTORIES) + 5)]
    for result in results:
        cells = [(f"{result['rates'][d]['rate']:.1%} / {result['rates'][d]['test_rate']:.1%}"
                  if result["rates"][d]["rate"] is not None else "–") for d in (*DIRECTORIES, "all")]
        lines.append(f"| {result['label']} ({result['date']}) | {result['product']} | {result['version']} | "
                     f"{result['wpt_revision'][:10]} | " + " | ".join(cells) + " |")
    (options.out / "wpt-baseline.md").write_text("\n".join(lines) + "\n")
    print("\n".join(lines))


if __name__ == "__main__":
    main()
