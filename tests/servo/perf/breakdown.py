#!/usr/bin/env python3
"""Parse and summarize WEB-001 breakdown logs (Penny's bench flag).

The same lines come from CuBit's serial log and hosted cubitshell's stderr:
PENNY-LOAD (load stages), CUBITSHELL-PERF (page-reported timings),
PENNY-BENCH (scripted input samples), PENNY-PROFILE (Servo's time profile,
including the Servo fork's per-stage timing categories).

    breakdown.py <results-directory>   # summarize every */result.json
"""
import json
import pathlib
import re
import statistics
import sys

# Servo time-profile categories, grouped into the breakdown's stages.
STAGES = {
    "style": ["LayoutStyle"],
    "box tree": ["LayoutBoxTree"],
    "layout": ["LayoutFragmentTree"],
    "stacking": ["LayoutStackingContext"],
    "display list": ["LayoutDisplayList"],
    "reflow (all)": ["Layout"],
    "script eval": ["ScriptEvaluate"],
    "HTML parse": ["ScriptParseHTML"],
    "WR scene build": ["WrSceneBuild"],
    "WR frame build": ["WrFrameBuild"],
    "WR upload": ["WrUpload"],
    "paint update": ["PaintUpdate"],
    "SWGL draw": ["PaintDraw"],
}
FIRST_PAINT_CATEGORIES = ("TimeToFirstPaint", "TimeToFirstContentfulPaint")
PERCENTILE_90 = 0.9
MICROSECONDS_PER_MILLISECOND = 1000.0


def parse(text):
    result = {"load": {}, "nav": {}, "js": {}, "bench": {}, "timeouts": {}, "profile": {},
              "viewport": None, "done": "PENNY-BENCH: done" in text}
    for line in text.splitlines():
        line = line.rstrip("\r")
        if (m := re.search(r"PENNY-LOAD: stage=(\w+) elapsed_ms=(\d+)", line)):
            result["load"].setdefault(m.group(1), int(m.group(2)))
        elif (m := re.search(r"CUBITSHELL-PERF: CuBitBrowserPerfNav (.*)", line)):
            result["nav"] = {k: int(v) for k, v in re.findall(r"(\w+)=(-?\d+)", m.group(1))}
        elif (m := re.search(r"CUBITSHELL-PERF: CuBitBrowserPerfJS:([\w-]+),(\d+),([\d.]+),", line)):
            result["js"].setdefault(m.group(1), []).append(float(m.group(3)))
        elif (m := re.search(r"PENNY-BENCH: viewport=(\d+x\d+)", line)):
            result["viewport"] = m.group(1)
        elif (m := re.search(r"PENNY-BENCH: kind=([\w-]+) step=(\d+) timeout", line)):
            result["timeouts"][m.group(1)] = result["timeouts"].get(m.group(1), 0) + 1
        elif (m := re.search(r"PENNY-BENCH: kind=([\w-]+) step=\d+ (.*)", line)):
            sample = {k: int(v) for k, v in re.findall(r"(\w+)_us=(\d+)", m.group(2))}
            result["bench"].setdefault(m.group(1), []).append(sample)
        elif (m := re.search(r"PENNY-PROFILE: (.*)", line)):
            fields = m.group(1).split("\t")
            if len(fields) >= 8 and not fields[0].startswith("_"):
                category = fields[0].strip()
                try:
                    mean, median, minimum, maximum = (float(x) for x in fields[-5:-1])
                    events = int(fields[-1])
                except ValueError:
                    continue
                entry = result["profile"].setdefault(
                    category, {"total_ms": 0.0, "events": 0, "max_ms": 0.0, "medians": []})
                entry["total_ms"] += mean * events
                entry["events"] += events
                entry["max_ms"] = max(entry["max_ms"], maximum)
                entry["medians"].append(median)
    return result


def percentile(values, fraction):
    ordered = sorted(values)
    return ordered[min(len(ordered) - 1, int(fraction * len(ordered)))]


def run_metrics(result):
    """One run's numbers, all in milliseconds."""
    metrics = {}
    load = result["load"]
    for stage in ("FirstPaint", "HeadParsed", "Complete"):
        if stage in load:
            metrics["load " + stage] = load[stage]
    profile = result["profile"]
    for category in FIRST_PAINT_CATEGORIES:
        if category in profile:
            metrics[category] = profile[category]["max_ms"]
    nav = result["nav"]
    if "re" in nav:
        metrics["network responseEnd"] = nav["re"]
        metrics["network responseStart"] = nav.get("rs", 0)
    for kind in ("key", "scroll", "load-paint"):
        samples = result["bench"].get(kind, [])
        if not samples:
            continue
        for part in ("total", "ready", "paint", "present"):
            values = [s[part] / MICROSECONDS_PER_MILLISECOND for s in samples]
            metrics[f"{kind} {part} median"] = statistics.median(values)
            if part == "total":
                metrics[f"{kind} {part} p90"] = percentile(values, PERCENTILE_90)
            if kind == "load-paint":
                metrics[f"{kind} {part} sum"] = sum(values)
    for stage, categories in STAGES.items():
        entries = [profile[c] for c in categories if c in profile]
        if entries:
            metrics["stage " + stage + " total"] = sum(e["total_ms"] for e in entries)
            metrics["stage " + stage + " max"] = max(e["max_ms"] for e in entries)
            metrics["stage " + stage + " events"] = sum(e["events"] for e in entries)
    script = [e for c, e in profile.items() if c.startswith("Script")]
    if script:
        metrics["stage script (all) total"] = sum(e["total_ms"] for e in script)
    for workload, samples in result["js"].items():
        metrics["js " + workload + " median"] = statistics.median(samples)
    for kind, count in result["timeouts"].items():
        metrics["timeouts " + kind] = count
    return metrics


def summarize(directory):
    runs = {}
    for path in sorted(pathlib.Path(directory).glob("*/result.json")):
        record = json.loads(path.read_text())
        runs.setdefault((record["page"], record["platform"]), []).append(run_metrics(record["parsed"]))
    summary = {}
    for key, metric_list in runs.items():
        names = sorted({name for metrics in metric_list for name in metrics})
        summary[key] = {}
        for name in names:
            values = [m[name] for m in metric_list if name in m]
            summary[key][name] = {"median": statistics.median(values), "min": min(values),
                                  "max": max(values), "runs": len(values)}
    return summary


# The breakdown table's rows, in order: (label, metric name).
TABLE_ROWS = [
    ("first paint (embedder)", "load FirstPaint"),
    ("HTML head parsed", "load HeadParsed"),
    ("load complete", "load Complete"),
    ("network: responseEnd", "network responseEnd"),
    ("scroll frame, median", "scroll total median"),
    ("scroll frame, p90", "scroll total p90"),
    ("  scroll: input to frame ready", "scroll ready median"),
    ("  scroll: paint (WR + SWGL)", "scroll paint median"),
    ("  scroll: present", "scroll present median"),
    ("key to paint, median", "key total median"),
    ("key to paint, p90", "key total p90"),
    ("  key: input to frame ready", "key ready median"),
    ("  key: paint (WR + SWGL)", "key paint median"),
    ("  key: present", "key present median"),
    ("stage total: style", "stage style total"),
    ("stage total: box tree", "stage box tree total"),
    ("stage total: fragment layout", "stage layout total"),
    ("stage total: stacking contexts", "stage stacking total"),
    ("stage total: display list", "stage display list total"),
    ("stage total: script (all)", "stage script (all) total"),
    ("stage total: HTML parse", "stage HTML parse total"),
    ("stage total: WR scene build", "stage WR scene build total"),
    ("stage total: WR frame build", "stage WR frame build total"),
    ("stage total: WR upload", "stage WR upload total"),
    ("stage total: paint update", "stage paint update total"),
    ("stage total: SWGL draw", "stage SWGL draw total"),
    ("paint calls (events)", "stage SWGL draw events"),
]


def cell(entry):
    if entry is None:
        return "-"
    return f"{entry['median']:.1f} [{entry['min']:.1f}-{entry['max']:.1f}]"


def table(summary, platforms=("cubit", "hosted")):
    """Markdown: per page, median [min-max] over runs, and the CuBit/Linux ratio."""
    lines = []
    pages = sorted({page for page, _ in summary})
    for page in pages:
        cubit, hosted = (summary.get((page, name), {}) for name in platforms)
        lines.append(f"\n### {page} (ms; median [min-max] of runs)\n")
        lines.append("| metric | CuBit KVM | Linux | CuBit/Linux |")
        lines.append("| --- | ---: | ---: | ---: |")
        rows = list(TABLE_ROWS)
        rows += [(name, name) for name in sorted(set(cubit) | set(hosted)) if name.startswith("js ")]
        for label, name in rows:
            a, b = cubit.get(name), hosted.get(name)
            if a is None and b is None:
                continue
            ratio = f"{a['median'] / b['median']:.1f}x" if a and b and b["median"] > 0 else "-"
            lines.append(f"| {label} | {cell(a)} | {cell(b)} | {ratio} |")
    return "\n".join(lines)


def main():
    summary = summarize(sys.argv[1])
    if len(sys.argv) > 2 and sys.argv[2] == "--markdown":
        print(table(summary))
        return
    out = {f"{page}/{platform}": values for (page, platform), values in summary.items()}
    print(json.dumps(out, indent=1))


if __name__ == "__main__":
    main()
