#!/usr/bin/env python3
"""PERF-010: the build benchmark's breakdown, from bench_build.py's results
(CuBit) and bench_linux.py's (the same builds by Linux's ccl-build).

Usage: bench_report.py CUBIT.json [LINUX.json]

For each app and kind of build (cold, warm, edit): the median of each
measure over the runs (each kind runs at least three times), the
categories of ccl-build's time (Build_Timing) ranked, and per step the
CuBit and Linux medians side by side. Then the hello cold build's time
split three ways:
  fixed per-process: Spawn (CuBit: procmgr reads the image, the kernel
      makes the process) plus each tool's run beyond Linux's run of the
      same tool (loading the driver's own children: gnat1, as, ld;
      page faults; zeroing; the tool's own file I/O over IPC);
  per-file: placing inputs in step directories, removing them again,
      hashing (sources, outputs, re-verifying entries), Storehouse writes,
      claims and lookups;
  compile work: the tools' run time on Linux (the work itself).
"""
import json
import statistics
import sys

MICROSECONDS = 1e6
PER_FILE = ("place_inputs", "release", "source_hash", "output_hash", "verify_hash",
            "store_write", "claim", "recall", "export", "store_open", "import")
KINDS = ("cold", "warm", "edit")


def median(values):
    values = [v for v in values if v is not None]
    return statistics.median(values) if values else None


def groups(report):
    found = {}
    for run in report.get("runs", []):
        if run.get("ok"):
            found.setdefault((run["app"], run["kind"]), []).append(run)
    return found


def category_medians(runs):
    names = sorted({c for r in runs for c in r["build"]["us"]})
    return {c: median([r["build"]["us"].get(c, 0) for r in runs]) for c in names}


def count_medians(runs):
    names = sorted({c for r in runs for c in r["build"]["counts"]})
    return {c: median([r["build"]["counts"].get(c, 0) for r in runs]) for c in names}


def step_medians(runs):
    """label -> {"wall": us, category: us} medians over the runs."""
    by_label = {}
    for r in runs:
        for s in r["steps"]:
            by_label.setdefault(s["label"], []).append(s)
    result = {}
    for label, steps in by_label.items():
        names = {c for s in steps for c in s["us"]}
        item = {"wall": median([s["wall_us"] for s in steps]), "ran": steps[0]["ran"]}
        for c in names:
            item[c] = median([s["us"].get(c, 0) for s in steps])
        result[label] = item
    return result


def ms(us):
    return "%9.1f" % (us / 1000.0) if us is not None else "        -"


def show_group(name, runs, linux_runs):
    wall = median([r["build"]["wall_us"] for r in runs])
    print("\n== %s: %d runs; %d steps ran, %d hit" % (name, len(runs), runs[0]["ran"], runs[0]["hit"]))
    line = "   ccl-build wall %s ms (median)" % ms(wall).strip()
    host = median([r.get("host_wall_s") for r in runs])
    if host is not None:
        line += "; host-measured %.1f ms" % (host * 1000)
    guest = median([r.get("guest_cpu_s") for r in runs])
    if guest is not None:
        line += "; guest CPU %.1f ms over %d vCPUs (%.0f%% of one CPU)" % (
            guest * 1000, runs[0].get("vcpus", 0), 100.0 * guest / (wall / MICROSECONDS) if wall else 0)
    print(line)
    if linux_runs:
        linux_wall = median([r["build"]["wall_us"] for r in linux_runs])
        print("   Linux ccl-build wall %s ms: CuBit/Linux = %.1fx" % (ms(linux_wall).strip(), wall / linux_wall))
    cats = category_medians(runs)
    linux_cats = category_medians(linux_runs) if linux_runs else {}
    print("   %-14s %9s %6s %9s" % ("category", "CuBit ms", "share", "Linux ms"))
    for c, us in sorted(cats.items(), key=lambda kv: -kv[1]):
        print("   %-14s %s %5.1f%% %s" % (c, ms(us), 100.0 * us / wall if wall else 0, ms(linux_cats.get(c))))
    counts = count_medians(runs)
    linux_counts = count_medians(linux_runs) if linux_runs else {}
    print("   counts: " + ", ".join("%s %d%s" % (c, v, " (Linux %d)" % linux_counts[c] if c in linux_counts else "")
                                     for c, v in counts.items()))
    show_spawns(runs)
    steps = step_medians(runs)
    linux_steps = step_medians(linux_runs) if linux_runs else {}
    print("   %-44s %9s %9s %9s %9s %9s %9s" % ("step", "wall", "Linux", "spawn", "tool", "place", "release"))
    for label, item in steps.items():
        if not item["ran"]:
            continue
        print("   %-44s %s %s %s %s %s %s" % (
            label[:44], ms(item["wall"]), ms(linux_steps.get(label, {}).get("wall")),
            ms(item.get("spawn", 0)), ms(item.get("tool_wait", 0)), ms(item.get("place_inputs", 0)),
            ms(item.get("release", 0))))
    return cats, linux_cats


def show_spawns(runs):
    """Per program: how many starts per build, image size, read and kernel spawn ms."""
    by_name = {}
    for r in runs:
        per_run = {}
        for s in r.get("spawns", []):
            per_run.setdefault(s["name"], []).append(s)
        for name, items in per_run.items():
            entry = by_name.setdefault(name, {"counts": [], "items": []})
            entry["counts"].append(len(items))
            entry["items"].extend(items)
    if not by_name:
        return
    print("   %-58s %5s %8s %8s %8s %10s" % ("program started (procmgr)", "count", "MiB", "read ms", "spawn ms",
                                            "MiB/s read"))
    for name, entry in sorted(by_name.items(), key=lambda kv: -sum(i.get("read_ms", 0) for i in kv[1]["items"])):
        items = entry["items"]
        size = median([i.get("image_bytes") for i in items])
        read = median([i.get("read_ms") for i in items])
        spawn = median([i.get("spawn_ms") for i in items])
        rate = (size / 1048576.0) / (read / 1000.0) if size and read else None
        print("   %-58s %5s %8.1f %8s %8s %10s" % (
            name[-58:], median(entry["counts"]), (size or 0) / 1048576.0, read, spawn,
            "%.0f" % rate if rate else "-"))


def split(cats, linux_cats):
    """Fixed per-process, per-file and compile work (microseconds)."""
    tool = cats.get("tool_wait", 0)
    linux_tool = linux_cats.get("tool_wait", 0)
    fixed = cats.get("spawn", 0) + max(0, tool - linux_tool)
    per_file = sum(cats.get(c, 0) for c in PER_FILE)
    compile_work = min(tool, linux_tool)
    other = sum(cats.values()) - fixed - per_file - compile_work
    return {"fixed_per_process": fixed, "per_file": per_file, "compile_work": compile_work,
            "ccl_build_own": other}


def show(cubit, linux=None):
    found = groups(cubit)
    linux_found = groups(linux) if linux else {}
    print("\nPERF-010 build benchmark (%s, %s)" % (cubit.get("platform"), cubit.get("where", "")))
    contention = cubit.get("contention", {})
    for when in ("before", "after"):
        c = contention.get(when)
        if c:
            print("contention %s: other QEMU processes %s, load average %s, %s host CPUs" % (
                when, c.get("other_qemu_processes") or "none", c.get("load_average"), c.get("cpus")))
    splits = {}
    for (app, kind) in sorted(found, key=lambda k: (k[0], KINDS.index(k[1]))):
        cats, linux_cats = show_group("%s %s" % (app, kind), found[(app, kind)], linux_found.get((app, kind)))
        if linux_cats:
            splits[(app, kind)] = split(cats, linux_cats)
    for (app, kind), parts in splits.items():
        total = sum(parts.values())
        print("\n%s %s split: " % (app, kind) + ", ".join(
            "%s %s ms (%.0f%%)" % (k, ms(v).strip(), 100.0 * v / total if total else 0) for k, v in parts.items()))


if __name__ == "__main__":
    show(json.loads(open(sys.argv[1]).read()),
         json.loads(open(sys.argv[2]).read()) if len(sys.argv) > 2 else None)
