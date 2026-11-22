#!/usr/bin/env python3
"""WEB-001 compatibility survey: the table, failure causes and gallery.

    analyze.py DIR [DIR...]    (run-survey.py output directories, merged)

Writes the first DIR's report/: table.md (per site and engine, with a per-engine summary
row), causes.md (failure causes ranked by the sites they break, port-only
failures listed separately), summary.json, and gallery/ (index.html with
labelled side-by-side screenshots).

Per site and engine, over the runs (medians):
- loaded: the load event fired within the deadline (most runs);
- first paint: first-contentful-paint (Paint Timing), from navigation start;
- load: loadEventEnd (Navigation Timing);
- visible text: the probe's first sight of MINIMUM_TEXT_CHARACTERS of
  rendered text (probe.js);
- JS errors: uncaught errors and unhandled rejections the probe saw;
- text: rendered text characters at the summary, as a share of Firefox's;
- similarity: SSIM of the first screenshot against Firefox's (greyscale,
  downscaled; the best match over all pairs of runs), next to the healthy
  baseline: the lower of Firefox's own
  run-to-run similarity (live sites change between loads) and Chromium's
  similarity to Firefox (another engine's fonts and antialiasing).

Classes (Firefox is the reference; a site whose Firefox load failed or
shows no text has no reference and is not classified):
- blank/crash: crashed in most runs, a blank screenshot, or under
  BLANK_TEXT_SHARE of Firefox's text;
- broken: under BROKEN_TEXT_SHARE of the text, a similarity this far under
  Firefox's own (BROKEN_SIMILARITY_DROP), or a scripted interaction that
  works in Firefox fails;
- minor: under MINOR_TEXT_SHARE of the text, a similarity MINOR_SIMILARITY_DROP
  under Firefox's own, more JS errors than Firefox, or no load event;
- works: none of these.
"""
import collections
import html
import json
import pathlib
import re
import statistics
import sys

import numpy
from PIL import Image, ImageDraw, ImageFont

REFERENCE = "firefox"
# Another healthy engine: its similarity to Firefox is part of the baseline.
BASELINE_ENGINE = "chromium"
ENGINE_ORDER = ("servo", "penny", "wpe", "chromium", "firefox")
ENGINE_LABELS = {"servo": "Servo (hosted Linux)", "penny": "Penny on CuBit",
                 "wpe": "WPE WebKit (hosted Linux, sandboxed)", "chromium": "Chromium", "firefox": "Firefox"}
GALLERY_ENGINES = ("servo", "penny", "wpe", "firefox")
CLASSES = ("works", "minor", "broken", "blank/crash")
NO_REFERENCE = "no reference"
BLANK_TEXT_SHARE = 0.05
BROKEN_TEXT_SHARE = 0.5
MINOR_TEXT_SHARE = 0.85
BROKEN_SIMILARITY_DROP = 0.40
MINOR_SIMILARITY_DROP = 0.15
REFERENCE_MINIMUM_TEXT = 50
# A screenshot this uniform is blank (greyscale standard deviation).
BLANK_DEVIATION = 2.0
SIMILARITY_SCALE = 4
SIMILARITY_WINDOW = 7
THUMB_WIDTH = 480
LABEL_HEIGHT = 28
GALLERY_SUCCESSES = 10
GALLERY_FAILURES = 15


def load_records(directory):
    records = []
    for path in sorted(directory.glob("*/run*/records.jsonl")):
        for line in path.read_text().splitlines():
            if line.strip():
                records.append(json.loads(line))
    return records


def median(values):
    values = [v for v in values if v is not None]
    return statistics.median(values) if values else None


def spread(values):
    values = [v for v in values if v is not None]
    return (min(values), max(values)) if len(values) > 1 else None


def grey(path):
    with Image.open(path) as image:
        small = image.convert("L").resize((image.width // SIMILARITY_SCALE, image.height // SIMILARITY_SCALE))
        return numpy.asarray(small, dtype=numpy.float64)


def box_mean(array, size):
    padded = numpy.pad(array, size // 2, mode="edge")
    total = padded.cumsum(0).cumsum(1)
    total = numpy.pad(total, ((1, 0), (1, 0)))
    return (total[size:, size:] - total[:-size, size:] - total[size:, :-size] + total[:-size, :-size]) / size ** 2


def ssim(a, b):
    """Structural similarity of two greyscale images (box window)."""
    if a.shape != b.shape:
        height, width = min(a.shape[0], b.shape[0]), min(a.shape[1], b.shape[1])
        a, b = a[:height, :width], b[:height, :width]
    c1, c2 = (0.01 * 255) ** 2, (0.03 * 255) ** 2
    mean_a, mean_b = box_mean(a, SIMILARITY_WINDOW), box_mean(b, SIMILARITY_WINDOW)
    var_a = box_mean(a * a, SIMILARITY_WINDOW) - mean_a ** 2
    var_b = box_mean(b * b, SIMILARITY_WINDOW) - mean_b ** 2
    covariance = box_mean(a * b, SIMILARITY_WINDOW) - mean_a * mean_b
    value = ((2 * mean_a * mean_b + c1) * (2 * covariance + c2)) / (
        (mean_a ** 2 + mean_b ** 2 + c1) * (var_a + var_b + c2))
    return float(value.mean())


class Survey:
    def __init__(self, directories, sources=None):
        """sources: engine -> the directories its records come from (a
        rerun of one engine replaces its earlier runs in the merge)."""
        self.directories = directories
        self.sources = sources or {}
        metas = [json.loads((d / "survey.json").read_text()) for d in directories]
        self.meta = dict(metas[0])
        if len({m["viewport"] for m in metas}) != 1:
            raise SystemExit("surveys with different viewports cannot be merged")
        self.sites, surveyed = [], set()
        for directory, meta in zip(directories, metas):
            listed = {s["id"]: s for s in json.loads((directory / "sites.json").read_text())["sites"]}
            for site in meta["sites"]:
                if site not in surveyed:
                    surveyed.add(site)
                    self.sites.append(listed[site])
        self.site_ids = [s["id"] for s in self.sites]
        self.records = [r for d in directories for r in load_records(d)
                        if d in self.sources.get(r["engine"], [d])]
        engines = {e for m in metas for e in m["engines"]}
        self.engines = [e for e in ENGINE_ORDER if e in engines]
        self.versions = {r["engine"]: r["text"] for r in self.records if r["kind"] == "version"}
        self.by = collections.defaultdict(list)
        for record in self.records:
            self.by[(record["engine"], record["site"], record["run"])].append(record)
        self.runs = sorted({r["run"] for r in self.records})
        self.rows = {}

    def shot(self, engine, site, run, name="top"):
        for directory in self.sources.get(engine, self.directories):
            path = directory / engine / f"run{run}" / "shots" / f"{site}-{name}.png"
            if path.exists():
                return path
        return None

    def run_facts(self, engine, site, run):
        records = self.by.get((engine, site, run), [])
        if not records:
            return None
        facts = {"loaded": False, "crashed": False, "interactions": {}, "console": 0}
        for record in records:
            kind, name, text = record["kind"], record["name"], record["text"]
            if kind == "load":
                load = json.loads(text)
                facts["loaded"] = load.get("loaded", False)
                facts["shell_load_ms"] = load.get("load_ms")
                facts["first_frame_ms"] = load.get("first_frame_ms")
                facts["load_error"] = load.get("error")
                facts["crashed"] |= bool(load.get("crashed"))
            elif kind == "js-ok" and name == "collect":
                try:
                    facts["collect"] = json.loads(text)
                except json.JSONDecodeError:
                    facts["collect_error"] = text[:200]
            elif kind == "js-error" and name == "collect":
                facts["collect_error"] = text[:200]
            elif kind == "js-ok" and name in ("scroll", "menu", "typed"):
                try:
                    facts["interactions"][name] = bool(json.loads(text).get("ok"))
                except (json.JSONDecodeError, AttributeError):
                    facts["interactions"][name] = False
            elif kind == "js-error" and name in ("scroll", "menu", "typed"):
                facts["interactions"][name] = False
            elif kind in ("crash", "site-failed"):
                facts["crashed"] = True
                facts["crash"] = text[:300]
            elif kind == "console" and name == "count":
                facts["console"] = int(text)
        shot = self.shot(engine, site, run)
        if shot:
            image = grey(shot)
            facts["blank"] = float(image.std()) < BLANK_DEVIATION
        else:
            facts["blank"] = True
        return facts

    def aggregate(self, engine, site):
        runs = [(run, self.run_facts(engine, site, run)) for run in self.runs]
        runs = [(run, f) for run, f in runs if f is not None]
        if not runs:
            return None
        facts = [f for _, f in runs]
        collect = [f.get("collect") or {} for f in facts]
        row = {
            "runs": len(facts),
            "loaded": sum(f["loaded"] for f in facts) * 2 > len(facts),
            "crashed": sum(f["crashed"] for f in facts) * 2 > len(facts),
            "crash": next((f["crash"] for f in facts if f.get("crash")), None),
            "load_error": next((f["load_error"] for f in facts if f.get("load_error")), None),
            "blank": sum(f["blank"] for f in facts) * 2 > len(facts),
            "first_paint": median([c.get("firstContentfulPaint") for c in collect]),
            "first_paint_spread": spread([c.get("firstContentfulPaint") for c in collect]),
            "load": median([c.get("load") for c in collect]),
            "load_spread": spread([c.get("load") for c in collect]),
            "text_at": median([c.get("textAt") for c in collect]),
            "text_at_spread": spread([c.get("textAt") for c in collect]),
            "first_frame": median([f.get("first_frame_ms") for f in facts]),
            "text": median([c.get("textCharacters") for c in collect]) or 0,
            "js_errors": median([c.get("errorCount") for c in collect if c]) or 0,
            "console": median([f["console"] for f in facts]) or 0,
            "error_messages": sorted({m for c in collect for m in c.get("errors", [])}),
            "collect_error": next((f["collect_error"] for f in facts if f.get("collect_error")), None),
            "interactions": {},
            "title": next((c.get("title") for c in collect if c.get("title")), ""),
        }
        names = {n for f in facts for n in f["interactions"]}
        for name in names:
            outcomes = [f["interactions"][name] for f in facts if name in f["interactions"]]
            row["interactions"][name] = sum(outcomes) * 2 > len(outcomes)
        return row

    def similarity(self, engine, site):
        """The best match over every pair of runs: live sites show banners,
        ads and rotating content in some loads and not others."""
        values = []
        for run in self.runs:
            for other in self.runs:
                a, b = self.shot(engine, site, run), self.shot(REFERENCE, site, other)
                if a and b:
                    values.append(ssim(grey(a), grey(b)))
        return max(values) if values else None

    def reference_noise(self, site):
        values = []
        for first in self.runs:
            for second in self.runs:
                if second > first:
                    a, b = self.shot(REFERENCE, site, first), self.shot(REFERENCE, site, second)
                    if a and b:
                        values.append(ssim(grey(a), grey(b)))
        return median(values)

    def classify(self, engine, site, row, reference, noise):
        reasons = []
        if row["crashed"]:
            reasons.append("crash")
        if row["blank"]:
            reasons.append("blank screenshot")
        share = row["text"] / reference["text"]
        row["text_share"] = share
        if share < BLANK_TEXT_SHARE:
            reasons.append(f"text {share:.0%} of Firefox")
        if reasons:
            return "blank/crash", reasons
        similarity = row.get("similarity")
        baseline = noise if noise is not None else 1.0
        if share < BROKEN_TEXT_SHARE:
            reasons.append(f"text {share:.0%} of Firefox")
        if similarity is not None and similarity < baseline - BROKEN_SIMILARITY_DROP:
            reasons.append(f"looks different (similarity {similarity:.2f}, baseline {baseline:.2f})")
        for name, ok in row["interactions"].items():
            if not ok and reference["interactions"].get(name):
                reasons.append(f"{name} failed")
        if reasons:
            return "broken", reasons
        if share < MINOR_TEXT_SHARE:
            reasons.append(f"text {share:.0%} of Firefox")
        if similarity is not None and similarity < baseline - MINOR_SIMILARITY_DROP:
            reasons.append(f"differs (similarity {similarity:.2f}, baseline {baseline:.2f})")
        if row["js_errors"] > reference["js_errors"]:
            reasons.append(f"JS errors {row['js_errors']:g} (Firefox {reference['js_errors']:g})")
        if not row["loaded"]:
            reasons.append("no load event in time")
        return ("minor" if reasons else "works"), reasons

    def analyze(self):
        for site in self.site_ids:
            reference = self.aggregate(REFERENCE, site)
            # What a healthy rendering scores against Firefox: Firefox's own
            # run-to-run similarity (live content changes) and Chromium's
            # (another engine's fonts and antialiasing), whichever is lower.
            healthy = [v for v in (self.reference_noise(site),
                                   self.similarity(BASELINE_ENGINE, site) if BASELINE_ENGINE in self.engines else None)
                       if v is not None]
            noise = min(healthy) if healthy else None
            usable = (reference is not None and not reference["crashed"] and not reference["blank"]
                      and reference["text"] >= REFERENCE_MINIMUM_TEXT)
            for engine in self.engines:
                row = self.aggregate(engine, site)
                if row is None:
                    continue
                if engine != REFERENCE:
                    row["similarity"] = self.similarity(engine, site)
                row["reference_noise"] = noise
                if not usable:
                    row["class"], row["reasons"] = NO_REFERENCE, ["Firefox did not show the page"]
                elif engine == REFERENCE:
                    row["class"], row["reasons"] = "works", []
                    row["text_share"] = 1.0
                else:
                    row["class"], row["reasons"] = self.classify(engine, site, row, reference, noise)
                self.rows[(engine, site)] = row


def fmt(value):
    return "–" if value is None else f"{value:.0f}"


def causes(row):
    """The cause labels a failing row counts toward."""
    out = []
    if row.get("crash"):
        out.append("crash: " + re.sub(r"0x[0-9a-f]+|\d+", "N", row["crash"])[:90])
    if row.get("load_error"):
        out.append("load error: " + re.sub(r"\d+", "N", row["load_error"])[:90])
    if not row["loaded"] and not row.get("crash"):
        out.append("no load event within the deadline")
    for message in row["error_messages"]:
        out.append("JS " + re.sub(r"'[^']*'|\"[^\"]*\"|\d+", "_", message)[:100])
    for reason in row["reasons"]:
        if reason.startswith(("looks different", "differs")):
            out.append("rendering differs from Firefox (text present)")
        elif reason.startswith("text"):
            out.append("missing text")
        elif reason.endswith("failed"):
            out.append("interaction failed: " + reason.split()[0])
        elif reason == "blank screenshot":
            out.append("blank screenshot")
    return sorted(set(out))


def write_table(survey, report):
    engines = survey.engines
    lines = [f"# WEB-001 compatibility survey ({survey.meta['host']}, {survey.meta['started']})", "",
             f"Viewport {survey.meta['viewport']}, {len(survey.runs)} runs, load deadline "
             f"{survey.meta['load_deadline_ms'] // 1000} s. Times in ms from navigation start, medians: "
             "FP = first-contentful-paint, Load = loadEventEnd, Text = first visible text. "
             "Text share and similarity are against Firefox.", ""]
    lines.append("Engines: " + "; ".join(f"{ENGINE_LABELS[e]}: {survey.versions.get(e, 'cubitshell')}"
                                         for e in engines))
    lines.append("")
    header = "| Site | " + " | ".join(ENGINE_LABELS[e] for e in engines) + " |"
    lines += [header, "|" + "---|" * (len(engines) + 1)]
    for group in ("priority", "tranco"):
        for site in [s for s in survey.sites if s["id"] in survey.site_ids and s["group"] == group]:
            cells = []
            for engine in engines:
                row = survey.rows.get((engine, site["id"]))
                if row is None:
                    cells.append("not run")
                    continue
                share = row.get("text_share")
                cells.append(f"**{row['class']}** FP {fmt(row['first_paint'])} / Load {fmt(row['load'])} / "
                             f"Text {fmt(row['text_at'])}; text {'–' if share is None else f'{share:.0%}'}, "
                             f"JS err {row['js_errors']:g}"
                             + (f", sim {row['similarity']:.2f}" if row.get("similarity") is not None else ""))
            lines.append(f"| {site['id']} ({group}) | " + " | ".join(cells) + " |")
    lines += ["", "## Per-engine summary", ""]
    lines.append("| Engine | Group | Sites | " + " | ".join(CLASSES) + " | no reference | Median FP | Median Load | Median Text |")
    lines.append("|---|---|---|" + "---|" * (len(CLASSES) + 4))
    summary = {}
    for engine in engines:
        for group in ("priority", "tranco", "all"):
            ids = [s["id"] for s in survey.sites if s["id"] in survey.site_ids
                   and (group == "all" or s["group"] == group)]
            rows = [survey.rows[(engine, i)] for i in ids if (engine, i) in survey.rows]
            counts = collections.Counter(r["class"] for r in rows)
            loaded = [r for r in rows if r["class"] != NO_REFERENCE]
            entry = {"sites": len(rows), **{c: counts.get(c, 0) for c in CLASSES},
                     NO_REFERENCE: counts.get(NO_REFERENCE, 0),
                     "first_paint": median([r["first_paint"] for r in loaded]),
                     "load": median([r["load"] for r in loaded]),
                     "text_at": median([r["text_at"] for r in loaded])}
            summary.setdefault(engine, {})[group] = entry
            lines.append(f"| {ENGINE_LABELS[engine]} | {group} | {entry['sites']} | "
                         + " | ".join(str(entry[c]) for c in CLASSES)
                         + f" | {entry[NO_REFERENCE]} | {fmt(entry['first_paint'])} | {fmt(entry['load'])} | {fmt(entry['text_at'])} |")
    # Like for like: the sites where every engine reported all three times.
    common = [s for s in survey.site_ids
              if all(survey.rows.get((e, s)) and all(survey.rows[(e, s)].get(k) is not None
                                                     for k in ("first_paint", "load", "text_at"))
                     for e in engines)]
    lines += ["", f"## Timing medians over the {len(common)} sites every engine loaded (ms)", "",
              "| Engine | First paint | Load | Visible text |", "|---|---|---|---|"]
    for engine in engines:
        values = {k: median([survey.rows[(engine, s)][k] for s in common]) for k in ("first_paint", "load", "text_at")}
        summary[engine]["common"] = {"sites": len(common), **values}
        lines.append(f"| {ENGINE_LABELS[engine]} | {fmt(values['first_paint'])} | {fmt(values['load'])} | {fmt(values['text_at'])} |")
    (report / "table.md").write_text("\n".join(lines) + "\n")
    return summary


def write_causes(survey, report):
    lines = ["# Failure causes", ""]
    ranked = {}
    for engine in [e for e in survey.engines if e != REFERENCE]:
        counter, minor = collections.Counter(), collections.Counter()
        examples = collections.defaultdict(list)
        for site in survey.site_ids:
            row = survey.rows.get((engine, site))
            if not row or row["class"] in ("works", NO_REFERENCE):
                continue
            breaking = row["class"] in ("broken", "blank/crash")
            target = counter if breaking else minor
            for cause in causes(row):
                target[cause] += 1
                examples[(breaking, cause)].append(site)
        ranked[engine] = {"breaking": counter.most_common(), "minor": minor.most_common()}
        lines += [f"## {ENGINE_LABELS[engine]}", "", "Causes on broken or blank/crash sites (a site can have several):", ""]
        for cause, count in counter.most_common(25):
            lines.append(f"- {count} sites: {cause} (e.g. {', '.join(examples[(True, cause)][:4])})")
        lines += ["", "On minor sites:", ""]
        for cause, count in minor.most_common(15):
            lines.append(f"- {count} sites: {cause} (e.g. {', '.join(examples[(False, cause)][:4])})")
        lines.append("")
    if "servo" in survey.engines and "penny" in survey.engines:
        lines += ["## Port-only failures (work hosted, break on CuBit)", ""]
        port_only = []
        for site in survey.site_ids:
            hosted, native = survey.rows.get(("servo", site)), survey.rows.get(("penny", site))
            if hosted and native and hosted["class"] in ("works", "minor") and native["class"] in ("broken", "blank/crash"):
                port_only.append(site)
                lines.append(f"- {site}: hosted {hosted['class']}, CuBit {native['class']}: "
                             + "; ".join(native["reasons"] + causes(native))[:300])
        if not port_only:
            lines.append("- none")
        ranked["port_only"] = port_only
    (report / "causes.md").write_text("\n".join(lines) + "\n")
    return ranked


def labelled(path, label, width):
    font = ImageFont.load_default()
    if path is None:
        tile = Image.new("RGB", (width, width * 5 // 8 + LABEL_HEIGHT), "#ddd")
        ImageDraw.Draw(tile).text((8, 6), f"{label}: no screenshot", fill="black", font=font)
        return tile
    with Image.open(path) as image:
        height = image.height * width // image.width
        tile = Image.new("RGB", (width, height + LABEL_HEIGHT), "white")
        tile.paste(image.convert("RGB").resize((width, height)), (0, LABEL_HEIGHT))
    ImageDraw.Draw(tile).text((8, 6), label, fill="black", font=font)
    return tile


def write_gallery(survey, report):
    gallery = report / "gallery"
    gallery.mkdir(parents=True, exist_ok=True)
    engines = [e for e in GALLERY_ENGINES if e in survey.engines]
    priority = [s["id"] for s in survey.sites if s["group"] == "priority" and s["id"] in survey.site_ids]
    tranco = [s for s in survey.site_ids if s not in priority]

    def score(site):
        row = survey.rows.get(("servo", site)) or survey.rows.get(("penny", site))
        return (CLASSES.index(row["class"]) if row and row["class"] in CLASSES else -1,
                row.get("similarity") or 0 if row else 0)

    successes = sorted([s for s in tranco if score(s)[0] == 0], key=lambda s: -score(s)[1])[:GALLERY_SUCCESSES]
    failures = sorted([s for s in tranco if score(s)[0] >= 2], key=lambda s: score(s))[:GALLERY_FAILURES]
    sections = [("Priority sites", priority), ("Clearest successes", successes), ("Clearest failures", failures)]
    body = []
    for title, sites in sections:
        body.append(f"<h2>{html.escape(title)}</h2>")
        for site in sites:
            tiles = []
            for engine in engines:
                row = survey.rows.get((engine, site), {})
                label = f"{ENGINE_LABELS[engine]}: {row.get('class', 'not run')}"
                tiles.append(labelled(survey.shot(engine, site, 0), label, THUMB_WIDTH))
            height = max(t.height for t in tiles)
            strip = Image.new("RGB", (THUMB_WIDTH * len(tiles) + 8 * (len(tiles) - 1), height), "white")
            for index, tile in enumerate(tiles):
                strip.paste(tile, (index * (THUMB_WIDTH + 8), 0))
            name = f"{site}.jpg"
            strip.save(gallery / name, quality=85)
            details = []
            for engine in engines:
                row = survey.rows.get((engine, site))
                if row:
                    details.append(f"<b>{html.escape(ENGINE_LABELS[engine])}</b>: {html.escape(row['class'])}"
                                   + (f" ({html.escape('; '.join(row['reasons']))})" if row["reasons"] else ""))
            url = next(s["url"] for s in survey.sites if s["id"] == site)
            body.append(f"<section><h3>{html.escape(site)} <small><a href=\"{html.escape(url)}\">{html.escape(url)}</a></small></h3>"
                        f"<img src=\"{name}\" alt=\"{html.escape(site)}: {' | '.join(engines)}\">"
                        f"<p>{'<br>'.join(details)}</p></section>")
    page = ("<!doctype html><meta charset=utf-8><title>Penny compatibility gallery</title>"
            "<style>body{font:15px system-ui,sans-serif;margin:16px;max-width:2000px}img{max-width:100%;"
            "border:1px solid #ccc}section{margin-bottom:28px}small{font-weight:normal;color:#555}</style>"
            f"<h1>Penny compatibility gallery</h1><p>{html.escape(survey.meta['host'])}, "
            f"{html.escape(survey.meta['started'])}, viewport {html.escape(survey.meta['viewport'])}. "
            "Left to right: " + ", ".join(ENGINE_LABELS[e] for e in engines)
            + ". First load of run 0, after the settle period.</p>" + "".join(body))
    (gallery / "index.html").write_text(page)


def main():
    """analyze.py DIR... [--engine-from ENGINE=DIR]...: the report lands in
    the first DIR; --engine-from takes ENGINE's records only from DIR
    (repeatable; DIR is added to the merge)."""
    directories, sources, arguments = [], {}, sys.argv[1:]
    while arguments:
        argument = arguments.pop(0)
        if argument == "--engine-from":
            engine, directory = arguments.pop(0).split("=", 1)
            directory = pathlib.Path(directory).resolve()
            sources.setdefault(engine, []).append(directory)
            if directory not in directories:
                directories.append(directory)
        else:
            directory = pathlib.Path(argument).resolve()
            if directory not in directories:
                directories.append(directory)
    survey = Survey(directories, sources)
    survey.analyze()
    report = directories[0] / "report"
    report.mkdir(exist_ok=True)
    summary = write_table(survey, report)
    ranked = write_causes(survey, report)
    write_gallery(survey, report)
    rows = {f"{engine}/{site}": row for (engine, site), row in survey.rows.items()}
    (report / "summary.json").write_text(json.dumps({"summary": summary, "causes": ranked, "rows": rows},
                                                    indent=1, default=str))
    print((report / "table.md").read_text().split("## Per-engine summary")[1])


if __name__ == "__main__":
    main()
