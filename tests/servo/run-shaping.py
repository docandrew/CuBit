#!/usr/bin/env python3
"""Penny's text shaping: HarfRust (Rust) against HarfBuzz (C++), WEB-004.

    run-shaping.py --app HOSTED_CUBITSHELL_WITH_BOTH_SHAPERS --out DIR

Needs a hosted cubitshell built with both shapers
(build-cubitshell-hosted.sh --features harfbuzz; the default features add
harfrust). Each sample is a page with one line of text in a web font
(shaping/fonts.nix, pinned): Arabic (Amiri's heavy contextual forms and
marks; Noto Sans Arabic), Devanagari conjuncts and reordering, Hebrew with
points, Thai, emoji ZWJ sequences (Noto Emoji's outlines), Fira Code's programming ligatures (a
variable font) and EB Garamond's fi/fl/ct ligatures.

The same binary loads every sample three times, by the shaping-backend
flag:
- compare: Servo shapes each run with both and prints SHAPING DATA
  DIFFERENT for every run whose glyphs, clusters, advances or offsets
  differ, and the cumulative time of each;
- harfbuzz and harfrust: the text's measured box and a screenshot.
Pass: no run differs, the boxes are equal, and the screenshots are
identical. DIR keeps the screenshots (PNG) and the compare log.
"""
import argparse
import html
import http.server
import json
import os
import pathlib
import re
import subprocess
import sys
import tempfile
import threading

ROOT = pathlib.Path(__file__).resolve().parents[2]
DEADLINE_SECONDS = 300
LOAD_DEADLINE_MS = 20000
# Web fonts load after the document: wait for them before measuring.
SETTLE_MS = 2500
FONT_PX = 44
SAMPLES = [
    ("arabic-amiri", "Amiri-Regular.ttf", "ar", "rtl",
     "بِسْمِ ٱللَّٰهِ ٱلرَّحْمَٰنِ ٱلرَّحِيمِ — مُحَمَّدٌ رَسُولُ ٱللَّهِ ١٢٣"),
    ("arabic-noto", "NotoSansArabic.ttf", "ar", "rtl",
     "اللغة العربية لغة جميلة، لا إله إلا الله؛ فِي ٱلْبَدْءِ"),
    ("devanagari", "NotoSansDevanagari.ttf", "hi", "ltr",
     "हिन्दी क्षत्रिय श्री द्वार प्रार्थना कृष्ण र्क त्र ज्ञ"),
    ("hebrew", "NotoSansHebrew.ttf", "he", "rtl", "בְּרֵאשִׁית בָּרָא אֱלֹהִים אֵת הַשָּׁמַיִם"),
    ("thai", "NotoSansThai.ttf", "th", "ltr", "ภาษาไทยเป็นภาษาที่สวยงาม กิ่ง น้ำ ปั้น"),
    ("emoji", "NotoEmoji.ttf", "en", "ltr", "👩‍👩‍👧‍👦 👍🏽 🏳️‍🌈 🇨🇦 1️⃣ ❤️‍🔥"),
    ("fira-ligatures", "FiraCode-VF.ttf", "en", "ltr", "=> != === <= >= -> <!-- www ::: |> <|>"),
    ("garamond-ligatures", "EBGaramond12-Regular.ttf", "en", "ltr", "office affluent ffl fi fl Th ct st 1234"),
]
# The sample's box, and the same text's in the default font: they differ
# when the web font is in use (Servo's FontFace.status stays "loading").
MEASURE = ("JSON.stringify((() => { const r = document.getElementById('t').getBoundingClientRect(); "
           "const f = document.getElementById('f').getBoundingClientRect(); "
           "return {width: r.width, height: r.height, fallback: f.width}; })())")


def page(font, lang, direction, text):
    return (f"<!doctype html><html lang='{lang}' dir='{direction}'><meta charset='utf-8'>"
            f"<style>@font-face {{ font-family: Sample; src: url('/fonts/{font}'); }}"
            f"body {{ margin: 24px; background: white; }}"
            f"#t {{ font: {FONT_PX}px Sample; color: black; white-space: nowrap; }}"
            f"#f {{ font: {FONT_PX}px serif; color: #888; white-space: nowrap; }}</style>"
            f"<span id='t'>{html.escape(text)}</span><br><span id='f'>{html.escape(text)}</span>").encode()


def serve(fonts):
    pages = {f"/{name}": page(font, lang, d, text) for name, font, lang, d, text in SAMPLES}

    class Handler(http.server.BaseHTTPRequestHandler):
        def do_GET(self):
            if self.path in pages:
                body, kind = pages[self.path], "text/html; charset=utf-8"
            elif self.path.startswith("/fonts/") and (fonts / self.path[7:]).is_file():
                body, kind = (fonts / self.path[7:]).read_bytes(), "font/ttf"
            else:
                self.send_error(404)
                return
            self.send_response(200)
            self.send_header("Content-Type", kind)
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)

        def log_message(self, *args):
            pass

    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    server.daemon_threads = True
    threading.Thread(target=server.serve_forever, daemon=True).start()
    return server


def run(app, backend, port, out):
    """One cubitshell process over every sample: name -> measure, and the log."""
    with tempfile.TemporaryDirectory(prefix="penny-shaping-") as directory:
        flags = pathlib.Path(directory)
        (flags / "compat").write_text("")
        (flags / "log-level").write_text("error")
        (flags / "compat-probe").write_text("window.__probe = true;")
        (flags / "viewport").write_text("1200x300")
        (flags / "shaping-backend").write_text(backend)
        (flags / "compat-plan").write_text("".join(
            f"load\t{name}\t{LOAD_DEADLINE_MS}\thttp://127.0.0.1:{port}/{name}\nwait\t{SETTLE_MS}\n"
            f"js\tmeasure\t{MEASURE}\nshot\ttext\n" for name, *_ in SAMPLES))
        completed = subprocess.run([app], env=dict(os.environ, PENNY_FLAGS=str(flags)),
                                   capture_output=True, text=True, timeout=DEADLINE_SECONDS)
        log = completed.stdout + completed.stderr
        from PIL import Image
        for ppm in flags.glob("compat-*.ppm"):
            with Image.open(ppm) as image:
                image.save(out / f"{backend}-{ppm.stem.removeprefix('compat-')}.png")
    measures = {}
    for name, *_ in SAMPLES:
        chunks = re.findall(rf"PENNY-COMPAT js-ok {re.escape(name)} measure \d+/\d+ (.*)", log)
        measures[name] = json.loads("".join(chunks)) if chunks else None
    return measures, log


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--app", default=str(ROOT / "userspace/rust/build/servo-work/target-hosted/"
                                             "x86_64-unknown-linux-gnu/release/cubitshell"))
    parser.add_argument("--out", type=pathlib.Path, required=True)
    options = parser.parse_args()
    options.out.mkdir(parents=True, exist_ok=True)
    fonts = pathlib.Path(subprocess.check_output(
        ["nix", "build", "--no-link", "--print-out-paths", "-f", str(ROOT / "tests/servo/shaping/fonts.nix")],
        text=True).strip())
    server = serve(fonts)
    failures = []
    _, compare_log = run(options.app, "compare", server.server_port, options.out)
    (options.out / "compare.log").write_text(compare_log)
    differing = compare_log.count("SHAPING DATA DIFFERENT")
    timings = re.findall(r"Cumulative timings: Buzz: ([\d.]+)ms\. Rust: ([\d.]+)ms", compare_log)
    if not timings:
        failures.append("compare: no shaping ran (a build without both shapers?)")
    elif differing:
        failures.append(f"compare: {differing} text runs shaped differently (compare.log)")
    if timings:
        buzz, rust = (float(v) for v in timings[-1])
        print(f"compare: {differing} differing runs; cumulative shaping HarfBuzz {buzz:.3f} ms, "
              f"HarfRust {rust:.3f} ms")
    results = {backend: run(options.app, backend, server.server_port, options.out)[0]
               for backend in ("harfbuzz", "harfrust")}
    from PIL import Image, ImageChops
    for name, *_ in SAMPLES:
        a, b = results["harfbuzz"][name], results["harfrust"][name]
        if a is None or b is None:
            failures.append(f"{name}: not measured (harfbuzz {a}, harfrust {b})")
            continue
        if abs(a["width"] - a["fallback"]) < 0.5:
            failures.append(f"{name}: the web font is not in use (as wide as the default font)")
        if (a["width"], a["height"]) != (b["width"], b["height"]):
            failures.append(f"{name}: box {a['width']}x{a['height']} (HarfBuzz) vs {b['width']}x{b['height']} (HarfRust)")
        shots = [options.out / f"{backend}-{name}-text.png" for backend in ("harfbuzz", "harfrust")]
        if all(s.exists() for s in shots):
            with Image.open(shots[0]) as x, Image.open(shots[1]) as y:
                box = ImageChops.difference(x.convert("RGB"), y.convert("RGB")).getbbox()
            if box:
                failures.append(f"{name}: screenshots differ in {box}")
        else:
            failures.append(f"{name}: missing screenshot")
        print(f"{name}: {b['width']:.2f}x{b['height']:.2f} px (default font {b['fallback']:.2f} px wide)")
    server.shutdown()
    for failure in failures:
        print("FAIL", failure)
    print("PENNY-SHAPING:", "FAIL" if failures else "PASS")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
