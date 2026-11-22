#!/usr/bin/env python3
"""Write the WEB-001 breakdown pages into a directory, deterministically.

    make_pages.py <directory>

- article.html: a text article of about ARTICLE_BYTES (headings, paragraphs,
  lists, quotes, tables, code), the daily-reading case;
- css-stress.html: CARD_COUNT boxes with border-radius, box-shadow and
  gradients, the paint/raster case;
- js-benchmark.html/.js: tests/servo's checked JavaScript workloads.

Page text uses one served web font (IBM Plex Sans, IBM_PLEX_SANS_FONT, as in
Penny's bundled faces), so CuBit and Linux shape and rasterize the same face.
The article and stress pages echo keys into a fixed box (the input-latency
path through script, style, layout and paint) and report Navigation Timing in
the title (CuBitBrowserPerfNav ...), which Penny logs with perf-check.
"""
import os
import pathlib
import shutil
import sys

ARTICLE_BYTES = 200 * 1024
CARD_COUNT = 2400
# A fixed linear congruential generator (Numerical Recipes constants): the
# same pages on every host and run.
LCG_MULTIPLIER = 1664525
LCG_INCREMENT = 1013904223
LCG_MODULUS = 2 ** 32
SEED = 20261118

WORDS = """the of and to in is that for it as was with be by on not he this are or his
from at which but have an they you were her she there been one all we their has would
when if so no what up out into more its time only about other some could them these two
may then first any like now my such over our even most made after also did many before
must through back years where much your way well down should because each just those
people how too little state good very make world still own see men work long here get
both between life being under never day same another know while last might us great old
year off come since against go came right used take three himself few house use during
without again place around however home small found thought went say part once general
high upon school every don't does got united left number course war until always away
something fact though water less public put think almost hand enough far took head yet
government system better set told nothing night end why called didn't eyes find going
look asked later knew point next program city business give group toward young days let
room president side social given present several order national possible rather second
face per among form important often things looking early white case john become large big
need four within felt along children saw best church ever least power development light
""".split()

HEAD = """<!doctype html>
<html lang="en"><head><meta charset="utf-8">
<title>{title}</title>
<style>
@font-face {{ font-family: "Penny Bench"; src: url("plex-sans.ttf"); }}
@font-face {{ font-family: "Penny Bench Mono"; src: url("plex-mono.ttf"); }}
body {{ font: 18px/1.6 "Penny Bench"; color: #1b1f24; background: #fbfaf7; margin: 0; }}
code, pre {{ font-family: "Penny Bench Mono"; font-size: 15px; }}
#keys {{ position: fixed; top: 8px; right: 8px; width: 220px; min-height: 24px; padding: 4px 8px;
  background: #fff3c4; border: 1px solid #c9a227; overflow: hidden; white-space: nowrap; }}
{style}
</style></head>
<body>
<div id="keys">keys:</div>
"""

TAIL = """<script>
// Key echo: each key changes the DOM (script, style, layout, display list, paint).
document.addEventListener('keydown', event => {
  const box = document.getElementById('keys');
  box.textContent = (box.textContent + event.key).slice(-24);
});
// Navigation Timing in milliseconds from the navigation start, logged by Penny.
addEventListener('load', () => setTimeout(() => {
  const entry = performance.getEntriesByType && performance.getEntriesByType('navigation')[0];
  const ms = value => Math.round(value);
  document.title = entry
    ? 'CuBitBrowserPerfNav rq=' + ms(entry.requestStart) + ' rs=' + ms(entry.responseStart) +
      ' re=' + ms(entry.responseEnd) + ' di=' + ms(entry.domInteractive) +
      ' dcl=' + ms(entry.domContentLoadedEventEnd) + ' ls=' + ms(entry.loadEventStart)
    : 'CuBitBrowserPerfNav unavailable';
}, 0));
</script>
</body></html>
"""


class Random:
    def __init__(self, seed):
        self.state = seed

    def next(self, bound):
        self.state = (self.state * LCG_MULTIPLIER + LCG_INCREMENT) % LCG_MODULUS
        return (self.state >> 8) % bound


def sentence(rng):
    words = [WORDS[rng.next(len(WORDS))] for _ in range(6 + rng.next(14))]
    text = " ".join(words)
    if rng.next(5) == 0:
        index = rng.next(len(words))
        text = text.replace(words[index], f"<a href=\"#s{rng.next(50)}\">{words[index]}</a>", 1)
    if rng.next(6) == 0:
        index = rng.next(len(words))
        text = text.replace(words[index], f"<em>{words[index]}</em>", 1)
    return text[0].upper() + text[1:] + "."


def paragraph(rng):
    return "<p>" + " ".join(sentence(rng) for _ in range(3 + rng.next(5))) + "</p>\n"


def article():
    rng = Random(SEED)
    style = ("main { max-width: 44em; margin: 0 auto; padding: 32px 24px; }\n"
             "h1 { font-size: 40px; line-height: 1.2; } h2 { font-size: 28px; margin-top: 2em; }\n"
             "blockquote { border-left: 4px solid #8aa; margin: 1em 0; padding: 0 1em; color: #445; }\n"
             "table { border-collapse: collapse; width: 100%; } td, th { border: 1px solid #ccd; padding: 4px 8px; }\n"
             "pre { background: #eef1f4; padding: 12px; overflow: auto; }")
    parts = [HEAD.format(title="Penny article", style=style),
             "<main>\n<h1>A long article for the Penny breakdown</h1>\n"]
    section = 0
    while sum(len(part) for part in parts) < ARTICLE_BYTES:
        parts.append(f"<h2 id=\"s{section}\">Section {section + 1}: {sentence(rng)[:-1]}</h2>\n")
        for _ in range(3 + rng.next(4)):
            parts.append(paragraph(rng))
        kind = rng.next(4)
        if kind == 0:
            parts.append("<ul>" + "".join(f"<li>{sentence(rng)}</li>" for _ in range(4)) + "</ul>\n")
        elif kind == 1:
            parts.append(f"<blockquote><p>{sentence(rng)} {sentence(rng)}</p></blockquote>\n")
        elif kind == 2:
            rows = "".join(f"<tr><td>{WORDS[rng.next(len(WORDS))]}</td><td>{rng.next(10000)}</td>"
                           f"<td>{rng.next(100)}.{rng.next(100):02}</td></tr>" for _ in range(6))
            parts.append(f"<table><tr><th>Item</th><th>Count</th><th>Share</th></tr>{rows}</table>\n")
        else:
            lines = "\n".join(f"let {WORDS[rng.next(len(WORDS))]}_{i} = {rng.next(1000)};" for i in range(5))
            parts.append(f"<pre><code>{lines}</code></pre>\n")
        section += 1
    parts.append("</main>\n")
    parts.append(TAIL)
    return "".join(parts)


def css_stress():
    rng = Random(SEED + 1)
    style = (".grid { display: flex; flex-wrap: wrap; gap: 12px; padding: 16px; }\n"
             ".card { width: 150px; height: 96px; border-radius: 14px; padding: 8px; box-sizing: border-box;\n"
             "  box-shadow: 0 2px 6px rgba(0,0,0,.25), 0 8px 18px rgba(20,40,80,.18); color: #fff; font-size: 14px; }\n"
             ".dot { width: 28px; height: 28px; border-radius: 50%; float: right;\n"
             "  background: radial-gradient(circle at 30% 30%, #fff, rgba(255,255,255,0) 70%); }")
    cards = []
    for index in range(CARD_COUNT):
        hue_a = rng.next(360)
        hue_b = (hue_a + 40 + rng.next(120)) % 360
        angle = rng.next(360)
        cards.append(f"<div class=\"card\" style=\"background: linear-gradient({angle}deg, "
                     f"hsl({hue_a},70%,45%), hsl({hue_b},70%,35%))\"><div class=\"dot\"></div>"
                     f"Card {index}</div>")
    return (HEAD.format(title="Penny CSS stress", style=style) + "<div class=\"grid\">\n" +
            "\n".join(cards) + "\n</div>\n" + TAIL)


def main():
    out = pathlib.Path(sys.argv[1])
    out.mkdir(parents=True, exist_ok=True)
    (out / "article.html").write_text(article())
    (out / "css-stress.html").write_text(css_stress())
    servo_tests = pathlib.Path(__file__).resolve().parents[1]
    for name in ("js-benchmark.html", "js-benchmark.js"):
        shutil.copyfile(servo_tests / name, out / name)
    fonts = pathlib.Path(os.environ["IBM_PLEX_SANS_FONT"]).parent
    shutil.copyfile(fonts / "IBMPlexSans-Regular.ttf", out / "plex-sans.ttf")
    shutil.copyfile(fonts / "IBMPlexMono-Regular.ttf", out / "plex-mono.ttf")


if __name__ == "__main__":
    main()
