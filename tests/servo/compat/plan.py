"""WEB-001 compatibility survey: the steps every engine runs for a site.

One definition for all engines: the Playwright driver (drive_reference.py)
executes these steps directly; cubitshell (hosted and on CuBit) gets them as
its tab-separated plan (ports/cubitshell/src/compat.rs in Penny's Servo fork).
"""
import json
import pathlib

HERE = pathlib.Path(__file__).resolve().parent
PROBE = (HERE / "probe.js").read_text()
COLLECT = " ".join(line.strip() for line in (HERE / "collect.js").read_text().splitlines()
                   if line.strip() and not line.strip().startswith("//"))

# The page has this long to fire its load event; then the survey records it
# as not loaded and carries on with whatever has rendered.
LOAD_DEADLINE_MS = 30000
# After load (or the deadline), late scripts, fonts and images get this long
# before the summary and the first screenshot. A fixed wait, the same in
# every engine.
SETTLE_MS = 3000
# After each interaction, before checking its effect.
STEP_SETTLE_MS = 1500
WHEEL_NOTCHES = 5
# One notch down, in CSS pixels (compat.rs sends Servo's equivalent, -76:
# Servo scrolls by the inverse of the wheel delta).
WHEEL_NOTCH_PIXELS = 76
TYPED_TEXT = "cubit"

# Interaction checks: each script returns a JSON string.
SCROLL_CHECK = ("(() => { let inner = 0; for (const e of document.querySelectorAll('*')) "
                "{ if (e.scrollTop > inner) inner = e.scrollTop; } "
                "return JSON.stringify({ok: (window.scrollY || 0) > 0 || inner > 0, "
                "window: Math.round(window.scrollY || 0), inner: Math.round(inner)}); })()")
TYPED_CHECK = ("(() => { let e = document.activeElement; "
               "while (e && e.shadowRoot && e.shadowRoot.activeElement) e = e.shadowRoot.activeElement; "
               "const value = e && 'value' in e ? String(e.value) : ''; "
               f"return JSON.stringify({{ok: value.includes('{TYPED_TEXT}'), "
               "tag: e ? e.tagName : null, value: value.slice(0, 40)}); })()")


def locate(selector):
    """A script that scrolls the first visible match into view and returns
    its centre as "x,y" in CSS pixels, or why nothing can be clicked."""
    quoted = json.dumps(selector)
    return ("(() => { const all = [...document.querySelectorAll(" + quoted + ")]; "
            "if (!all.length) return 'missing'; "
            "const shown = (e) => { const r = e.getBoundingClientRect(); return r.width >= 1 && r.height >= 1; }; "
            "const e = all.find(shown); if (!e) return 'hidden'; "
            "e.scrollIntoView({block: 'center'}); const r = e.getBoundingClientRect(); "
            "return Math.round(r.left + r.width / 2) + ',' + Math.round(r.top + r.height / 2); })()")


def menu_check(script):
    """The site's own success test, wrapped to return a JSON string."""
    return f"(() => {{ let ok = false; try {{ ok = !!({script}); }} catch (e) {{}} return JSON.stringify({{ok}}); }})()"


def steps(site):
    """The steps for one site entry from sites.json."""
    out = [
        {"do": "load", "site": site["id"], "url": site["url"], "deadline_ms": LOAD_DEADLINE_MS},
        {"do": "wait", "ms": SETTLE_MS},
        {"do": "js", "name": "collect", "script": COLLECT},
        {"do": "shot", "name": "top"},
    ]
    for action in site.get("interactions", ["scroll"]):
        if action == "scroll":
            out += [{"do": "wheel", "notches": WHEEL_NOTCHES}, {"do": "wait", "ms": STEP_SETTLE_MS},
                    {"do": "js", "name": "scroll", "script": SCROLL_CHECK},
                    {"do": "shot", "name": "scrolled"}]
        elif action["kind"] == "menu":
            out += [{"do": "click", "name": "menu", "script": locate(action["target"])},
                    {"do": "wait", "ms": STEP_SETTLE_MS},
                    {"do": "js", "name": "menu", "script": menu_check(action["check"])},
                    {"do": "shot", "name": "menu"}]
        elif action["kind"] == "type":
            out += [{"do": "click", "name": "focus", "script": locate(action["target"])},
                    {"do": "wait", "ms": STEP_SETTLE_MS // 3},
                    {"do": "keys", "text": TYPED_TEXT},
                    {"do": "wait", "ms": STEP_SETTLE_MS},
                    {"do": "js", "name": "typed", "script": TYPED_CHECK},
                    {"do": "shot", "name": "typed"}]
        else:
            raise ValueError(f"{site['id']}: unknown interaction {action}")
    return out


def cubitshell_plan(sites):
    """The tab-separated plan cubitshell's compat mode reads."""
    lines = []
    for site in sites:
        for step in steps(site):
            kind = step["do"]
            if kind == "load":
                fields = ["load", step["site"], str(step["deadline_ms"]), step["url"]]
            elif kind == "wait":
                fields = ["wait", str(step["ms"])]
            elif kind in ("js", "click"):
                fields = [kind, step["name"], step["script"]]
            elif kind == "wheel":
                fields = ["wheel", str(step["notches"])]
            elif kind == "keys":
                fields = ["keys", step["text"]]
            else:
                fields = ["shot", step["name"]]
            for field in fields:
                if "\t" in field or "\n" in field:
                    raise ValueError(f"plan field with a tab or newline: {field[:60]}")
            lines.append("\t".join(fields))
    return "\n".join(lines) + "\n"


def load_sites(path=HERE / "sites.json", only=None, groups=None):
    data = json.loads(pathlib.Path(path).read_text())
    sites = [s for s in data["sites"] if (groups is None or s["group"] in groups)
             and (only is None or s["id"] in only)]
    return data, sites
