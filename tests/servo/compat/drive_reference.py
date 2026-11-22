#!/usr/bin/env python3
"""WEB-001 compatibility survey: the reference browsers.

    drive_reference.py --engine firefox|chromium --run N --out DIR
        --viewport WxH [--sites a,b] [--groups priority,tranco]

Playwright drives Firefox (Playwright's Gecko build) and the stock Chromium
(CHROMIUM_EXECUTABLE, over CDP). WebKit is drive_wpe.py: stock WPE WebKit
in its sandbox, not Playwright's patched build.
Each site gets a fresh browser context (cold cache, no cookies) with the
survey probe installed before page scripts, then the steps in plan.py, the
same as cubitshell runs. Records go to DIR/records.jsonl in the same shape
run-survey.py parses from cubitshell's log; screenshots are PNG files.
"""
import argparse
import asyncio
import json
import os
import pathlib
import sys
import time

from playwright.async_api import async_playwright

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
import plan  # noqa: E402
import playwright_fix  # noqa: E402

# A step that has not finished by then is abandoned (a hung page script).
STEP_DEADLINE_SECONDS = 15
# The references run as Playwright drives them (navigator.webdriver set,
# Chromium's headless user agent): sites that refuse automated browsers
# refuse these too, and such a site has no reference in the analysis.
CHROMIUM_ARGUMENTS = ["--no-first-run", "--disable-gpu"]
MAXIMUM_CONSOLE_LINES = 20
# Playwright's driver process died: no further step can run in this process.
DRIVER_GONE = "Connection closed while reading from the driver"


class DriverGone(Exception):
    pass
MAXIMUM_CONSOLE_CHARACTERS = 300


class Recorder:
    def __init__(self, path, engine, run):
        self.stream = path.open("a")
        self.engine = engine
        self.run = run

    def __call__(self, kind, site, name, text):
        self.stream.write(json.dumps({"engine": self.engine, "run": self.run, "kind": kind,
                                      "site": site, "name": name, "text": text}) + "\n")
        self.stream.flush()


async def launch(playwright, engine):
    if engine == "chromium":
        return await playwright.chromium.launch(
            executable_path=os.environ["CHROMIUM_EXECUTABLE"], args=CHROMIUM_ARGUMENTS)
    return await playwright.firefox.launch()


async def survey_site(browser, site, viewport, shots, record):
    context = await browser.new_context(viewport=viewport, device_scale_factor=1)
    await context.add_init_script(plan.PROBE)
    page = await context.new_page()
    console = []
    crashed = []
    console_count = [0]

    def on_console(message):
        if message.type == "error":
            console_count[0] += 1
            if len(console) < MAXIMUM_CONSOLE_LINES:
                console.append(message.text[:MAXIMUM_CONSOLE_CHARACTERS])

    page.on("console", on_console)
    page.on("crash", lambda _: crashed.append("page crashed"))
    sid = site["id"]
    gone = False
    try:
        for step in plan.steps(site):
            kind = step["do"]
            try:
                if kind == "load":
                    await load(page, step, record)
                elif kind == "wait":
                    await page.wait_for_timeout(step["ms"])
                elif kind == "js":
                    try:
                        value = await asyncio.wait_for(page.evaluate(step["script"]), STEP_DEADLINE_SECONDS)
                        record("js-ok", sid, step["name"], value if isinstance(value, str) else json.dumps(value))
                    except asyncio.TimeoutError:
                        record("js-error", sid, step["name"], "timeout")
                    except Exception as error:  # the page's own failure, recorded
                        record("js-error", sid, step["name"], str(error).splitlines()[0][:300])
                elif kind == "click":
                    await click(page, step, sid, record)
                elif kind == "wheel":
                    await page.mouse.move(viewport["width"] / 2, viewport["height"] / 2)
                    for _ in range(step["notches"]):
                        await page.mouse.wheel(0, plan.WHEEL_NOTCH_PIXELS)
                        await page.wait_for_timeout(50)
                elif kind == "keys":
                    await asyncio.wait_for(page.keyboard.type(step["text"], delay=30), STEP_DEADLINE_SECONDS)
                elif kind == "shot":
                    path = shots / f"{sid}-{step['name']}.png"
                    await asyncio.wait_for(page.screenshot(path=str(path), timeout=STEP_DEADLINE_SECONDS * 1000),
                                           STEP_DEADLINE_SECONDS * 2)
                    record("shot", sid, step["name"], f"{viewport['width']}x{viewport['height']}")
            except Exception as error:
                if DRIVER_GONE in str(error):
                    gone = True
                    break
                record(f"{kind}-failed", sid, step.get("name", "-"), str(error).splitlines()[0][:300])
                if crashed:
                    break
    finally:
        if gone:
            # Not finished: run-survey.py starts a new driver for it.
            raise DriverGone()
        record("console", sid, "count", str(console_count[0]))
        for index, line in enumerate(console):
            record("console", sid, str(index), line)
        if crashed:
            record("crash", sid, "-", crashed[0])
        try:
            await asyncio.wait_for(context.close(), STEP_DEADLINE_SECONDS)
        except Exception:
            pass


async def load(page, step, record):
    started = time.monotonic()
    loaded, error = True, None
    try:
        await page.goto(step["url"], wait_until="load", timeout=step["deadline_ms"])
    except Exception as failure:
        loaded = False
        error = str(failure).splitlines()[0][:300]
    elapsed = round((time.monotonic() - started) * 1000)
    record("load", step["site"], "-", json.dumps({
        "loaded": loaded, "load_ms": elapsed if loaded else None, "first_frame_ms": None,
        "crashed": False, "error": error}))


async def click(page, step, sid, record):
    try:
        answer = await asyncio.wait_for(page.evaluate(step["script"]), STEP_DEADLINE_SECONDS)
    except Exception as error:
        record("click-missed", sid, step["name"], str(error).splitlines()[0][:200])
        return
    try:
        x, y = (float(v) for v in str(answer).split(","))
    except ValueError:
        record("click-missed", sid, step["name"], str(answer)[:200])
        return
    await page.mouse.move(x, y)
    await page.wait_for_timeout(50)
    await page.mouse.click(x, y)
    record("click", sid, step["name"], f"{x:g},{y:g}")


async def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--engine", required=True, choices=["firefox", "chromium"])
    parser.add_argument("--run", type=int, required=True)
    parser.add_argument("--out", type=pathlib.Path, required=True)
    parser.add_argument("--viewport", required=True)
    parser.add_argument("--sites")
    parser.add_argument("--groups")
    parser.add_argument("--sites-file", default=str(plan.HERE / "sites.json"))
    options = parser.parse_args()
    width, height = (int(v) for v in options.viewport.split("x"))
    viewport = {"width": width, "height": height}
    _, sites = plan.load_sites(options.sites_file,
                               only=set(options.sites.split(",")) if options.sites else None,
                               groups=set(options.groups.split(",")) if options.groups else None)
    shots = options.out / "shots"
    shots.mkdir(parents=True, exist_ok=True)
    record = Recorder(options.out / "records.jsonl", options.engine, options.run)
    playwright_fix.install(options.out.parent.parent / "playwright-driver")
    async with async_playwright() as playwright:
        browser = await launch(playwright, options.engine)
        record("version", "-", "-", f"{options.engine} {browser.version}")
        for site in sites:
            if not browser.is_connected():
                browser = await launch(playwright, options.engine)
                record("relaunch", site["id"], "-", "browser disconnected")
            started = time.monotonic()
            try:
                await asyncio.wait_for(survey_site(browser, site, viewport, shots, record),
                                       plan.LOAD_DEADLINE_MS / 1000 + 120)
            except DriverGone:
                print(f"{options.engine}: Playwright driver died on {site['id']}", flush=True)
                sys.exit(3)
            except Exception as error:
                if DRIVER_GONE in str(error):
                    print(f"{options.engine}: Playwright driver died on {site['id']}", flush=True)
                    sys.exit(3)
                record("site-failed", site["id"], "-", str(error).splitlines()[0][:300] if str(error) else type(error).__name__)
            print(f"{options.engine} run {options.run} {site['id']}: {time.monotonic() - started:.1f}s", flush=True)
        await browser.close()
    record("done", "-", "-", "-")


if __name__ == "__main__":
    asyncio.run(main())
