#!/usr/bin/env python3
"""WEB-001 compatibility survey: build sites.json (committed, so every rerun
surveys the same pages).

    make_sites.py --tranco tranco-L5KW4-top1000.csv --resolved DIR/resolved.jsonl
        [--resolve] [--count 200]

Two groups:
- priority: the user's daily drivers, signed out (GitHub, X, Reddit,
  Facebook, then Wikipedia, YouTube, MDN, Stack Overflow, DigitalOcean,
  Amazon, BBC, NYTimes, Hacker News, the Rust and Ada docs), with scripted
  scrolling, a menu and typing into a login or search field where that needs
  no account.
- tranco: the pinned, dated Tranco list (TRANCO below), in rank order, one
  entry per site. A domain is a site only if the Firefox reference lands on
  a page there: --resolve loads https://<domain>/ and records the landing
  URL, HTTP status and visible text. Domains that serve no page (CDNs, DNS,
  APIs, trackers: an error, HTTP status 400 or higher, or under
  MINIMUM_SITE_TEXT characters) are skipped, as are domains landing on a site
  already listed (redirects and country domains: youtu.be, google.de) and the
  EXCLUDED adult, malware and suspicious domains. The first --count sites
  are kept.
"""
import argparse
import asyncio
import collections
import datetime
import json
import pathlib
import sys
import urllib.parse

HERE = pathlib.Path(__file__).resolve().parent

TRANCO = {
    "list": "L5KW4",
    "created": "2026-11-18",
    "window": "2026-10-20..2026-11-18",
    "configuration": "Dowdall over CrUX, Farsight, Majestic, Radar, Umbrella; pay-level domains",
    "source": "https://tranco-list.eu/list/L5KW4/1000000",
}
MINIMUM_SITE_TEXT = 50
RESOLVE_DEADLINE_MS = 25000
RESOLVE_SETTLE_MS = 2000
RESOLVE_PARALLEL = 4
# Second-level labels under country domains (example.co.uk is one site).
COUNTRY_SECOND_LEVELS = {"co", "com", "org", "net", "gov", "ac", "edu", "ne", "or"}

SCROLL = "scroll"


def menu(target, check='document.querySelector("[aria-expanded=\\"true\\"]")'):
    return {"kind": "menu", "target": target, "check": check}


def typing(target):
    return {"kind": "type", "target": target}


# Pinned pages where the site allows it (a fixed commit, issue, post).
PRIORITY = [
    ("github-repo", "https://github.com/servo/servo", [SCROLL, menu("header button[aria-expanded]")]),
    ("github-file", "https://github.com/servo/servo/blob/8319b662d03f884a3e874c54deea0f09bae669f3/"
     "components/servo/lib.rs", [SCROLL]),
    ("github-issue", "https://github.com/servo/servo/issues/12125", [SCROLL]),
    ("github-pr-diff", "https://github.com/servo/servo/pull/40526/files", [SCROLL]),
    ("github-search", "https://github.com/search?q=web+browser+engine&type=repositories", [SCROLL]),
    ("github-login", "https://github.com/login", [typing("#login_field")]),
    ("x-profile", "https://x.com/github", [SCROLL]),
    ("x-post", "https://x.com/github/status/__X_POST__", [SCROLL]),
    ("x-login", "https://x.com/i/flow/login", [typing('input[autocomplete~="username"]')]),
    ("reddit-front", "https://www.reddit.com/", [SCROLL, menu("#expand-user-drawer-button")]),
    ("reddit-thread", "__REDDIT_THREAD__", [SCROLL]),
    ("reddit-login", "https://www.reddit.com/login/", [typing("#login-username")]),
    ("facebook-login", "https://www.facebook.com/", [typing('input[name="email"]')]),
    ("facebook-page", "https://www.facebook.com/NASA", [SCROLL]),
    ("wikipedia", "https://en.wikipedia.org/wiki/Web_browser", [SCROLL, typing('input[name="search"]')]),
    ("youtube-home", "https://www.youtube.com/", [SCROLL, typing('input[name="search_query"]')]),
    ("youtube-watch", "https://www.youtube.com/watch?v=jNQXAC9IVRw", [SCROLL]),
    ("mdn", "https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array",
     [SCROLL]),
    ("stackoverflow", "https://stackoverflow.com/questions/11227809/"
     "why-is-processing-a-sorted-array-faster-than-processing-an-unsorted-array", [SCROLL]),
    ("digitalocean", "https://www.digitalocean.com/community/tutorials/"
     "how-to-install-nginx-on-ubuntu-22-04", [SCROLL]),
    ("amazon", "https://www.amazon.com/", [SCROLL, typing("#twotabsearchtextbox")]),
    ("bbc", "https://www.bbc.com/news", [SCROLL]),
    ("nytimes", "https://www.nytimes.com/", [SCROLL]),
    ("hackernews", "https://news.ycombinator.com/", [SCROLL]),
    ("rust-std", "https://doc.rust-lang.org/std/vec/struct.Vec.html", [SCROLL]),
    ("rust-book", "https://doc.rust-lang.org/book/ch04-01-what-is-ownership.html", [SCROLL]),
    ("ada-learn", "https://learn.adacore.com/courses/intro-to-ada/chapters/introduction.html", [SCROLL]),
    ("ada-rm", "https://ada-lang.io/docs/arm/AA-TOC", [SCROLL]),
]

# Kept out of the survey, with the reason (reviewed against the resolved
# titles; a domain that serves no page is skipped by rule, not listed here).
EXCLUDED = {}


# Playwright's own driver can die (an assertion in its Firefox WebSocket
# handling); such a domain is resolved again by the next --resolve.
DRIVER_FAILURES = ("Connection closed while reading from the driver", "Target page, context or browser has been closed")


DRIVER_FAILURE_LIMIT = 3


def driver_failure(error):
    return any(text in error for text in DRIVER_FAILURES)


def registrable(host):
    labels = host.lower().rstrip(".").split(".")
    if len(labels) >= 3 and len(labels[-1]) == 2 and labels[-2] in COUNTRY_SECOND_LEVELS:
        return ".".join(labels[-3:])
    return ".".join(labels[-2:])


def brand(domain):
    """google.com, google.de and google.co.in are one site."""
    labels = domain.split(".")
    if len(labels) >= 3 and labels[-2] in COUNTRY_SECOND_LEVELS:
        return labels[-3]
    return labels[-2] if len(labels) >= 2 else domain


async def resolve(domains, out_path):
    from playwright.async_api import async_playwright
    sys.path.insert(0, str(HERE))
    import playwright_fix
    playwright_fix.install(out_path.parent / "playwright-driver")
    done, failures = set(), collections.Counter()
    if out_path.exists():
        for line in out_path.read_text().splitlines():
            entry = json.loads(line)
            if driver_failure(entry.get("error", "")):
                failures[entry["domain"]] += 1
            else:
                done.add(entry["domain"])
    # A domain that was in flight whenever the driver died is given up; the
    # others in flight with it are retried after every clean domain.
    gave_up = {d for d, count in failures.items() if count >= DRIVER_FAILURE_LIMIT and d not in done}
    pending = sorted((d for d in domains if d not in done and d not in gave_up),
                     key=lambda d: (failures[d], domains.index(d)))
    gate = asyncio.Semaphore(RESOLVE_PARALLEL)
    stream = out_path.open("a")
    async with async_playwright() as playwright:
        launch = lambda: playwright.firefox.launch()
        holder = {"browser": await launch()}
        relaunch = asyncio.Lock()

        async def one(domain):
            async with gate:
                result = {"domain": domain}
                context = None
                try:
                    async with relaunch:
                        if not holder["browser"].is_connected():
                            holder["browser"] = await launch()
                    context = await holder["browser"].new_context(viewport={"width": 1280, "height": 800})
                    page = await context.new_page()
                    response = await page.goto(f"https://{domain}/", wait_until="load",
                                               timeout=RESOLVE_DEADLINE_MS)
                    await page.wait_for_timeout(RESOLVE_SETTLE_MS)
                    result.update(await page.evaluate(
                        "() => ({landing: location.href, title: document.title.slice(0, 100), "
                        "text: document.body ? document.body.innerText.replace(/\\s+/g, '').length : 0})"))
                    result["status"] = response.status if response else None
                except Exception as error:
                    result["error"] = (str(error).splitlines() or [type(error).__name__])[0][:200]
                if context is not None:
                    try:
                        await context.close()
                    except Exception:
                        pass
                stream.write(json.dumps(result) + "\n")
                stream.flush()
                print(json.dumps(result)[:160], flush=True)

        await asyncio.gather(*(one(d) for d in pending))
        await holder["browser"].close()


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--tranco", type=pathlib.Path, default=HERE / "tranco-L5KW4-top1000.csv")
    parser.add_argument("--resolved", type=pathlib.Path, required=True)
    parser.add_argument("--resolve", action="store_true")
    parser.add_argument("--candidates", type=int, default=450)
    parser.add_argument("--count", type=int, default=200)
    parser.add_argument("--x-post", required=True)
    parser.add_argument("--reddit-thread", required=True)
    options = parser.parse_args()
    ranked = [line.split(",")[1].strip() for line in options.tranco.read_text().splitlines() if line]
    candidates = ranked[:options.candidates]
    if options.resolve:
        asyncio.run(resolve(candidates, options.resolved))
    resolved = {}
    for line in options.resolved.read_text().splitlines():
        entry = json.loads(line)
        if entry["domain"] not in resolved or not driver_failure(entry.get("error", "")):
            resolved[entry["domain"]] = entry
    # Still a driver failure after the passes: Firefox could not be driven
    # there, so there is no reference.
    for entry in resolved.values():
        if driver_failure(entry.get("error", "")):
            entry["error"] = "the Playwright driver died on this site (no Firefox reference)"
    sites, skipped, brands = [], [], {}
    for rank, domain in enumerate(ranked, 1):
        if len(sites) >= options.count:
            break
        entry = resolved.get(domain)
        reason = None
        if domain in EXCLUDED:
            reason = EXCLUDED[domain]
        elif entry is None:
            reason = "not resolved"
        elif "error" in entry:
            reason = "no page: " + entry["error"][:80]
        elif (entry.get("status") or 0) >= 400:
            reason = f"no page: HTTP {entry['status']}"
        elif entry.get("text", 0) < MINIMUM_SITE_TEXT:
            reason = f"no page: {entry.get('text', 0)} characters"
        else:
            site = registrable(urllib.parse.urlsplit(entry["landing"]).hostname or domain)
            key = brand(site)
            if key in brands:
                reason = f"same site as {brands[key]}"
            else:
                brands[key] = domain
        if reason:
            skipped.append({"rank": rank, "domain": domain, "reason": reason})
            continue
        sites.append({"id": domain, "group": "tranco", "rank": rank, "url": f"https://{domain}/",
                      "landing": entry["landing"], "interactions": [SCROLL]})
    priority = []
    for sid, url, interactions in PRIORITY:
        url = url.replace("__X_POST__", options.x_post).replace("__REDDIT_THREAD__", options.reddit_thread)
        priority.append({"id": sid, "group": "priority", "url": url, "interactions": interactions})
    data = {"generated": datetime.date.today().isoformat(), "tranco": TRANCO,
            "rules": {"minimum_site_text": MINIMUM_SITE_TEXT, "reference": "Playwright Firefox"},
            "sites": priority + sites, "skipped": skipped}
    (HERE / "sites.json").write_text(json.dumps(data, indent=1) + "\n")
    print(f"{len(priority)} priority + {len(sites)} Tranco sites; {len(skipped)} domains skipped",
          file=sys.stderr)


if __name__ == "__main__":
    main()
