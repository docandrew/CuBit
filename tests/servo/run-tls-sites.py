#!/usr/bin/env python3
"""Penny's TLS against real servers (WEB-004), hosted cubitshell.

    run-tls-sites.py [--app HOSTED_CUBITSHELL]

Each site is loaded in cubitshell's compat mode (one process for all) and
the page reports its origin, title and text. A site that must work shows
its own content; one that must be refused (badssl.com's broken
certificates) must not. The sites cover TLS 1.3 and 1.2 servers, RSA
2048/4096/8192 and ECDSA P-256/P-384 certificates, and the groups and
suites large sites negotiate (X25519MLKEM768 at Google and Cloudflare).
The local handshake matrix (tests/servo/tls-provider) covers every
suite x group x key type exactly; this checks the real world.
"""
import argparse
import json
import os
import pathlib
import re
import subprocess
import sys
import tempfile

ROOT = pathlib.Path(__file__).resolve().parents[2]
LOAD_DEADLINE_MS = 30000
CERTIFICATE_ERROR_TITLE = "Certificate error"
SETTLE_MS = 1500
REPORT = ("JSON.stringify({origin: location.origin, title: document.title.slice(0, 80), "
          "text: (document.body ? document.body.innerText : '').replace(/\\s+/g, ' ').slice(0, 160)})")
# (name, URL, expectation): ok = the site's own page (its title contains the
# word given); refused = no page from that origin.
SITES = [
    ("rsa2048", "https://rsa2048.badssl.com/", ("ok", "rsa2048")),
    ("rsa4096", "https://rsa4096.badssl.com/", ("ok", "rsa4096")),
    ("ecc256", "https://ecc256.badssl.com/", ("ok", "ecc256")),
    ("ecc384", "https://ecc384.badssl.com/", ("ok", "ecc384")),
    ("tls12", "https://tls-v1-2.badssl.com:1012/", ("ok", "tls-v1-2")),
    ("mozilla-modern", "https://mozilla-modern.badssl.com/", ("ok", "mozilla-modern")),
    ("google", "https://www.google.com/", ("ok", "Google")),
    ("cloudflare", "https://www.cloudflare.com/", ("ok", "Cloudflare")),
    ("wikipedia", "https://en.wikipedia.org/wiki/Transport_Layer_Security", ("ok", "Transport Layer Security")),
    ("expired", "https://expired.badssl.com/", ("refused", None)),
    ("wrong-host", "https://wrong.host.badssl.com/", ("refused", None)),
    ("self-signed", "https://self-signed.badssl.com/", ("refused", None)),
    ("untrusted-root", "https://untrusted-root.badssl.com/", ("refused", None)),
    ("sha1-intermediate", "https://sha1-intermediate.badssl.com/", ("refused", None)),
    # Expired since 2022-2024 (badssl.com): refused, after their RSA-8192 and
    # SHA-384/512-signed chains are parsed.
    ("rsa8192-expired", "https://rsa8192.badssl.com/", ("refused", None)),
    ("sha384-expired", "https://sha384.badssl.com/", ("refused", None)),
    ("sha512-expired", "https://sha512.badssl.com/", ("refused", None)),
]


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--app", default=str(ROOT / "userspace/rust/build/servo-work/target-hosted/"
                                             "x86_64-unknown-linux-gnu/release/cubitshell"))
    options = parser.parse_args()
    plan = "".join(f"load\t{name}\t{LOAD_DEADLINE_MS}\t{url}\nwait\t{SETTLE_MS}\njs\treport\t{REPORT}\n"
                   for name, url, _ in SITES)
    with tempfile.TemporaryDirectory(prefix="penny-tls-sites-") as directory:
        flags = pathlib.Path(directory)
        for name, value in {"compat": "", "log-level": "error", "compat-probe": "window.__probe = true;",
                            "compat-plan": plan}.items():
            (flags / name).write_text(value)
        completed = subprocess.run([options.app], env=dict(os.environ, PENNY_FLAGS=str(flags)),
                                   capture_output=True, text=True, timeout=len(SITES) * 60 + 120)
    log = completed.stdout + completed.stderr
    failures = []
    for name, url, (expect, word) in SITES:
        chunks = re.findall(rf"PENNY-COMPAT js-ok {re.escape(name)} report \d+/\d+ (.*)", log)
        report = json.loads("".join(chunks)) if chunks else {"origin": None, "title": "", "text": ""}
        host = re.match(r"https://([^/]+)", url).group(1)
        # Servo's certificate error page keeps the URL's origin; its title says.
        own = (report["origin"] or "").startswith(f"https://{host.split(':')[0]}") and \
            report["title"] != CERTIFICATE_ERROR_TITLE
        if expect == "ok":
            ok = own and word.lower() in (report["title"] + " " + report["text"]).lower()
        else:
            ok = not own
        print(f"{'PASS' if ok else 'FAIL'} {name} ({expect}): origin {report['origin']!r} title {report['title']!r} "
              f"text {report['text'][:80]!r}")
        if not ok:
            failures.append(name)
    print("PENNY-TLS-SITES:", "FAIL " + ", ".join(failures) if failures else "PASS")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
