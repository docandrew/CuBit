"""Playwright 1.61.1's Node driver asserts that every Firefox "WebSocket
opened" event follows a request and response it saw; with its Firefox build
(firefox-1532) some sites open WebSockets that violate this, the assertion
kills the driver, and every page in that browser is lost. The survey drives
a copy of the driver package where such an event is skipped instead (the
WebSocket is then missing from Playwright's own bookkeeping only)."""
import os
import pathlib
import shutil
import stat

ASSERTING = ("const request2 = this._webSocketRequests.get(event.requestId);\n"
             "        assert(request2);\n"
             "        const response2 = this._webSocketResponses.get(event.requestId);\n"
             "        assert(response2);")
TOLERANT = ("const request2 = this._webSocketRequests.get(event.requestId);\n"
            "        const response2 = this._webSocketResponses.get(event.requestId);\n"
            "        if (!request2 || !response2) return;")


def install(cache):
    """Point Playwright at the patched driver copy in cache (made once)."""
    from playwright._impl import _driver, _transport
    from playwright._repo_version import version
    node, cli = _driver.compute_driver_executable()
    source = pathlib.Path(cli).resolve().parent
    target = pathlib.Path(cache) / f"playwright-core-{version}-websocket-fix"
    if not (target / "cli.js").exists():
        # Engines start at once: each stages its own copy, the first rename wins.
        staging = target.with_name(f"{target.name}.partial-{os.getpid()}")
        shutil.rmtree(staging, ignore_errors=True)
        shutil.copytree(source, staging, symlinks=True)
        bundle = staging / "lib/coreBundle.js"
        bundle.chmod(bundle.stat().st_mode | stat.S_IWUSR)
        text = bundle.read_text()
        if text.count(ASSERTING) != 1:
            raise RuntimeError("Playwright's WebSocket handler changed; review playwright_fix.py")
        bundle.write_text(text.replace(ASSERTING, TOLERANT))
        try:
            staging.rename(target)
        except OSError:
            if not (target / "cli.js").exists():
                raise
            shutil.rmtree(staging, ignore_errors=True)
    patched = (node, str(target / "cli.js"))
    _driver.compute_driver_executable = lambda: patched
    _transport.compute_driver_executable = lambda: patched
