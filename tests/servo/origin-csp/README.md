# Parent-origin and CSP integration fixture

Run `serve.py --requests /tmp/penny-origin-requests.log` in the Nix shell.
Open `http://10.0.2.2:18471/parent` in the disposable CuBit guest. Enable
`/servo/perf-check` to log `CuBitBrowserPerfOriginPASS` or `CuBitBrowserPerfOriginFAIL`.
The parent requires both the unprotected control and the CSP-allowed child to
report exactly its origin through `location.ancestorOrigins`. Execution of the
CSP-denied child fails the test. After 12 seconds the parent reports completion.
Also require requests to all three child paths and a recorded CSP violation for
`/deny`; absence of a message alone does not prove CSP enforcement.

For distinct sites without public DNS, set `--child-bind` to a local interface
address and `--child-origin` to `http://ADDRESS:18472`. The default uses distinct
ports on the same site. The server serves only these embedded fixture pages.
The allow case uses `frame-ancestors *`; CSP host matching intentionally rejects
non-loopback literal IPv4 host sources, so an explicit guest-gateway IP is not
a valid substitute for the wildcard allow case.

This validates ancestry and CSP behavior, not the navigation race by itself.
Both the old and fixed builds passed static cross-port/distinct-site cases.
The actual crash was captured separately during Wikipedia/htmx navigation,
reloads and window resizing, at WindowProxy's active-parent assertion.
