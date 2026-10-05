"""Native keyboard/toolbar regression for the dynamic logical-tab bridge."""
import time

def check_dynamic_tabs(text, wait, expect, key, navigate, toggle_layout, command, capture, record):
    for tab_id in range(2, 66):
        expect(f"CUBITSHELL-BROWSER: tab new {tab_id}", lambda: key("ctrl-t"))
    wait("CUBITSHELL-TABS: logical=65 frontend_views=2")
    expect("CUBITSHELL-BROWSER: tab select 1", lambda: key("ctrl-tab"))
    expect("CUBITSHELL-BROWSER: tab select 65", lambda: key("ctrl-shift-tab"))
    expect("CUBITSHELL-BROWSER: title CuBitBrowserA", lambda: navigate("/browser-a"))
    wait("CUBITSHELL-TABS: logical=65 frontend_views=3")
    blank = "CUBITSHELL-BROWSER: tab state 64 url=about:blank"
    previous = text().count(blank)
    expect("CUBITSHELL-BROWSER: tab select 64", lambda: key("ctrl-shift-tab"))
    wait(blank, previous)
    expect("CUBITSHELL: reload", lambda: key("ctrl-r"))
    loaded = "CUBITSHELL-BROWSER: tab state 65 url=http://10.0.2.2:18470/browser-a"
    previous = text().count(loaded)
    expect("CUBITSHELL-BROWSER: tab select 65", lambda: key("ctrl-tab"))
    wait(loaded, previous)
    # Engine callbacks precede software presentation. Allow settling for visual
    # inspection; this delay is not a rendering-completion or latency oracle.
    time.sleep(5)
    command(f'screendump "{capture.with_name(capture.stem + "-65-tabs.ppm")}"')
    expect("CUBITSHELL-BROWSER: viewport 608x524", toggle_layout)
    time.sleep(5)
    command(f'screendump "{capture.with_name(capture.stem + "-65-vertical.ppm")}"')
    expect("CUBITSHELL-BROWSER: tab select 1", lambda: key("ctrl-tab"))
    expect("CUBITSHELL-BROWSER: tab select 65", lambda: key("ctrl-shift-tab"))
    expect("CUBITSHELL-BROWSER: viewport 800x494", toggle_layout)
    expect("CUBITSHELL-BROWSER: tab close 65", lambda: key("ctrl-w"))
    wait("CUBITSHELL-BROWSER: tab parked 65")
    expect("CUBITSHELL-BROWSER: tab new 66", lambda: key("ctrl-t"))
    # A retired engine container may be reused; its logical ID must not be.
    expect("CUBITSHELL-BROWSER: tab close 66", lambda: key("ctrl-w"))
    for tab_id in range(64, 1, -1):
        expect(f"CUBITSHELL-BROWSER: tab close {tab_id}", lambda: key("ctrl-w"))
        wait(f"CUBITSHELL-BROWSER: tab parked {tab_id}")
    expect("CUBITSHELL-BROWSER: tab new 67", lambda: key("ctrl-t"))
    expect("CUBITSHELL-BROWSER: tab select 1", lambda: key("ctrl-tab"))
    expect("CUBITSHELL-BROWSER: tab select 67", lambda: key("ctrl-tab"))
    expect("CUBITSHELL-BROWSER: tab close 67", lambda: key("ctrl-w"))
    record("dynamic-tabs-pass", peak_logical_tabs=65, last_id=67,
           empty_view_isolation=True, orientations=2)
