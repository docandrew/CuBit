"""Native loaded-tab retirement requests while the browser stays alive.

The final pipeline/memory trace must be inspected separately; request markers
are not evidence that asynchronous resources have already been reclaimed.
"""
import time


def check_retirement(text, wait, expect, key, navigate, record):
    next_id = 2
    for cycle in range(1, 3):
        opened = []
        for _ in range(3):
            tab_id = next_id
            next_id += 1
            expect(f"CUBITSHELL-BROWSER: tab new {tab_id}", lambda: key("ctrl-t"))
            expect("CUBITSHELL-BROWSER: title CuBitBrowserA", lambda: navigate("/browser-a"))
            opened.append(tab_id)
        # Closing backwards leaves tab1 alive throughout. Every excess ready
        # container must request retirement rather than remain in the pool.
        for position, tab_id in enumerate(reversed(opened)):
            expect(f"CUBITSHELL-BROWSER: tab close {tab_id}", lambda: key("ctrl-w"))
            wait(f"CUBITSHELL-BROWSER: tab parked {tab_id}")
            if position:
                wait(f"CUBITSHELL-TABS: retire-request id={tab_id}")
        trace_start = len(text())
        record("retirement-idle-start", cycle=cycle)
        time.sleep(15)
        record("retirement-idle-end", cycle=cycle, serial_trace=text()[trace_start:])

    # Keep a shared blank alive, then close the original root page. This tests
    # the formerly hidden strong reference in main, not just new tab containers.
    expect(f"CUBITSHELL-BROWSER: tab new {next_id}", lambda: key("ctrl-t"))
    expect(f"CUBITSHELL-BROWSER: tab new {next_id + 1}", lambda: key("ctrl-t"))
    # The first new tab consumes the ready pool; the second uses the blank.
    expect(f"CUBITSHELL-BROWSER: tab close {next_id + 1}", lambda: key("ctrl-w"))
    expect(f"CUBITSHELL-BROWSER: tab close {next_id}", lambda: key("ctrl-w"))
    wait(f"CUBITSHELL-BROWSER: tab parked {next_id}")
    # A live blank must coexist with a ready real container, so navigate the
    # reused container first and create its blank sibling before closing it.
    expect(f"CUBITSHELL-BROWSER: tab new {next_id + 2}", lambda: key("ctrl-t"))
    expect("CUBITSHELL-BROWSER: title CuBitBrowserA", lambda: navigate("/browser-a"))
    expect(f"CUBITSHELL-BROWSER: tab new {next_id + 3}", lambda: key("ctrl-t"))
    expect(f"CUBITSHELL-BROWSER: tab select {next_id + 2}", lambda: key("ctrl-shift-tab"))
    expect(f"CUBITSHELL-BROWSER: tab close {next_id + 2}", lambda: key("ctrl-w"))
    wait(f"CUBITSHELL-BROWSER: tab parked {next_id + 2}")
    expect("CUBITSHELL-BROWSER: tab select 1", lambda: key("ctrl-tab"))
    expect("CUBITSHELL-BROWSER: tab close 1", lambda: key("ctrl-w"))
    wait("CUBITSHELL-TABS: retire-request id=1")
    expect("CUBITSHELL: reload", lambda: key("ctrl-r"))
    trace_start = len(text())
    record("root-retirement-idle-start")
    time.sleep(15)
    record("retirement-functional-pass", cycles=2, loaded_tabs=7,
           root_retirement_requested=True, serial_trace=text()[trace_start:])
