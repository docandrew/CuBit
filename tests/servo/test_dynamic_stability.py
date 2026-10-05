"""Ensure interaction evidence follows each cycle's stable logical tab ID."""
import copy
from check_stability import check
from test_stability_checker import events

dynamic = copy.deepcopy(events)
tab_id = None
for event in dynamic:
    if event["event"] == "cycle-start":
        tab_id = event["cycle"] + 1
        event["tab_id"] = tab_id
    elif event["event"] == "callback":
        marker = event["marker"]
        if marker.startswith("CUBITSHELL-BROWSER: tab ") and marker.endswith(" 2"):
            event["marker"] = marker[:-1] + str(tab_id)
assert check(dynamic)["complete_interaction_cycles"] == 3
for verb in ("new", "select", "close", "parked"):
    bad = copy.deepcopy(dynamic)
    target = next(e for e in bad if e.get("marker") == f"CUBITSHELL-BROWSER: tab {verb} 3")
    target["marker"] = f"CUBITSHELL-BROWSER: tab {verb} 2"
    try:
        check(bad)
    except AssertionError:
        continue
    raise AssertionError(f"accepted stale {verb} evidence")
print("PASS dynamic interaction IDs and four stale-ID negative controls")
