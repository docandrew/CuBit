"""Reject vacuous/skipped success for the arrangement helper."""
import json
from pathlib import Path

if not __debug__:
    raise SystemExit("Proof audit requires assertions")
result = json.loads((Path(__file__).parent / "build/gnatprove/cubit-display_arrangement.spark").read_text())
assert result["spark"] and all(value == "all" for value in result["spark"].values())
assert result["progress"] == "PROGRESS_PROOF"
assert result["stop_reason"] == "STOP_REASON_NONE"
for field in ("pragma_assume", "skip_flow_proof", "skip_proof"):
    assert not result[field], field
assert result["proof"]
assert all(item["severity"] == "info" for item in result["proof"])
assert any(item["rule"] == "VC_POSTCONDITION" for item in result["proof"])
print(f"PASS arrangement proof: {len(result['proof'])} diagnostics, no skips or assumptions")
