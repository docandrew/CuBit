"""Validate the actual native service against the RAM-backed bootstrap fixture.

This is IPC/mapping evidence, not Intel hardware or rendering emulation.
"""
import pathlib
import sys

text = pathlib.Path(sys.argv[1]).read_text(errors="replace")
expected_forcewake = sys.argv[2] if len(sys.argv) > 2 else "grant-denied"
assert expected_forcewake in ("grant-denied", "acquire-timeout")
start = text.index("devmgr: Intel RAM fixture (NOT hardware)")
ready = text.index("intel-gpu: read-only register mapping ready; firmware scanout retained", start)
assert "intel-gpu: GGC=000000C0 GGTT table bytes 8388608" in text[start:]
firmware = text.index("intel-gpu: snapshot FIRMWARE_POWER_CONTROL=00000000", ready)
driver = text.index("intel-gpu: snapshot DRIVER_POWER_CONTROL=00000000", firmware)
assert "desktop: display info ready" in text[driver:]
assert "intel-gpu: snapshot publication losses 0" in text[driver:]
assert ("boot-logs: intel-gpu: read-only snapshot; firmware=00000000 "
        "driver=00000000; scanout retained") in text[driver:]
assert f"boot-logs: intel-gpu: forcewake={expected_forcewake}" in text
assert "boot-logs: intel-gpu: file=LOADED bytes= 335360" in text
assert "boot-logs: intel-gpu: ggtt= 8388608; first=0000000012345001 present= 4 scanned= 1048576" in text
assert "boot-logs: intel-gpu: firmware buffer prepared-retained (NOT GPU-published)" in text
assert f"intel-gpu: forcewake {expected_forcewake}" in text[driver:]
assert "devmgr: Intel firmware read scope installed" in text
assert "intel-gpu: firmware file LOADED bytes 335360 code 334976 (NOT authenticated or uploaded)" in text
assert "devmgr: Intel GGTT inspection page granted read-only" in text
assert "intel-gpu: GGTT read-only first=0000000012345001 present= 4 scanned= 1048576" in text
if expected_forcewake == "acquire-timeout":
    assert "devmgr: Intel RAM forcewake page granted (NOT hardware)" in text
assert "devmgr: Intel forcewake page granted" not in text
for failure in ("intel-gpu: bootstrap rejected", "intel-gpu: resource rejected", "intel-gpu: read-only mapping denied"):
    assert failure not in text, failure
assert "intel-gpu: snapshot unavailable" not in text
print("PASS: native Intel bootstrap, RAM snapshot, authorized logstore/viewer delivery and desktop survival (not hardware)")
