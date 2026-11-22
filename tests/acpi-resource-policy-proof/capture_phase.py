"""Run one proof phase with the maintained bounded log collector."""
from pathlib import Path
import signal
import sys

root = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(root / "tests/acpi-field-remote"))
from hosted_capture import capture, CaptureInterrupted

def interrupted(signum, frame):
    raise CaptureInterrupted("Client proof phase interrupted")

signal.signal(signal.SIGTERM, interrupted)
signal.signal(signal.SIGINT, interrupted)
status = capture(sys.argv[2:], root, Path(sys.argv[1]), 180, 1024 * 1024)
raise SystemExit(status if isinstance(status, int) else 124 if status == "timeout" else 125)
