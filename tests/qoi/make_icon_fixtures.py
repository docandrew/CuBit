#!/usr/bin/env python3
"""A valid QOI image of the wrong size for Desktop's icon atlas
(icon_store_tests wrong-size): the atlas width, one largest-edge icon row
short."""
from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "tools"))
import generate_desktop_icons as icons  # noqa: E402
import qoi  # noqa: E402

out = Path(sys.argv[1])
out.mkdir(parents=True, exist_ok=True)
width, height = icons.ATLAS_WIDTH, icons.ATLAS_HEIGHT - max(icons.EDGES)
(out / "icons-wrong-size.qoi").write_bytes(qoi.encode(bytes(width * height * 4), width, height))
