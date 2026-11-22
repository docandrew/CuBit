#!/usr/bin/env python3
"""Embed a supplied 640x360 WebM in the eight-cycle retirement/idle fixture."""
import argparse
import base64
import hashlib
import json
from pathlib import Path

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('video', type=Path)
parser.add_argument('--output', required=True, type=Path)
args = parser.parse_args()
video = args.video.read_bytes()
template = Path(__file__).with_name('retirement-idle.html').read_text()
assert template.count('__MEDIA_URL__') == 1
url = 'data:video/webm;base64,' + base64.b64encode(video).decode()
html = template.replace('__MEDIA_URL__', json.dumps(url))
assert html.lower().count('</script>') == 1
args.output.mkdir(parents=True, exist_ok=True)
(args.output / 'page.html').write_text(html)
(args.output / 'pages').write_text('data:text/html;base64,' + base64.b64encode(html.encode()).decode() + '\n')
(args.output / 'provenance.json').write_text(json.dumps({
    'video_sha256': hashlib.sha256(video).hexdigest(),
    'template_sha256': hashlib.sha256(template.encode()).hexdigest(),
    'cycles': 8, 'idle_timeout_ms': 60000,
    'scope': 'Normal playback/removal then navigation; no forced garbage collection',
}, indent=2) + '\n')
