"""Execution evidence only; combine with artifact identity and pixel checks.

Markers come from successful Desktop Draw_Text/Draw_Output results. This
oracle does not measure presentation latency or prove hardware execution.
"""
BOOT = 'DESKTOP-BOOTSTRAP: released; renderer may start'
START = 'DESKTOP-VULKAN: startup=SOFTWARE'
TEXT = 'desktop: Mesa retained-mask text active'
SURFACE = 'desktop: Mesa imported-surface compositor active'
CLIENT = 'MESA-WINDOW: PASS 9 frames with retired-buffer reuse'
FORBIDDEN = (
    'DESKTOP-VULKAN: startup=READY', 'DESKTOP-VULKAN: frame=',
    'desktop: CPU text fallback active', 'desktop: retained software text active',
    'desktop: text batch failed; repainting scene in software',
    'desktop: Mesa unavailable; CPU compositor fallback',
    'desktop: text completion uncertain; restarting',
    'desktop Mesa FFI:', 'MESA-WINDOW: FAIL',
    'MESA-WINDOW: surface unavailable; exiting',
    'USER-MEMORY-FAULT', 'EXCEPTION:', 'TEST: FAIL', 'KERNEL PANIC',
)

def verify(trace, manifest):
    if manifest.get('status') != 'LINKED' or manifest.get('software_renderer') != 'mesa-softpipe':
        raise ValueError('Artifact is not a linked unified software-Mesa build')
    for marker in FORBIDDEN:
        if marker in trace:
            raise ValueError('Unexpected failure/fallback: ' + marker)
    for marker in (BOOT, START, TEXT, SURFACE, CLIENT):
        if trace.count(marker) != 1:
            raise ValueError('Expected exactly one execution marker: ' + marker)
    if not trace.index(BOOT) < trace.index(START) < min(trace.index(TEXT), trace.index(SURFACE)):
        raise ValueError('Mesa draw occurred outside initialized renderer lifetime')
    return {'status': 'PASS', 'renderer': 'mesa-softpipe',
            'text_draw': True, 'imported_surface_draw': True,
            'client_retired_buffer_frames': 9, 'hardware_validated': False,
            'scope': 'Successful native Mesa draw and client self-check markers; pixel checks required separately'}

def verify_client_pixels(frame, cursor=(78, 158, 97, 186)):
    """Find the unscaled frame-8 quadrant buffer; compare every visible texel.

    The native fixture self-checks BGRA storage before attaching it. This
    independent screen oracle verifies Desktop's sampling/copy into scanout.
    Only the known native cursor rectangle is excluded from the comparison.
    """
    from PIL import Image, ImageChops
    expected = Image.new('RGB', (512, 384))
    for box, color in (((0, 0, 256, 192), (255, 0, 0)),
                       ((256, 0, 512, 192), (0, 255, 0)),
                       ((0, 192, 256, 384), (0, 0, 255)),
                       ((256, 192, 512, 384), (255, 255, 255))):
        expected.paste(color, box)
    frame = frame.convert('RGB')
    needle = bytes((255, 0, 0)) * 256 + bytes((0, 255, 0)) * 256
    matches = []
    # Sample an interior row, away from the expected pointer at y=160.
    for top in range(frame.height - 384 + 1):
        row = frame.crop((0, top + 191, frame.width, top + 192)).tobytes()
        start = 0
        while (at := row.find(needle, start)) >= 0:
            start = at + 1
            if at % 3:
                continue
            left = at // 3
            difference = ImageChops.difference(
                frame.crop((left, top, left + 512, top + 384)), expected)
            x0, y0, x1, y1 = cursor
            clipped = (max(0, x0-left), max(0, y0-top),
                       min(512, x1-left), min(384, y1-top))
            if clipped[0] < clipped[2] and clipped[1] < clipped[3]:
                difference.paste((0, 0, 0), clipped)
            if difference.getbbox() is None:
                matches.append([left, top, 512, 384])
    if len(matches) != 1:
        raise ValueError('Expected exactly one intact native client quadrant image')
    return {'status': 'PASS', 'client_rectangle': matches[0],
            'cursor_exclusion': list(cursor), 'scale': [1, 1]}
