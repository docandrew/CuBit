"""Test-only recovery oracle; never accepts a production artifact as fault-injected."""
from mesa_activation import BOOT, START, CLIENT
TEXT_FALLBACK = 'desktop: retained software text active'
# Any cause: startup, runtime switch or a declined client draw.
CLIENT_FALLBACK = 'desktop: software rendering ('
PARTIAL = 'desktop: text batch failed; repainting scene in software'
def expected_markers(manifest):
    fault = manifest.get('build_variant', {}).get('software_fault')
    if fault not in ('init', 'draw', 'text-partial'):
        raise ValueError('Explicit injected software fault required')
    return (CLIENT, TEXT_FALLBACK, CLIENT_FALLBACK) + ((PARTIAL,) if fault == 'text-partial' else ())
def verify(trace, manifest):
    if manifest.get('status') != 'LINKED' or manifest.get('software_renderer') != 'mesa-softpipe':
        raise ValueError('Linked unified Mesa fault artifact required')
    for marker in ('USER-MEMORY-FAULT', 'EXCEPTION:', 'TEST: FAIL', 'KERNEL PANIC',
                   'DESKTOP-VULKAN: startup=READY', 'DESKTOP-VULKAN: frame=',
                   'desktop: text completion uncertain; restarting', 'MESA-WINDOW: FAIL'):
        if marker in trace: raise ValueError('Unexpected fault result: ' + marker)
    for marker in (BOOT, START, *expected_markers(manifest)):
        if marker not in trace: raise ValueError('Missing recovery evidence: ' + marker)
    if trace.count(START) != 1 or not trace.index(BOOT) < trace.index(START) < trace.index(TEXT_FALLBACK):
        raise ValueError('Recovery outside selected renderer lifetime')
    return {'status': 'PASS', 'injected_fault': manifest['build_variant']['software_fault'],
            'recovery': 'retained CPU text and client fallback',
            'scope': 'Native failure/recovery markers; separate pixel/input checks required'}
