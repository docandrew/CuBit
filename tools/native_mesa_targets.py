"""CuBit combined engine archives, excluding unused OS-loader/video/test targets."""
from pathlib import Path

GALLIUM = {
    'src/gallium/drivers/softpipe/libsoftpipe.a',
    'src/gallium/auxiliary/libgallium.a',
    'src/gallium/winsys/sw/null/libws_null.a',
}
REQUIRED = GALLIUM | {
    'src/intel/vulkan/libvulkan_intel.a',
    'src/mesa/libmesa.a',
    'src/mesa/glapi/glapi/libglapi_bridge.a',
    'src/mesa/glapi/shared-glapi/libglapi.a',
}

def archives(targets, build):
    build = Path(build).resolve()
    selected = set()
    for target in targets:
        if target['type'] != 'static library':
            continue
        for name in target['filename']:
            path = Path(name).resolve()
            if not path.is_relative_to(build) or path.suffix != '.a':
                raise ValueError('invalid native static archive target')
            relative = path.relative_to(build).as_posix()
            if relative in GALLIUM or relative.startswith((
                'src/intel/', 'src/vulkan/', 'src/mesa/', 'src/compiler/',
                'src/util/', 'src/c11/')):
                selected.add(path)
    missing = REQUIRED - {p.relative_to(build).as_posix() for p in selected}
    if missing:
        raise ValueError('missing required combined engine archives: ' + repr(sorted(missing)))
    return sorted(selected)
