#!/usr/bin/env python3
"""Build embedded mesh/SPIR-V for the shared hosted/native Vulkan probe."""
import importlib.util
from pathlib import Path
import struct
import subprocess
import sys
root = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location('mesh', root/'build-mesh.py')
mesh = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mesh)
out = Path(sys.argv[1]).resolve()
gallery = len(sys.argv) == 3 and sys.argv[2] == '--gallery'
if len(sys.argv) > 2 and not gallery:
    raise SystemExit('usage: build-assets.py OUTPUT [--gallery]')
out.mkdir(parents=True, exist_ok=True)
subprocess.run([sys.executable,str(root/'build-mesh.py'),str(out)],check=True)
vertices, _ = mesh.mesh(12)
header = ['#include <stdint.h>',
          f'#define CUBIT_TEAPOT_ASSET_GALLERY {int(gallery)}',
          'static const float teapot_vertices[][6] = {']
header += ['{'+','.join(f'{x:.9e}f' for x in v)+'},' for v in vertices]
header += ['};']
for suffix,name in (('vert','vertex'),('frag','fragment')):
    binary = out/f'teapot.{suffix}.spv'
    shader = ('gallery' if gallery else 'teapot') + '.' + suffix
    subprocess.run(['glslangValidator','-V','--target-env','vulkan1.0',str(root/shader),'-o',str(binary)],check=True)
    subprocess.run(['spirv-val','--target-env','vulkan1.0',str(binary)],check=True)
    data=binary.read_bytes()
    words=struct.unpack('<'+'I'*(len(data)//4),data)
    header += [f'static const uint32_t teapot_{name}[] = {{']
    header += [','.join(f'0x{w:08x}' for w in words[i:i+8])+',' for i in range(0,len(words),8)]
    header += ['};']
(out/'teapot-assets.h').write_text('\n'.join(header)+'\n')
