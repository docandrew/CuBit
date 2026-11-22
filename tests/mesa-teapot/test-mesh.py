#!/usr/bin/env python3
import importlib.util
import math
import itertools
from pathlib import Path
import struct

spec = importlib.util.spec_from_file_location('teapot', Path(__file__).with_name('build-mesh.py'))
m = importlib.util.module_from_spec(spec)
spec.loader.exec_module(m)
patches, points = m.source_data()
for patch in patches:
    for u,v,index in ((0,0,0),(0,1,3),(1,0,12),(1,1,15)):
        assert m.sample(points,patch,u,v) == points[patch[index]]
for steps in (2, 8, 12, 32):
    vertices, skipped = m.mesh(steps)
    assert len(vertices)//3 + skipped == 32*steps*steps*2
    assert len(vertices) % 3 == 0 and vertices
    assert all(math.isfinite(x) for v in vertices for x in v)
    assert all(abs(sum(x*x for x in v[3:])-1) < 1e-10 for v in vertices)
    assert all(-3.6 <= v[0] <= 3.6 and -2.1 <= v[1] <= 2.1 and 0 <= v[2] <= 3.16 for v in vertices)
    # Reflection symmetry, including rounded floating point evaluation noise.
    positions = {tuple(round(x*1e6) for x in v[:3]) for v in vertices}
    # Opposite parameter orders can straddle a rounding boundary. Require a
    # matching sample within one microunit per axis, not exact rounded equality.
    neighbors = tuple(itertools.product((-1,0,1), repeat=3))
    assert all(any((x+dx,-y+dy,z+dz) in positions for dx,dy,dz in neighbors)
               for x,y,z in positions)
    assert min(v[0] for v in vertices) < -2.9  # handle
    assert max(v[0] for v in vertices) > 3.3   # spout
    for v in vertices:
        assert len(struct.pack('<6f',*v)) == 24
    print(f'PASS subdivisions={steps} triangles={len(vertices)//3} poles-skipped={skipped}')
for bad in (0,1,33,1000000):
    try:
        m.mesh(bad)
    except ValueError:
        pass
    else:
        raise AssertionError('unbounded mesh accepted')
