#!/usr/bin/env python3
"""Offline asset generation, not rasterization or a GPU driver.

Emit a triangle-list vertex buffer: six little-endian float32 values per vertex
(XYZ position, XYZ unit face normal), plus a manifest. The source license must
accompany the generated assets. Vulkan will perform transformation/rasterization.
"""
import argparse
import hashlib
import json
import math
from pathlib import Path
import re
import struct


def source_data():
    text = Path(__file__).with_name('teapot-control-points.h').read_text()
    def array(name):
        part = text.split(name, 1)[1].split('=', 1)[1].split('};', 1)[0]
        return re.sub(r'/\*.*?\*/', '', part, flags=re.S)
    indices = [int(n) for n in re.findall(r'\d+', array('patchdata_teapot'))]
    values = [float(n) for n in re.findall(r'(-?\d+\.\d+)f', array('cpdata_teapot'))]
    if len(indices) != 160 or len(values) != 129 * 3:
        raise ValueError('unexpected upstream geometry shape')
    points = [tuple(values[i:i+3]) for i in range(0, len(values), 3)]
    if not all(0 <= i < len(points) for i in indices):
        raise ValueError('out-of-range control point')
    return [indices[i:i+16] for i in range(0, 160, 16)], points


def basis(t):
    return ((1-t)**3, 3*t*(1-t)**2, 3*t*t*(1-t), t**3)


def sample(points, patch, u, v):
    a, b = basis(u), basis(v)
    return tuple(sum(a[i]*b[j]*points[patch[4*i+j]][axis]
                     for i in range(4) for j in range(4)) for axis in range(3))


def transform(p, copy, rotate):
    x, y, z = p
    if not rotate:
        return x, y if copy == 0 else -y, z
    return ((x,y,z), (-y,x,z), (-x,-y,z), (y,-x,z))[copy]


def mesh(steps):
    if not 2 <= steps <= 32:
        raise ValueError('subdivisions must be 2..32')
    patches, points = source_data()
    vertices, skipped = [], 0
    for number, patch in enumerate(patches):
        rotate = number < 6
        for copy in range(4 if rotate else 2):
            grid = [[transform(sample(points, patch, u/steps, v/steps), copy, rotate)
                     for v in range(steps+1)] for u in range(steps+1)]
            for u in range(steps):
                for v in range(steps):
                    a,b,c,d = grid[u][v],grid[u][v+1],grid[u+1][v+1],grid[u+1][v]
                    for triangle in ((a,b,c), (a,c,d)):
                        if not rotate and copy:
                            triangle = tuple(reversed(triangle))
                        p,q,r = triangle
                        e = [q[i]-p[i] for i in range(3)]
                        f = [r[i]-p[i] for i in range(3)]
                        n = (e[1]*f[2]-e[2]*f[1], e[2]*f[0]-e[0]*f[2], e[0]*f[1]-e[1]*f[0])
                        length = math.sqrt(sum(x*x for x in n))
                        # Collapsed pole triangles have no normal or area.
                        if length < 1e-12:
                            skipped += 1
                            continue
                        normal = tuple(x/length for x in n)
                        vertices.extend(p + normal for p in triangle)
    return vertices, skipped


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('output', type=Path)
    parser.add_argument('--subdivisions', type=int, default=12)
    args = parser.parse_args()
    vertices, skipped = mesh(args.subdivisions)
    data = b''.join(struct.pack('<6f', *v) for v in vertices)
    args.output.mkdir(parents=True, exist_ok=True)
    (args.output / 'teapot.vertices').write_bytes(data)
    source = Path(__file__).with_name('teapot-control-points.h')
    (args.output / 'teapot-control-points.h').write_bytes(source.read_bytes())
    manifest = dict(stride=24, position_offset=0, normal_offset=12,
                    vertices=len(vertices), triangles=len(vertices)//3,
                    subdivisions=args.subdivisions, expanded_patches=32,
                    skipped_degenerate_triangles=skipped,
                    source_sha256=hashlib.sha256(source.read_bytes()).hexdigest(),
                    vertex_sha256=hashlib.sha256(data).hexdigest(),
                    rendered=False)
    (args.output / 'mesh.json').write_text(json.dumps(manifest, indent=2)+'\n')
    print(json.dumps(manifest))


if __name__ == '__main__':
    main()
