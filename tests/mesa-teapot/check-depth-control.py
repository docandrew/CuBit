#!/usr/bin/env python3
"""Compare real Mesa frames from identical draws with/without depth testing.

This is not a CPU rasterizer or a universal golden-image oracle. It checks that
depth changes visibility rather than letting a shader-only smoke test pass.
"""
from pathlib import Path
import sys


def pixels(path):
    data=Path(path).read_bytes()
    header=b'P6\n256 256\n255\n'
    if not data.startswith(header) or len(data)!=len(header)+256*256*3:
        raise ValueError('invalid rendered image shape')
    return data[len(header):]


def compare(a,b):
    changed=sum(a[i:i+3]!=b[i:i+3] for i in range(0,len(a),3))
    # The default background comes from a clear and must remain unchanged.
    bg=a[:3]
    changed_background=sum(a[i:i+3]==bg and b[i:i+3]!=bg for i in range(0,len(a),3))
    if changed<1000 or changed_background:
        raise ValueError(f'ineffective/invalid depth control: changed={changed}, background={changed_background}')
    return changed


if __name__=='__main__':
    a,b=pixels(sys.argv[1]),pixels(sys.argv[2])
    count=compare(a,b)
    try:
        compare(a,a)
    except ValueError:
        pass
    else:
        raise AssertionError('unchanged frame incorrectly passed depth control')
    print(f'Depth negative control PASS: {count} pixels differ, cleared background unchanged; identical control rejected')
