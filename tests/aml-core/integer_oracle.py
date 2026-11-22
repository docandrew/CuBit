#!/usr/bin/env python3
"""Generate boundary vectors using unbounded Python arithmetic, then modulo.
Run through Nix. The Ada fixture runs these through AML_Execute, not Apply alone.
"""
from pathlib import Path
values = [0, 1, 2, 31, 32, 33, 63, 64, 65, 2**31-1, 2**31, 2**32-1, 2**32, 2**32+1,
          2**63-1, 2**63, 2**64-2, 2**64-1]
ops = [(0x72, lambda a,b:a+b), (0x74, lambda a,b:a-b),
       (0x77, lambda a,b:a*b), (0x7b, lambda a,b:a&b),
       (0x7d, lambda a,b:a|b), (0x7f, lambda a,b:a^b),
       (0x7c, lambda a,b:~(a&b)), (0x7e, lambda a,b:~(a|b)),
       (0x80, lambda a,b:~a),
       (0x81, lambda a,b:a.bit_length()),
       (0x82, lambda a,b:(a & -a).bit_length()),
       (0x79, lambda a,b:0 if b >= width else a << b),
       (0x7a, lambda a,b:0 if b >= width else a >> b),
       (0x78, lambda a,b:a//b), (0x85, lambda a,b:a%b)]
path = Path(__file__).parent / 'build' / 'integer-oracle.txt'
path.parent.mkdir(exist_ok=True)
with path.open('w') as f:
    for width in (32,64):
        modulus = 2**width
        for op, apply in ops:
            for a in values:
                for b in ([0] if op in (0x80, 0x81, 0x82) else values):
                    if op in (0x78, 0x85) and b == 0:
                        continue
                    # External argument values remain raw; normalize results.
                    # In particular, do not truncate shift counts or discard
                    # high source bits before a right shift.
                    answer = apply(a, b) % modulus
                    f.write(f'{width} {op} {a} {b} {answer}\n')
