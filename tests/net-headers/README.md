# TCP header codec against RecordFlux

`TCP_Header` (`userspace/net/src`) is the codec netstack uses for the TCP
header on its data path. `specs/tcp.rflx` stays the specification; this
Linux-hosted test runs the proved codec and the RecordFlux parser generated
from that specification on the same segments:

- 20,000 well-formed segments (random fields, NOPs, MSS, window scale,
  SACK-permitted, timestamps, an unknown option kind, random data), each
  with 8 one-byte mutations and 8 truncations;
- 20,000 random byte strings of 0 to 80 bytes.

Whenever RecordFlux accepts a segment, the codec must too, with every field
equal. The codec may accept a segment RecordFlux rejects only if the same
segment with its option bytes replaced by NOPs passes RecordFlux (option
contents are `TCP_Wire`'s job). Any other difference fails the test.

```sh
nix develop -c bash -c 'cd tests/net-headers &&
  gprbuild -p -P net_headers_tests.gpr && ./build/check/differential &&
  gprbuild -p -P net_headers_tests.gpr -XMODE=speed && ./build/speed/differential'
```

`MODE=check` (default) builds with assertions, `MODE=speed` as netstack is
built (no assertions), for the timing line.

Result, 2026-09-26: 360,000 segments, 234,863 accepted by both with equal
fields, 125,137 rejected by both, none accepted by only one. Parse time for
a 1,480-byte segment, `MODE=speed`: RecordFlux 879 ns, codec 10 ns.

The codec's own properties (each field is its wire bytes; writing then
parsing gives back what was written; writing touches only the header) are
proved at level 1 in `tests/net-tcp`. `net.ads` here is a stand-in parent
for the generated packages (netstack has its own).
