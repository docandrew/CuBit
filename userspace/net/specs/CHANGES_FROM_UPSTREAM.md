# Changes from upstream RecordFlux specifications

The specifications in this directory come from AdaCore's
[RecordFlux](https://github.com/AdaCore/RecordFlux), `examples/specs/` at
tag `v0.26.0` (commit `857ac277e2c41a28f86efc23c0c5c47f2f3dce56`),
Apache-2.0. See `../NOTICE`.

**All specifications are currently byte-identical to upstream.** A change
must be listed here (what, why, and the date) and marked in the file's
header comment, and the checksum below updated.

Upstream SHA-256 sums (the byte-identical files):

```
924eee8b83d7af505014eff1c82adce8fcc5ae644857d41ade65f6f6f7895be6  arp.rflx
90ded7552da14240639bd22af670bf0ab1ae0653bc50243dfe35c1187cf3f781  ethernet.rflx
7484f8f618c364ab63561667812297671e188627830bc9a5fbfb2b676bf5b739  icmp.rflx
b59dca0f034db95e9aa1dd77593f5843dcb2fcec3818cf1e571a8e6093e2cb63  ipv4.rflx
6c1d47ee9b39532bf65706329d0a15bb6215026dc2709fc2c2c9de92568dc1e5  protocol_numbers.rflx
b82b09d873614ef57d5d66dfdf193891e21692e972decdffb572d6690336fa38  tcp.rflx
3ad4cee803b91b039a33caf1934e57a77a6748b7634ee58666f64fd9798ac372  udp.rflx
```

Notes on CuBit's use (not changes to the files):

- `ipv4.rflx` declares a header checksum that RecordFlux 0.26.0 cannot
  generate for Ada; it is generated with `--ignore-unsupported-checksum`
  and the checksum is verified by netstack's own code.
- Netstack's earlier generated units (2026-03) came from locally modified
  specifications that were never committed; these upstream ones replace
  them. The differences: upstream TCP models the reserved bits and options
  (MSS, window scale, SACK-permitted, SACK, timestamps), upstream IPv4
  computes the options size from the header, and three deprecated
  protocol-number names are spelled `Deprecated_*`.
