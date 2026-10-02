# Hosted GGTT mapping client tests

Run in the Nix development shell:

```sh
gprbuild -P tests/intel-gpu/ggtt_mapping.gpr
for scenario in $(seq 0 20); do
    tests/intel-gpu/build-ggtt-mapping/ggtt_mapping_tests "$scenario"
done
```

Each process tests the production `Intel_GPU_GGTT_Mapping` body with fresh
one-shot state. The local `CuBit.Messages` fixture supplies only its transport
surface. Fixture syscall numbers are symbolic test values, not the kernel ABI.
It asserts exact request shape, endpoint, mapping addresses, chunk lengths and
writable mode; it never accesses MMIO. A second call must never submit or map.

Cases 0/19/20 cover 8/2/4 MiB success; 1/2 changed physical address/length;
3 denial; 4 explicit startup-busy then success; 5 failed submission;
6 second mapping failure with the first retained; 7 unavailable clock;
8 regressing clock; 9 stalled clock bounded by poll count; 10 elapsed timeout;
11 completion failure; 12 malformed tag; 13 nonzero trailing word;
14 wrong completion token; 15/16 missing owner/reset; 17 wrapping BAR;
18 unsupported table size.

This tests client control flow, not kernel capability enforcement, real IPC
layout, the native broker's identity checks, MMIO cache attributes or hardware
reset. Native build and live hardware tests remain separate requirements.
