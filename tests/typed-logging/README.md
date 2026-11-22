# Typed logging: hosted tests and flow demo

From the repository root:

```sh
nix develop -c bash tests/typed-logging/run.sh
```

No VM or live service is involved. The assertion-enabled Linux test includes:

- 9,234 round trips: every text length 0–512, six severities and three timebases,
  with maximum-width timestamp/domain values and cleared unused buffer tails.
- An independently specified little-endian wire vector and a UTF-8 round trip.
- All provided wire lengths 0–544 against a one-byte record, malformed headers,
  invalid timestamp combinations and every possible single text byte.
- All 65,536 two-byte combinations checked against the UTF-8/ASCII rules.
- UTF-8 boundary cases, overlong encodings, surrogates, incomplete sequences,
  invalid scalar ranges and control characters.
- Chunk-split CRLF and UTF-8, exact capacity, oversize rejection/recovery, empty
  lines, EOF flushing, bare CR and incomplete-code-point reporting.
- Upstream gap reports, conservative newline resynchronization and EOF recovery
  without splicing together text from opposite sides of a loss.

The second executable moves actual encoded records through an in-process typed
adapter and two binding ADTs, exercising a collector change while keeping the
old route active until commit. It is not an IPC security test: authority evidence,
resource readiness and peer identities are trusted fixture inputs. This runner
does not boot the live log service or rebuild an ISO. The separate
[native logging regression](../log-fanout/README.md#native-integration) covers
the grant-backed service integration.

The final runner phase proves the portable codec/adapter's initialization and
runtime-safety obligations with GNATprove. No Assume or SPARK-Off escapes are
used. Behavioral correctness of UTF-8 and encode/decode inversion is tested,
not claimed formally proved. Full wire and scope: [design](../../docs/typed-logging.md).
