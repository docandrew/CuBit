# logstore architecture: today and the road to enterprise scale

Status as of 2026-10-03. The goal is an enterprise-grade log store, able to
hold petabytes and ingest from many nodes, with CuBit's standards of
correctness and security.

## Today

| Path | How | Cost |
| --- | --- | --- |
| Publish | One IPC per record. The record sits in a lent page; logstore copies a snapshot and decodes it. | 1 round trip per record |
| Admission | Per-pool token buckets (`Log_Budgets`) answer `Rate_Limited`. The minimum level answers `Below_Minimum`, and publishers drop those records before IPC once they learn the minimum. | none for filtered records |
| Fan-out | `Log_Fanout` (SPARK): a 512-record replay history, and one queue per subscriber that is filtered when an event is published. A slow reader loses its oldest events and is told how many. | 1 copy per interested subscriber |
| Read | **Stream rings** (`CuBit.Log_Streams`). The reader lends a 64 KiB region; logstore keeps it mapped and writes its events into a `Datagram_Rings` ring. Reading takes no IPC, and one Subscribe call every 10 s renews the lease. | 0 IPC per record |
| Identity | Each event carries `Node` (16 bytes) and the source pid, both stamped by the ingesting logstore and never by the publisher. | |

Proved: the policy gates, the broker's queues and leases, the ring index
discipline (`Channel_Rings`), and the stream entry codec (`Log_Streams`, level 2).
Tested: the codec round trip through a ring that wraps many times, and the
native Logs app and CCL `logs.recent` on QEMU.

## Back pressure

Nothing ever slows a producer down; diagnostics are shed instead.

- Budgets refuse with `Rate_Limited`.
- A publisher has one record in flight.
- `CuBit.Log`'s 32-record queue drops when full.
- Every loss is counted and reported.

This is deliberate: a log call must never stall a driver.

Publisher rings (next) make a choice possible for each stream. A full ring is
the signal, and the stream declares how to answer it:

- **Shed** (the default for diagnostics): drop and count, as today.
- **Wait** (audit and transactional logs): the producer waits on the ring's
  doorbell until logstore frees room. This is real back pressure, opted into and
  capability-gated, because a waiting producer is a producer logstore can stall.

## Next steps, in order

1. **Publisher rings.** Each publisher lends a ring. logstore consumes it, with a
   doorbell only when the ring goes from empty to non-empty. Publishing costs no
   IPC while logstore keeps up. The ring also carries batching, and the
   shed/wait policy above.
2. **Sequence numbers.** Each event gets a per-node offset, as `(node, offset)`.
   Readers can resume from an offset. Replication can deduplicate by it.
3. **Persistence** (docs/filesystem-journaling-decision.md): records stay in a
   RAM FIFO until the filesystem is up, then go to append-only segment files.
   Each segment gets an index from time to offset and from source to offset,
   block checksums and block compression. Writes are ordered through JBD2
   data=ordered.
4. **Retention.** Oldest-first eviction, both per stream and in total (the user's
   rule), as quotas in bytes and in time.
5. **Query.** Filter by time range, source, node, level, and text through an
   index. Live queries become cursors over the ring/segment boundary. CCL
   built-ins come over the same cursors.
6. **Multi-node.**
   - Node identities: 16 bytes, from the node's installation identity.
   - Ingestion from remote logstores over TLS with mutual authentication. The
     receiving logstore keeps the sender's node.
   - Aggregation and replication.
7. **Scale-out.** Shards by node and time, tiered storage, parallel scans.

Track at each step:
- Ingest records per second.
- Publish latency at p50/p99.
- How far each reader lags.
- Loss counts.

Compare each step against the previous one (A/B, tests/net-bench style).
