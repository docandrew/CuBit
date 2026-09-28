# dns.svc: a verified resolver and authoritative server

Status: design (2026-09-26). Nothing here is implemented. Decided with the
user: DNS is its own service, not part of netstack, and it serves zones as
well as resolving (a verified alternative to BIND, whose history is
largely parsing and memory-safety bugs).

## Shape

- **One service, two roles.** A caching stub resolver for CuBit itself,
  and an authoritative server for zones configured in Config. It is not
  an open recursive resolver: it answers recursive queries only for local
  clients, so it cannot be used for reflection or amplification.
- **Authority.** dns.svc holds netstack capabilities for UDP and TCP to
  its configured upstreams on port 53, and a listening capability on port
  53 when it serves zones. It reads its settings and zones from Config
  (`dns.upstream.*`, `dns.zone.*`) and gets randomness from the entropy
  service. No filesystem access.
- **Clients.** Applications keep naming hosts (`@net:tcp:host:port`).
  Netstack asks dns.svc through a capability (RESOLVE name, type; the
  answer is addresses with TTLs) and keeps checking scopes by the name the
  application asked for, as today. The resolver can restart, or change
  transport (DNS over TLS through SPARKTLS later), without netstack
  changing.

## Parsing: nothing by hand except names

- **Messages:** header, question, resource records and EDNS(0) OPT
  (RFC 1035 4, RFC 6891) as RecordFlux specifications in
  `userspace/net/specs/dns.rflx`, written by us (RecordFlux 0.26.0's
  examples have no DNS). The generated parsers and serializers are proved
  free of runtime errors.
- **Names:** compression pointers (RFC 1035 4.1.4) do not fit a
  RecordFlux message. A hand-written SPARK decoder, with these proved:
  it terminates; it only follows pointers strictly backwards (so no loops,
  at most one pass over the message); labels are at most 63 octets and
  names at most 255; the result is the name's labels in order.
  Compression when writing is optional; we never emit forward pointers.

## Resolver

- **Queries.** Random 16-bit IDs and random source ports (entropy
  service); the answer must match the ID, the question name, type and
  class, and the upstream's address and port (RFC 5452).
- **Timeouts.** Retries with backoff across upstreams; fall back to TCP
  when an answer is truncated (TC).
- **Answers.** CNAME chains are followed, bounded to 8 links, only within
  the answer's bailiwick; out-of-bailiwick records are dropped.
- **Cache.** Bounded, with LRU eviction. TTLs are capped (1 day) and
  honoured exactly: an entry is never returned after it expires (proved).
  Negative answers are cached with the SOA minimum (RFC 2308).
- **EDNS.** A 1232-byte UDP buffer (the DNS flag day 2020 value), so
  answers are not fragmented.
- **Later:** DNSSEC validation, then DNS over TLS.

## Authoritative server

- **Zones** from Config as CCL records: A, AAAA, CNAME, NS, SOA, MX, TXT,
  PTR, SRV. Zones are validated when loaded: one SOA, NS present, no CNAME
  beside other data, names within the zone.
- **Answers:** exact matches, CNAME, NXDOMAIN or NODATA with the SOA
  (RFC 2308), and referrals for delegated subzones. It is authoritative
  only; the AA flag is set exactly for in-zone data.
- **Abuse limits:** responses are never larger than the query's EDNS
  buffer (or 512 bytes without EDNS); truncation sets TC. Response rate
  limiting per source prefix. No zone transfers at first (AXFR/IXFR
  later, over TCP, to listed secondaries only).
- **Proved:** every answer echoes the query's ID and question; a response
  never exceeds the client's buffer; answer records come from the
  configured zone and are within it.

## Tests

- Linux-hosted: the name decoder against crafted messages (pointer
  loops, forward pointers, 64-octet labels, 256-octet names) and a corpus
  of real responses; the resolver's state machine against a scripted fake
  upstream (loss, truncation, spoofed IDs, wrong questions, CNAME loops).
- Differential testing against a host resolver (unbound or dig) for the
  server's answers to the same zone.
- Native: netstack plus dns.svc resolving real names (Servo pages) and
  serving a test zone queried from the host.

## Order

1. `dns.rflx` and the name decoder, proved and fuzzed on the host.
2. The resolver's cache and its query/answer matching, proved; the
   service with UDP, then TCP fallback; netstack switches from its
   built-in lookup.
3. The authoritative server over Config zones.
4. DNSSEC, DNS over TLS, zone transfers.
