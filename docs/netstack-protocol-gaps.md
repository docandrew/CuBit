# Netstack protocol gaps (IPv4 daily use)

Status: audit of 2026-09-27, read against the code (file:line references
are to that date). This is the working list for protocol completeness and
correctness, in priority order. Items are struck off as they are fixed,
with the fix's evidence (proof, hosted test, native test). IPv6 has its
own list in netstack-redesign.md ("IPv6 on the link").

Summary: the proved units (TCP state machine, RFC 5961 defences,
reassembly, RFC 6298 RTO, NewReno, window scaling, RFC 6528 ISN,
TIME-WAIT, the spoof-resistant ARP cache) are solid. The glue around
them assumes a static configuration on a QEMU-style LAN where every peer
is beyond the gateway.

## Fixed

- **2026-09-27 (items 3, 4, 6, 31).**
  - Every IPv4 send looks up its next hop in the route table and uses
    the ARP cache:
    - the peer itself when it is on a connected network, else the
      route's gateway;
    - on a miss, a request goes out (at most one per second per
      neighbour) and the frame is dropped;
    - an arriving answer fills in connections that opened unresolved,
      and resends waiting DNS questions.

    Opens and resolves no longer depend on the gateway being ARPed at
    configure. The gateway MAC is read from the cache, whichever ARP
    packet taught it.
  - DNS:
    - it tries three times within its 5 s budget (at 0, 1 and 3 s),
      alternating primary and secondary; netmgr now passes
      `net.dns.secondary`;
    - it accepts answers from either server;
    - the proved parser reports RCODE. NXDOMAIN and NODATA fail at once;
      SERVFAIL and REFUSED move to the next server;
    - with no server configured, it fails at once.
  - Native evidence (network-authority): "localhost" resolves through
    QEMU's forwarder, and a `.invalid` name fails in 1 ms, not 5 s.
    wget-https still resolves and fetches over the internet.
  - Not yet tested natively: failover to a silent primary. The test
    image has one DNS server, and per-test system settings need image
    support.

- **2026-09-27 (items 5, 13, and 19 for receiving).** ICMP errors about
  our packets now take effect. The proved `ICMPv4_Error` parses them and
  classifies them as too-big, hard or soft. netstack acts only when the
  quoted packet was ours: for TCP, a live connection's addresses and
  ports, and a sequence number in [SND.UNA, SND.NXT) (RFC 5927).
  - **Too big:** the connection's segment size falls to the reported
    MTU (never below 536), and the unacknowledged data is resent at once.
    This uses the proved `TCP_Flow.Path_MTU_Reduced`
    (`TCP_Congestion.Reduce_Segment_Size`), which keeps the congestion
    window: RFC 8201 says a smaller path is not congestion.
  - **Hard errors** (port or protocol unreachable, prohibited) end a
    connection attempt as Unreachable. A connected UDP channel is marked
    Unreachable.
  - **Soft errors** change nothing.
  - **Evidence:**
    - natively, a datagram to a closed host port draws QEMU's port
      unreachable, and the channel reports it;
    - the too-big path is proved and hosted-tested only, since QEMU's
      user network cannot produce one;
    - three mutants are rejected.
  - **Still open:** sending port unreachable for our own closed UDP ports
    (item 19), and soft-error reporting when retries run out.

- **2026-09-27 (item 10).** A segment for no connection is answered by
  the proved `TCP_Reset` rule (RFC 9293 3.10.7.1): this covers a SYN to
  a closed port, and anything but a plain SYN for an unknown connection.
  A reset is never answered. Resets are rate-limited (50 in a burst, one
  per millisecond after that) and never sent to zero, multicast or
  broadcast sources. Native evidence: once the guest closes its last
  listener, the host's connection to the forwarded port is refused within
  3 s (tests/network-authority/peer.py).

## P0: breaks daily use or security

1. DHCP does not work end to end. netstack's raw-open is a stub, netmgr's
   writes are not admitted, and port-68 datagrams are dropped; images
   ship static addresses.
2. The DHCP client is incomplete even once plumbed:
   - fixed xid and zero CHADDR;
   - no retransmission, no NAK restart;
   - no T1/T2 renewal, rebinding or expiry;
   - no DECLINE/RELEASE/INIT-REBOOT, option 55 or broadcast flag;
   - one DNS server kept; OFFERs not filtered by server ID.
3. The gateway is ARPed once at configure and never retried. A lost
   reply refuses every open forever. The gateway MAC is taken only from
   a reply, not from the cache.
4. There is no next-hop selection or ARP on the data path. TCP, UDP and
   DNS always send to the gateway MAC, so on-link peers work only if the
   router hairpins, and static routes do not affect TCP or UDP.
5. PMTU black hole: DF is set on TCP and UDP, ICMP "fragmentation needed"
   is ignored, and MSS is fixed at 1460.
6. DNS makes one attempt to one server. It has no retransmission, never
   uses the second server, and ignores RCODE (NXDOMAIN waits out the 5 s
   timeout).

## P1: important

7. ARP entries never age, and connections pin the peer MAC at open, so a
   gateway change is a permanent black hole.
8. There is no queue for packets waiting on ARP resolution.
9. Reconfiguring is unsafe:
   - routes are appended, never replaced;
   - the ARP cache is not flushed;
   - live connections switch source address.
10. There is no RST for segments to closed ports or unknown connections.
11. There is no persist timer or zero-window probe; a lost window update
    hangs the connection.
12. There is no TCP keepalive.
13. ICMP errors are not delivered to TCP or UDP. A connect to an
    unreachable host takes the whole SYN backoff.
14. There is no loopback: 127/8 and our own address work for ping only,
    and "localhost" goes to the upstream DNS server.
15. UDP is client-only: no fixed-port bind, no unconnected send/receive,
    no subnet broadcast or multicast.
16. Listeners are limited:
    - exact interface address only (no wildcard);
    - 16 listeners of backlog 16;
    - SYN cookies unused.
17. SACK and timestamps are not negotiated, although the proved units
    exist; there is no PAWS.
18. The DNS resolver is minimal:
    - no cache, no AAAA, no search domains, no static hosts;
    - TC is ignored;
    - names are limited to 32/64 bytes;
    - the first A record is taken whatever its owner.
19. There is no ICMP port-unreachable for closed UDP ports.

## P2: minor correctness and nice to have

20. A UDP checksum computing to 0 is sent as 0 (meaning "none"), not
    0xFFFF.
21. There is no fragmentation or reassembly. ICMP is sent with DF clear
    and ID 0 (RFC 6864).
22. Ephemeral ports are sequential and predictable (RFC 6056), and a
    4-tuple clash fails the connect.
23. Everything is hard-wired to interface 0.
24. There is no RFC 5227 address probe or conflict detection.
25. `OP_NET_ROUTE_ADD` does not validate its interface index or prefix
    (manager-only).
26. Echo requests to broadcast are answered.
27. Ping replies are matched by sequence number only.
28. There is no Nagle.
29. TIME-WAIT ACKs advertise window 0.
30. RX length is read twice and headers are parsed in the driver-shared
    buffer (TOCTOU against a hostile driver).
31. DNS queries go out even when no server is configured.
32. The ISN key falls back to TSC and clock without RDRAND.
33. virtio-net is legacy, negotiating MAC only: no checksum offload and
    no link status.
