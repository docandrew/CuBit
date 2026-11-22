# Network authority: current implementation

An ELF manifest requests authority; it cannot approve itself. Policy distributes
ordinary endpoint capabilities whose kernel-stamped **authority tags** identify
private netstack grant records. Caller-supplied message tags confer no authority.
The grant is also bound to the caller process. Holding a general netstack
endpoint permits the current inspection operations, not network IO or control.

## Scope and admission

`CuBit.Network_Authority.Scope` distinguishes `Connect_TCP`, `Listen_TCP` and
`Connect_UDP`.
Outbound scopes contain a canonical IPv4 prefix, inclusive nonzero TCP port
range, and explicit DNS permission. A browser can request all IPv4 destinations
and ports; a dedicated client can request a single address or subnet and port.
DNS permission permits queries to the configured resolver, but a resolved
address must still pass the destination/port check before a SYN is sent.
Allowing DNS is itself an information-release channel, not merely a convenience.

Listener scopes require one explicit, nonzero local IPv4 address and one exact
port. Wildcard addresses, truncated ports, and using outbound authority to listen
are rejected. Listening currently supports the configured first interface only.
Port numbers do not confer authority or imply TLS or peer authentication.

The initial approval source is trusted boot `init.conf`:

```text
network-check.app pri=5 network=declared
```

This explicitly approves that boot image's declared network scopes. Without
the option, the network slots remain empty. Ordinary `OP_SPAWN` requests cannot
set it. This is deliberately a limited bootstrap policy, **not** signed package
admission, a persistent installation ceiling, or a desktop consent mechanism.
It trusts both the configuration and the selected executable. Do not use it to
automatically approve untrusted or replaceable binaries.

Consequently desktop-launched NetSurf, wget, and shell currently receive no
network IO authority through their manifests alone. Their requests have been
migrated, but an installation/launch approval path is still needed for them.
`Connect_UDP` scopes are implemented (see below). ICMP application scopes and
UDP listening are not. Network configuration, raw
manager traffic, and driver ingress use separate bootstrap endpoint tags;
application endpoints cannot impersonate these roles.

## Manifest and IPC representation

Manifest request kind 10, `REQ_NETWORK`, uses the existing 16-byte entry format:
rights must be read/write (3), and the destination slot must be 1 through 62.
`param0` is the IPv4 network-order integer (10.0.2.0 is `0x0A000200`). `param1`:

| Bits | Meaning |
| --- | --- |
| 0–15 | First port |
| 16–31 | Last port |
| 32–39 | IPv4 prefix length, 0–32 |
| 40–47 | Operation: connect TCP = 1; listen TCP = 2; connect UDP = 3 |
| 48 | DNS allowed |
| 49–63 | Reserved; must be zero |

The encoding describes a request, never a bearer credential. Procmgr asks the
policy endpoint to install its approved scope, then mints the child an endpoint
with the returned opaque authority tag. Failed minting releases the reservation.
Explicit endpoint mint parameters now select the authority tag; a zero parameter
retains the existing target-PID tag. Only existing capability-space authority
permits this mint operation; applications cannot self-mint through the protocol.

`NET_OPEN` requires a generation-tagged shared-memory grant: words 0/1 hold its
slot and buffer size, word 3 its generation. Netstack acquires the grant from
the actual caller before reading the scheme or using the buffer and returns
the acquisition when releasing the channel. Read/write/close also check the
channel's owner and authority tag. Raw connection-index operations are denied.

### Async transport correction (2026-09-09)

`SYSCALL_SUBMIT_VIA_ENDPOINT_CAPABILITY` now carries all four message words:
RDI is the endpoint slot, RSI the message tag, RDX/R10/R8/R9 the four payload
words, and R12 the completion token. The kernel preserves the argument
registers and stamps the authority tag itself. No caller-supplied authority tag
or userspace message pointer is introduced by this ABI change.

Previously R9 held the token and the fourth payload word was silently replaced
with zero. That discarded the transfer generation used by NetSurf's async
open and by async accepts. Ada and C wrappers and DOOM's direct submission path
must be rebuilt together with the kernel; there is no old-ABI fallback.

Regression coverage now includes high-bit fourth-word payloads, full-width
completion tokens, 20 synchronous plus 20 asynchronous TCP lifetimes,
unknown-arena and out-of-range buffer rejection, a buffer in use refused for a
second channel, and a listener closed with an offer outstanding. Tests are not
a formal ABI proof.

NetSurf reports a missing network endpoint as "Network access not granted".
This diagnostic does not approve access. Persistent Apps-menu approval remains
unimplemented; a filename-only allowlist or an unverified package ID must not
stand in for approval bound to an authenticated executable and authority ceiling.

The channel handle is an opaque 64-bit ID, not the channel table index. IDs
never repeat within one netstack lifetime and fail closed on exhaustion.
`Network_Channel_Handles` resolves the ID together with the actual caller and
authority tag before the service accesses mapped buffers. Closing a channel
invalidates its ID; a later channel occupying that slot gets a different ID.
A TCP connection's CLOSED state does not release its storage while a channel
still owns it, preventing old channel references from reaching a new connection
under the same application authority. Explicit channel release drops that
reservation. These IDs are not credentials, serialized capabilities, or
persistent identities across a service restart.

Listening has no bind, listen or accept operations. OPEN of
`@net:tcp-listen:<address>:<port>` in an arena buffer makes a listener, in
one step. It is checked against the caller's `tcp-listen` scope: the exact
interface address, and a port not in use by an outbound connection. SHUT closes
it. Its rings carry records (`CuBit.Net_Channel_Layout`, "Listeners"):
- In the send ring, the process offers arena buffers for connections.
- In the receive ring, netstack reports each arrival: a connection now open
  in an offered buffer, with its channel handle and peer.

An offer must name a free buffer of the caller's own arena; a bad one is
dropped without harm to anyone else. An accepted channel is charged to the
listener's scope and answers only to that scope's endpoint. Accepted channels
survive listener closure; offers and connections nobody took do not.
Connections that no offer takes stay in the listener's backlog until the
handshake deadline resets them.

`OP_NET_SCOPE` on any network endpoint returns that endpoint's own scope, so a
program holding several (the libc's sockets, for one) can route each connection
to the scope that permits it. It reveals nothing about any other scope.

The unchecked PID-directed `SYSCALL_SUBMIT` (23) and runtime wrapper are removed.
Its callers now find an already-held endpoint and use `capSubmit`. A missing
endpoint fails closed; lookup is not a request to grant new authority.

## Declared connections

Every scope declares how many channels its holder may keep open at once
(`(connections N)` in the manifest, descriptor bits 49–63; zero is invalid).
`OP_INSTALL_SCOPE` reserves that many against netstack's channel capacity and
fails if the sum of reservations would exceed it. Opening a channel (outbound
TCP, connected UDP, or a waiting accept) charges one to the scope; releasing
the channel refunds it. A further open is refused while all declared channels
are in use. The policy ceiling `Broad_Outbound_TCP` carries the maximum count,
so `Includes` also bounds the declaration.

Proved at level 1 (`make -C kernel prove-network-authority`): reservations
never exceed the capacity, and no grant has more channels open than it
declared. Tested on Linux (tests/network-authority) and natively
(network-check fills its two declared datagram channels, sees a third refused,
and sees a closed channel's place returned).

The capacity is currently netstack's fixed channel table; startup limits
passed as typed launch parameters will replace it.

## Connected UDP channels

A `Connect_UDP` scope has the same shape as an outbound TCP scope: an IPv4
prefix, a port range and DNS permission. In CCL:

```lisp
(request-network udp-connect (ipv4 "10.0.2.2" 32)
  (ports 123 123) (dns deny) (connections 2) time-server)
```

Operations never cross protocols: a TCP scope cannot open a UDP channel, a UDP
scope cannot open a TCP connection, and `Includes` never places one inside the
other. The desktop's browser approval (`Browser_Outbound`) is TCP-only. UDP
authority currently requires `network=declared` boot approval.

UDP reuses the channel operations rather than adding new ones. `NET_OPEN` with
`@net:udp:HOST:PORT` opens a **connected** channel: netstack checks the scope,
then assigns a fresh ephemeral local port (49152–65535) that no other open UDP
channel uses. Applications cannot choose a local port. There is no handshake,
so the reply is immediate, or follows DNS resolution when the scope allows it.

| Operation | UDP meaning |
|---|---|
| `NET_WRITE` | Sends exactly one datagram of 0–1472 bytes to the channel's peer. |
| `NET_READ` | Returns the oldest queued datagram. Word 0 is the copied length; word 1 bit 0 means the datagram was longer than the requested maximum and was truncated. The whole datagram is consumed either way. The optional absolute deadline and the one-reader limit match TCP. |
| `NET_SHUT` | Ends a waiting read with EOF, releases the local port and discards queued datagrams. |

Incoming datagrams are queued only for the channel whose local port, remote
address and remote port all match. Everything else is dropped, and so is a
datagram for a full queue (four per channel). netstack's own resolver uses
port 10053. It accepts replies only from the configured DNS server's port 53
to that port, so an application's UDP traffic to a port-53 server is never
treated as a resolver reply. 1472 bytes is the most that fits one 1500-byte
frame; the stack does not reassemble IPv4 fragments.

The pure `UDP_Channels` core (port allocation, exact-endpoint filtering,
FIFO queue, truncation, close) is SPARK-proved for absence of runtime errors
and for its postconditions: a fresh port is not used by any other active
channel, and a queued datagram's channel matches its local port and source
endpoint exactly. Packet parsing, grants and IPC handling in `main.adb` are
tested, not proved.

## Raw link access (design, 2026-09-27)

Raw access is a supported feature, not a special case: capture tools,
intrusion detection, userspace protocol daemons, CuBit as a router, and
DHCP in netmgr all use it. It is granted, like every other network
authority, by a declared scope that admission checks and review tools
can show.

```lisp
(request-network raw-link
  (interface "if0")
  (ethertype ipv4)
  (send (ip-protocol udp) (source-port 68) (destination-port 67))
  (receive copy (ip-protocol udp) (destination-port 68))
  (source-mac own)
  (source-ip unspecified own)
  (promiscuous no)
  (connections 1) dhcp)
```

**Scope fields**
- **`interface`:** a named interface (netmgr assigns names). A scope never
  covers every interface implicitly.
- **`send` and `receive`:** each takes a match, and either may be absent
  (a capture tool is receive-only). netstack checks every outgoing frame
  against the send match, with a proved matcher, before it reaches the
  driver.
- **The match vocabulary** is small and closed, not a program:
  - EtherType and VLAN;
  - IP protocol;
  - source and destination prefixes, in the 16-byte address type;
  - port ranges and ICMP types.
- **Receive modes:**
  - `copy` (a tap): the holder gets a copy, and the stack processes the
    frame as usual.
  - `divert`: only the holder gets the frame, and the stack never sees
    it. This is for a userspace daemon that owns a protocol. Admission
    refuses overlapping divert scopes, so a frame goes to at most one
    holder.
  - `verdict` (planned): inline pass, drop or rewrite, for firewalls.
    Possibly a proved in-stack filter instead of a userspace round trip.
- **Source addresses:** `source-mac` is `own` (the default), a specific
  address (a router's or VRRP virtual MAC), or `any` (security tools,
  bridges). `source-ip`, on IP payloads, is `own`, `unspecified`, a
  prefix or `any`. Spoofing power is therefore explicit and visible.
- **`promiscuous`:** meaningful only on a receive scope. The NIC is
  promiscuous while any held scope asks for it (see below).

**Locator:** `@net:raw-link:if0`, or `@net:raw-link:if0:ipv4` to choose
the EtherType when a program holds several. The locator names the
endpoint; the scope bounds what it may do. In CCL, the `Net` authority
kind gains a `Raw_Link` variant with typed `Interface` and optional
`EtherType` fields.

**Records:** a whole frame per ring record, with interface, direction,
receive time, and truncated/broadcast/multicast flags.

**Planned alongside:** an IP-layer raw kind (`@net:raw-ip:if0:icmp`, for
ping and traceroute), where netstack writes the IP header; and
unconnected UDP with multicast membership, for mDNS and SSDP.

**DHCP in netmgr:**
- netmgr declares the scope above and builds its frames with proved
  codecs (`IPv4_Header.Build`, a UDP builder, `DHCP_Message`). A proved
  `DHCP_Client` runs the lease.
- It applies the lease through `OP_NET_CONFIGURE` and `OP_NET_SET_DNS`.
- It replaces the `OP_NET_OPEN_RAW` stub and netmgr's current
  `dhcp.adb`.

## Packet capture (planned)

Network security tools (capture, intrusion detection, diagnostics) need to
see frames not addressed to this host. This is a separate authority, not a
flag on an ordinary channel:
- **Declared scope.** A program declares `(request-network capture ...)` in
  its manifest, naming the interface, direction, and optionally a filter
  (EtherType, addresses, ports). As with other scopes, installing it is
  explicit and inspectable.
- **Read-only mirror.** netstack copies matching frames into the holder's
  receive ring, the same ring type channels use. A capture never
  consumes, alters or injects a frame: sending raw frames would be a
  further, separate authority.
- **Promiscuous mode by holder count.** The NIC enters promiscuous mode
  (virtio-net `VIRTIO_NET_CTRL_RX_PROMISC`) only while at least one capture
  scope that asks for it is held. It leaves promiscuous mode when the last
  such scope is released or its owner exits (`OP_RELEASE_OWNER`).
- **Bounded and lossy by design.** A full ring drops the frame and counts
  the drop. Capture never applies backpressure to the stack.
- **Filters.** A filter is a small declarative match, checked by a proved
  unit, rather than a BPF-style program.

## Limits and follow-up work

- Scope/grant ADTs have focused SPARK analysis and hosted tests; the entire
  network service, policy adapter, and kernel mint bridge are not thereby proved.
- The listener table (`TCP_Listeners`) is proved at level 1
  (`make -C kernel prove-tcp-session`):
  - a connection waits in at most one backlog, by construction;
  - every waiting connection's listener is open, so a closed listener
    leaves none behind;
  - handles are never reused;
  - only the owner takes a ready connection, and taking it removes it;
  - no listener ever has more than `Maximum_Backlog` connections waiting (its
    count always equals a ghost count of the connections waiting on it).
- Listeners, arrivals into offered buffers, and accepted-channel IO are live
  and tested natively (network-check, and the libc's sockets in net-bench's
  serve workload).
- Capacity: 64 TCP connections (each a 64 KiB receive queue backed at
  load, so the count is set by memory: 128 no longer fit the 128 MB desktop
  profile), 64 channels, 16 listeners of 16 pending
  connections each, 32 scopes, 32 arenas: fixed until netstack takes startup
  limits (docs/ccl-launch-parameters.md).
- The 32-entry grant table fails closed on exhaustion and never reuses a tag
  within a service lifetime. Owner exit: `OP_RELEASE_OWNER` (policy endpoint
  only) answers the process's deferred requests, releases its channels,
  listeners, arenas and scopes, and returns their reservations. The kernel
  sends `EVENT_CHILD_EXIT` to the registered procmgr as well as the parent;
  procmgr acts only when the PID is absent from the kernel process list (the
  event can be forged), clears its authority and sends `OP_RELEASE_OWNER`. It
  also sends it before launching into a reused PID. Tested natively
  (network-authority requires the release marker), not proved. Restart
  semantics remain.
  Release is used for installation rollback, not a completed general
  revocation protocol for live streams.
- Outbound channel handles still need generation-safe lifetimes, including
  connection-slot reuse within one grant. Cross-owner/tag checks are not a
  replacement for that work. Timeout, owner exit, pending replies and connection
  reservations need unified cleanup before exposure to untrusted networks.
- Persistent approved-installation ceilings, scoped launch delegation, signed
  provenance, consent UX and structured denial visibility are follow-up work.
  `Includes` supplies scope containment logic, not an implemented policy service.
- Retransmission, segmentation, backpressure, initial sequence numbers and
  network abuse budgets remain tracked in the inbound implementation document.

See [the regression instructions](../tests/network-authority/README.md).
