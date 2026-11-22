# Native inbound networking implementation

Status: authorized native bind/accept/close and passive TCP are implemented and
tested in loopback-only QEMU. Observatory now connects directly over bounded
HTTP/CBOR to the native control app: real CCL evaluation, own bindings, and clock
IPC. This is not an internet-ready TCP or management stack. See
`userspace/ccl/tools/ccl-observatory/README.md` for the isolated lab procedure.

The management client should connect to CuBit's network stack directly. A
workstation TCP relay is not the target architecture. SPARKTLS can later wrap
the same accepted byte-stream transport without changing CCL evaluation or
granting the browser ambient operating-system authority.

## Implemented and checked

- `TCPSession` is a pure SPARK event/action model with passive SYN/SYN-ACK/ACK
  handling, duplicate SYN handling, and validation of the initial ACK.
- Peer FIN closes the receive direction, allowing the application to send a
  response before closing its own direction. A final handshake ACK carrying
  data and FIN has enough action capacity to retain all notifications.
- Receive credit prevents acknowledging payload that cannot fit in the
  connection's receive buffer. Reading buffered bytes updates advertised credit.
- `TCP_Listeners` is a separate bounded SPARK ADT: explicit local address/port,
  owner-bound non-reused handles within a service lifetime, bounded half-open
  plus ready backlog, readiness-before-accept, and deadlines. Closing a listener
  returns its unaccepted children for cleanup; accepted connections are separate.
- Native receive processing rejects malformed IPv4 total lengths, unsupported
  fragments, invalid IPv4 header checksums, and invalid TCP pseudo-header
  checksums before dispatching TCP events.
- `Network_Channel_Handles` now separates wire-visible identities from reusable
  channel slots. Native OPEN returns non-reused IDs; READ/WRITE/SHUT resolve
  them against both caller and kernel-stamped authority tag. A CLOSED TCP slot
  remains reserved while its channel owns it. Failed opens return acquisitions
  and reservations when they receive a connection error. Remote TCP port 53 no
  longer diverts arbitrary application data into the DNS parser.

Hosted regression tests and focused proof commands are documented in
`tests/tcp-session/README.md`. The listener ADT now drives the native receive
loop after checksum and exact interface-zero destination validation. No public
host port has been enabled; the test forwards only `127.0.0.1:18444` to the
explicitly authorized guest listener `10.0.2.15:8080`.

`NET_ACCEPT` takes four words: listener ID, shared-memory grant slot, buffer
size, and grant generation. It acquires the caller's writable transfer buffer
and returns a normal owner/tag-bound channel ID only after the handshake.
The same READ/WRITE/SHUT path handles both outbound and accepted channels.
Closing a listener cancels its waiting accept and resets its unaccepted backlog,
but does not close channels already handed to the application.

There is at most one waiting accept per listener. Its deadline is 30 seconds;
handshaking and ready-but-unaccepted children share a five-second deadline
from the initial SYN. Expiry resets those children and returns their connection
reservations. The event loop checks deadlines even under message traffic and
uses `receiveUntil` with the earliest actual deadline when idle, not an accept
polling interval. Existing pending pings also use this wakeup mechanism.

`NET_READ` optionally accepts length=4 with an absolute monotonic-millisecond
deadline in word 3. Length=3 retains indefinite-read behavior for existing
callers. A channel permits one outstanding read; a second read cannot replace
its deadline or race the transfer buffer. Zero-length reads return immediately.
Expired reads consume their pending reply and leave the owned channel available
for the caller to close. The control app uses one deadline for the whole HTTP
request, not a renewed allowance for each received fragment. Its loopback
forward is host port 18445; the independent network-authority test uses 18444.

## Admission decision

The existing `REQ_SERVICE` manifest request grants an endpoint to netstack;
it does not distinguish outbound connection requests from listening. Existing
service capabilities minted by procmgr carry the target process ID as their
authority tag, not a network-operation scope. Merely adding a message label or
trusting a caller-supplied authority tag does not create a separate authority gate.

Approved policy, consistent with `web-hosting.md`: require a distinct
manifest-declared `network.listen` scope for an explicit local address and port.
Trusted policy grants this scope; the netstack creates an owner-bound listener
handle after validating that grant. Ordinary outbound apps cannot bind ports.
This should reuse the existing capability/policy model, not invent a parallel
identity-based permission system or hard-code a privileged application PID.

The initial scope identifies TCP, an explicit local address, and an exact
nonzero local port. The manifest is a request ceiling, not a self-issued grant:
trusted policy must approve it. Missing authority, a different port/address,
another transport, or a wildcard request is denied. There is no implicit
"any port" default, nor special ambient privilege for low-numbered ports.
These checks run on the native bind path. Incoming SYNs allocate state only
under such an admitted listener; ACCEPT additionally checks the actual caller,
the listener's authority tag, and the transfer grant generation.

Listener authority permits admission of incoming connections on that endpoint;
accepted connection handles permit IO on those connections. It does not grant
arbitrary outbound connection creation. Likewise, outbound authority does not
grant listening. Port 443 alone implies neither TLS nor an authenticated peer.

The grant/admission representation is described in
[network-authority.md](network-authority.md). `NET_BIND` reserves an authorized
endpoint; `NET_CLOSE_LISTENER` releases it. `NET_ACCEPT` hands off an established
connection without granting the application outbound-connect authority.

### Relationship to host firewalls and malware containment

Default-deny capability distribution replaces a major host-firewall role:
deciding which applications may expose which listening endpoints. An unrelated
application cannot take advantage of a generally "open port" without its own
authority. This is an allowlist of explicit grants, not a blacklist requiring
recognition of every malicious binary.

It does not eliminate all network defenses. Source-network restrictions,
connection budgets, rate limits, and early malformed-packet rejection still
protect admitted services and the network stack. Upstream filtering may be
necessary against traffic that saturates the host's link. These controls should
compose with the same declared policy and have clear visibility, rather than
become an independent source of application authority.

In particular, denying listeners prevents an unauthorized inbound RAT listener,
not every remote-access tool: malware can use an outbound HTTP(S) connection for
command and control ([MITRE ATT&CK T1071.001](https://attack.mitre.org/techniques/T1071/001/)).
Outbound authority therefore needs independent policy, with destination/peer
scope where appropriate. An application with unrestricted outbound access can
exchange commands and data over that access. A compromised approved service can
also misuse its legitimate connections; signing/provenance is admission evidence,
not proof of benign behavior. Its filesystem, secrets, input, and other authority
must remain least-privilege independently of its network authority.

## Integration sequence

1. Done: enforce manifest-declared, explicitly boot-approved TCP scopes and
   exercise the native authorization boundary with denial tests.
2. Done: wire passive receive, deferred accept, and nearest-deadline wakeups into netstack.
   Validate destination address and checksum before reserving connection state.
3. Done: give accepted channels non-reused, owner-bound identities. Prevent the
   older raw connection-index APIs from accessing another application's accepted
   connection. Use generation-tagged grant acquire/return for accepted IO buffers.
   Accepted channels use the common outbound identity/reservation mechanism.
4. Ensure resets, timeout, peer close, listener close, and owner death release
   buffers, reply capabilities, and connection reservations exactly once.
   Normal timeout/close and failed accept delivery release service resources.
   General owner-death teardown and stale kernel reply-cap cleanup remain open
   (SEC-016 in `security-hardening.md`).
5. Done: remove dispatch based solely on remote port 53. A client's source port
   must not cause its application bytes to enter the DNS response parser.
6. Add a local-only QEMU incoming TCP test: successful handshake, fragmented
   application writes, request/reply after peer half-close, malformed traffic,
   wrong-owner/stale-handle rejection, backlog exhaustion and recovery.
   Implemented: real inbound handshake/data, half-close response, direction/tag
   denial, stale grant/listener rejection, accepted-channel survival of listener
   close, and idle accept expiry. Cancellation/backlog-expiry coverage is in the
   expanded regression. Malformed-wire injection and half-open flood coverage
   remain to be added; the hosted listener tests cover bounded-table exhaustion.
7. Build the bounded HTTP/typed-message adapter on accepted native channels;
   reconnect the browser visualization to actual replies. There is no completed
   browser-to-CuBit path at this stage.

## Before internet exposure

The stack still needs retransmission/timer work, MTU-aware TX segmentation,
send-window/backpressure enforcement, stronger initial sequence numbers,
comprehensive close/TIME_WAIT handling, and abuse/resource-budget tests.
Initial support should explicitly limit local interfaces rather than imply
wildcard or general multihoming support.

A TCP listener is not authenticated management access. Development plaintext
must be confined to a deliberately selected local test path; TLS peer
verification, session authority admission, and browser-origin protections are
separate requirements before exposing CCL evaluation to an untrusted network.
