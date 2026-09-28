# Secure Networking, Time, and Trust Services

Status: design proposal, September 2026. Nothing in this document is
implemented yet unless it is listed under the current baseline.

## Purpose

This roadmap covers the next usable-networking milestone. The goal is
for CuBit to fetch real HTTPS content on physical hardware, with verified
certificates, trustworthy time, and no TLS code inside untrusted applications.
It also covers two prerequisites and one related direction:

- the time service that certificate validation depends on;
- driver matching in devmgr, so hardware NIC and Wi-Fi drivers start only for
  devices that are present;
- a key and certificate lifecycle design that later supports CuBit as a server.

The first hardware target for wired networking is the Intel N95 NUC. The
development laptop (Sunrise Point PCH, Intel Wireless-AC 3165, no Ethernet
port) is the Wi-Fi target. Everything except the hardware NIC drivers should be
developed and regression-tested in QEMU first.

### Milestone exit criteria

1. **QEMU:** CuBit boots and gets a DHCP lease over virtio-net. Its clock is
   synchronized by NTP, and it reports that clock's quality. NetSurf loads an
   `https://` page through the TLS service with a verified WebPKI chain. The
   tests reject an expired certificate, a wrong hostname, an untrusted root and
   a downgrade attempt, each with a typed error.
2. **NUC:** the same boot, DHCP, time and HTTPS flow over the NUC's wired NIC,
   booted from USB media.
3. **Laptop (separate track):** the same over the Wireless-AC 3165, with the
   card confined by the IOMMU.

## Current baseline

Facts this design builds on (verified against the tree, not proved):

- `netstack.svc` implements TCP connect and listen channels, authorized by
  manifest scopes, as described in the
  [network authority](network-authority.md) document. **UDP and ICMP
  application scopes do not exist.** DNS resolution lives inside netstack.
  DHCP lives in `netmgr.svc`.
- The only NIC driver is `virtio-net.drv`. Its netstack interface is
  `OP_NET_ATTACH`, `OP_NET_RX` and `OP_NET_TX`: one frame per IPC through fixed
  RX and TX halves of a single grant, with a deferred TX queue.
  That is adequate for QEMU bring-up, but it is not the right shape for a
  gigabit hardware NIC.
- `clock.svc` provides UTC with the quality `RTC_Only`.
  [Clock and time services](clock-and-time-services.md) already requires a
  separate time-sync service and a distinct clock-adjustment endpoint.
- CuBit has no random-number service, and this design adds none. Randomness
  comes from SPARKTLS itself (see below).
- The kernel saves FPU state with `fxsave`/`fxrstor` only and never enables
  AVX state in XCR0. SPARKTLSCrypto's AVX2 and AVX-512 detection checks
  OSXSAVE and XCR0, so it falls back safely to the AES-NI/SSE and portable
  code. Kernel XSAVE support is a later performance item, not a prerequisite.
- `devmgr.svc` has a fixed startup sequence. It scans PCI into an inventory and
  records the first device found for each supported class. It then starts a
  hardcoded list of drivers, and some start whether or not their device exists
  (see the HDA backlog item).
- `FETCHER.md` (in `userspace/c/netsurf/`) already specifies that `https:`
  goes through a SPARKTLS client boundary service, never falls back to plain
  HTTP, and reports certificate failures as typed errors.
- The [web hosting](web-hosting.md) and [remote management](remote-management.md)
  documents both assume a SPARKTLS gateway for inbound TLS.

### SPARKTLS as a dependency

SPARKTLS is used as-is from its sibling repository. CuBit does not modify it.
Upstream changes (a configurable handshake pool and similar) are tracked there.
Relevant properties:

- The API does no I/O of its own. `Advance` returns actions such as
  `Has_Output`, `Need_Input`, `Handshake_Done`, `Plaintext_Ready`,
  `Error_Alert` and `Shutdown`. The caller moves bytes. This maps directly
  onto a CuBit service event loop.
- Random bytes, the time source, the certificate-verification veto and
  external signing are callbacks installed in `Config`.
- `SPARKTLS.RBG` (pending on SPARKTLS's `chore/entropy-failure` branch) seeds a
  two-tier DRBG from SPARKEntropy's CPU-jitter source, which runs its SP 800-90B
  health tests. Its `Source_Fn` hook can substitute another entropy source.
  SPARKEntropy needs only `rdtsc` and libkeccak. The kernel leaves CR4.TSD
  clear, so `rdtsc` works from userspace.
- `SPARKTLS.Cert_Verify.Load_Roots` (concatenated DER), `Add_Root`,
  `Set_Identity` and `Set_Identity_Public` all work from in-memory buffers.
  `SPARKTLS.Credentials` and `SPARKTLS.System_Roots` are file-reading
  convenience wrappers built on `Ada.Text_IO`, `Ada.Directories` and stream I/O.
  No other SPARKTLS unit depends on them. **The CuBit build excludes both
  units.** Trust anchors and identities arrive as byte buffers from CuBit
  services, not as ambient paths.
- Handshake data lives in a fixed pool of `Max_Inflight` slots (currently 16,
  roughly 236 KB each). A full pool refuses new handshakes before allocating.
  Established sessions hold no slot.
- The trust store holds up to 200 roots of at most 8 KB DER each. That is
  enough for the Mozilla root program.

## Service map

```text
               untrusted applications
      NetSurf        wget        shell / CCL        future mail client
         \             |             |                  /
          \   REQ_TLS scope (host pattern, ports, profile)
           v           v             v                v
        +--------------------------------------------------+
        | tls.svc   SPARKTLS client sessions (with RBG),   |
        |           policy, trust store, per-client quotas |
        +------+-------------------+----------------+------+
               |                   |                |
        time + quality       connect by name    signatures (client
               v                   v            auth, later)
           clock.svc          netstack.svc           v
               ^               ^        ^        keystore.svc
          adjustment      UDP scope   frame rings     ^
           endpoint            |        |             |
               |               |        |         certmgr.svc
          timesync.svc --------+        |         (ACME, CRL/OCSP,
          (SNTP, then NTS;              |          root updates)
           links SPARKTLS)         NIC drivers
                                   virtio-net, NUC wired, iwlwifi
                                        ^
                                        | match + grant
                                    devmgr.svc
```

Every arrow is an explicitly granted endpoint. No service in this map gains
ambient filesystem, network or device authority.

## Randomness

No separate entropy service. tls.svc and timesync.svc get randomness from
`SPARKTLS.RBG`, which seeds its DRBG from SPARKEntropy's CPU-jitter source
inside each process. keystore.svc, when it exists, uses the same component.

CuBit-side testing:

- Record SPARKEntropy's start-up and continuous health-test results under QEMU
  (TCG and KVM) and on both physical machines. Timer jitter in a virtual
  machine is not evidence for real hardware, and the reverse is also true.
- Check that a health-test failure at start-up makes tls.svc refuse new
  handshakes with a typed error and a log event, rather than start.
- Measure start-up collection time, because it adds to service start latency.

Adding RDSEED through the `Source_Fn` hook is a possible later change, to be
decided with SPARKTLS upstream.

## Time synchronization

### Netstack prerequisite: UDP scopes

Implemented (2026-09-23); see
[connected UDP channels](network-authority.md#connected-udp-channels).
`Connect_UDP` scopes open connected channels through the existing `NET_OPEN`,
`NET_WRITE`, `NET_READ` and `NET_SHUT` operations rather than new opcodes. A
channel accepts datagrams only from its one remote endpoint, and netstack picks
the local port. No application can listen on a chosen port.

### Stage 1: SNTP

`timesync.svc` holds UDP scopes only for its configured servers (CCL setting
`time.servers`, by name or address) on port 123. It:

- queries at least three servers, and takes samples at a bounded rate with
  jitter;
- rejects Kiss-o'-Death replies, bad leap indicators, stratum 0 or stratum 16,
  zero transmit timestamps, and replies that don't echo its origin timestamp;
- combines agreeing samples (the intersection of their error intervals, in
  the style of Marzullo's algorithm) and drops outliers;
- submits a typed sample to clock.svc's **adjustment endpoint**: UTC, the local
  monotonic observation, uncertainty, source count and authentication state.

`clock.svc` validates the sample and applies an explicit step/slew policy, as
[clock and time services](clock-and-time-services.md) already requires. Add
new quality values:

| Quality | Meaning |
|---|---|
| `RTC_Only` | Existing. Boot RTC sample advanced by monotonic time. |
| `Network_Unauthenticated` | Agreeing SNTP samples. An on-path attacker can shift it. |
| `Network_Authenticated` | NTS-authenticated samples. |

### Protecting certificate validation from time attacks

Unauthenticated time is an attack surface for TLS. An attacker who can move the
clock backward can make an expired or revoked certificate look valid again.
Rules:

- The clock never moves below a **time floor**. The floor is the maximum of:
  the image build timestamp, and the last authenticated time persisted in
  Config.
- Unauthenticated sources may not step the clock by more than a configured
  bound (for example 15 minutes) away from RTC time. The exception is when the
  RTC is unavailable, or below the time floor. Larger unauthenticated
  corrections are recorded as a conflict and wait for authenticated time or
  user confirmation.
- `tls.svc` receives the quality along with the time. Policy may require
  `Network_Authenticated` or `RTC_Only` for particular profiles. A time error
  is reported to the client as a distinct `Clock_Untrusted` failure, not as a
  certificate error.

### Stage 2: NTS (RFC 8915)

NTS key establishment is TLS 1.3 with ALPN `ntske/1`, followed by a
TLS keying-material exporter. SPARKTLS supports both. The RTC is accurate
enough to check certificate validity windows, so the bootstrap order works:
RTC time validates the NTS-KE certificate, NTS then provides authenticated time.

**Decided:** `timesync.svc` links SPARKTLS directly for NTS-KE, with its own
small trust store read from the same bundle. tls.svc gains no exporter
operation. The cost is a second SPARKTLS instance and trust-store copy.

**Bootstrap time for NTS-KE certificate validation.** Before NTS succeeds, the
only time sources are the RTC, the time floor and unauthenticated SNTP. They
are combined as a cross-check, not a vote:

- If the RTC and the SNTP estimate agree within the unauthenticated step
  bound, the NTS-KE certificate must be valid at **both** times. Validity is a
  single interval, so this means it is valid across the whole span between
  them. A spoofed SNTP reply then cannot, by itself, bring an expired
  certificate back into validity.
- If they disagree beyond the bound, NTS-KE validation uses the RTC time. The
  conflict is logged, and the clock stays at `RTC_Only` until NTS succeeds or
  the user resolves the conflict.
- If the RTC is unavailable or below the time floor, validation uses the SNTP
  estimate, which must itself be at or above the floor. An attacker can then
  move time forward, which only denies service by making certificates look
  expired. They cannot move it back below the floor.

The time used and its sources are recorded with each NTS-KE result, so a later
review can see what a validation depended on.

### Testing

Run a Linux-hosted SNTP and NTS fixture server, reached through QEMU user
networking. It injects wrong-origin replies, Kiss-o'-Death, large offsets,
backward steps, disagreeing servers, replayed packets and server loss. The
clock-side checks for the step/slew policy and the time floor should be pure
SPARK and hosted-tested, like the existing clock helpers.

## TLS client service: status (2026-09-24)

Step one, SPARKTLS running natively on CuBit, is done. Step two, the
`tls.svc` client service, is implemented and passes its headless test; see
"`tls.svc` v1" below. NetSurf and wget HTTPS (phase 4) come next.

- **Build.** `userspace/lib/tls/sparktls_cubit.gpr` compiles SPARKTLS,
  SPARKTLSCrypto, SPARKx509, SPARKEntropy, SPARKMLKEM, SPARKNaCl and libkeccak
  unmodified against CuBit's runtime. The crates are pinned `flake = false`
  inputs in `flake.nix`/`flake.lock`. `nix develop` exports them as one
  directory, `CUBIT_SPARK_CRATES`; to build against local checkouts, use
  `nix develop --override-input sparktls path:../sparktls`.
  `SPARKTLS.Credentials`, `SPARKTLS.System_Roots` and SPARKNaCl's debug units
  are excluded.
- **Trust store.** `make -C kernel tls-roots` converts nixpkgs' pinned
  Mozilla bundle (`CUBIT_CA_BUNDLE`, from `cacert`) into concatenated DER
  with `tools/pem_bundle_to_der.py`: 121 roots, 129 KB. It is installed on
  the development disk as `tls/roots.der`. The default `init.ccl` starts
  tls.svc with its declared network scope approved.
- **Runtime additions**, needed and generally useful:
  `Interfaces.Unsigned_128` (with `System.Max_Binary_Modulus` raised to
  2**128; signed limits unchanged), plus minimal `System.Tasking` and
  `System.Tasking.Protected_Objects` for protected objects without entries.
  CuBit processes are single-threaded, so the lock only detects re-entry.
- **A known compiler bug had to be worked around.** A long-standing GCC
  value-range-propagation bug miscompiles SPARKNaCl's SHA-256 at `-O2` with
  `-gnatp`: the last three digest bytes are never stored. This reproduces with
  GNAT 15.3 and 16.1, and with both SPARKNaCl 4.0.1 and master. SPARKNaCl's
  own project already applies `-fno-tree-vrp` on x86, so upstream builds are
  unaffected. CuBit compiles the sources directly and at first lacked the
  flag. That broke HMAC-SHA-256 and the HMAC-DRBG on CPUs without SHA-NI,
  where SPARKTLSCrypto falls back to SPARKNaCl; SPARKTLS's DRBG self-test
  caught it and refused to start. `sparktls_cubit.gpr` now uses
  `-fno-tree-vrp` for all crates.
- **netstack.** TCP writes are now split into 1460-byte segments; before this,
  anything larger was one oversized frame and was dropped. The transmit-slot
  ring grew from 8 to 48 slots, more than the driver's 32-message mailbox, so
  a burst cannot overwrite an unsent frame. Retransmission and peer-window
  handling are still missing.
- **`tls-probe`** (headless test `tls-probe`) is a native app linking SPARKTLS.
  It seeds the RBG from SPARKEntropy (health tests pass under KVM and TCG),
  loads a test root from memory, and runs four handshakes against an OpenSSL
  fixture. The valid case completes TLS 1.3 (AES-256-GCM) with a data
  exchange; wrong host, untrusted root and expired certificate are all
  rejected. Measured: entropy start-up 15 ms under KVM and 40 ms under TCG;
  handshake 22 ms under KVM and 716 ms under TCG. These are emulator numbers
  against a Linux-hosted fixture, not real hardware or internet servers.

### `tls.svc` v1 (2026-09-24)

What exists:

- **Authority.** Instead of a new manifest request kind, a TLS scope is an
  access-section entry: `(tls-scope "host:port")`, also written
  `host:first-last`, `*.suffix:port` or `*:port`. `CuBit.TLS_Scopes` (pure
  SPARK, 88 checks proved) parses and matches patterns. The manifest compiler
  and procmgr validate with it, and so does the service. Names are canonical
  lowercase DNS names matched at label boundaries. IP literals are never
  names. `*.com` style top-level wildcards are rejected.
- **Installation.** procmgr mints itself a `Policy_Tag` endpoint to tls.svc
  once the service registers. Before resuming a new process it sends that
  process's scopes (`Set_Scopes`), and it revokes a reused PID's scopes first.
  Only the kernel-stamped policy tag can install or revoke; a client's forged
  attempt is refused. Registration as tls.svc also requires the trusted
  startup plan, not just the self-declared package ID.
- **Service** (`userspace/services/tls/`). A single-threaded event loop over
  client requests, async netstack completions and deadlines, with
  `OPEN`/`WRITE`/`READ`/`SHUT`/`INFO` as in `CuBit.TLS_Protocol`. Each
  channel runs one SPARKTLS client session with WebPKI validation and the
  requested name as SNI. The trust store is loaded from
  `@nvme:0/tls/roots.der` (concatenated DER, read through a scoped filesystem
  handle). Certificate validation refuses to run without valid wall time.
  Failures are typed (`Scope_Denied`, `Certificate_Untrusted`,
  `Certificate_Expired`, `Timeout`, ...). A peer close retires the
  connection, but the client keeps its channel ID: buffered data stays
  readable, then EOF, until the client's `SHUT`.
- **Names without public DNS.** The `tls.hosts` setting
  (`name=a.b.c.d ...`) maps names to addresses for private deployments and
  tests. Authorization and certificate checks still use the name.
- **Test** (`tests/headless/run.sh --test tls-service`, KVM and TCG). The
  client has no network scope and links no TLS code. Checked: denials for
  out-of-scope names, ports and subdomains; IP literal refused; forged policy
  refused; a verified TLS 1.3 channel with a data exchange, `INFO`, `SHUT` and
  stale-channel refusal; wrong host, untrusted root and expired certificate as
  typed failures, each aborted from the OpenSSL fixture's point of view.

Known limits: eight channels (netstack now supports 16 TCP connections
system-wide, up from four); a client write is refused with `Busy` while that channel's
previous ciphertext write is in flight; no client-death cleanup (channels are
revoked when the PID is reused); a restarted tls.svc is not re-bound by
procmgr; no OCSP stapling request, CRLs or certificate exceptions; stapled
OCSP only under SPARKTLS's `Soft_Fail`. The expired-certificate case
currently surfaces as `Certificate_Untrusted`, because SPARKTLS reports the
chain failure generically.

### Phase 4: HTTPS in applications (2026-09-24)

- **Launch approval gates TLS scopes.** procmgr installs a process's
  `tls-scope` names only when its launch was approved for network use (boot
  `network approve-declared`, or the desktop's browser approval), exactly as
  for network scopes. A manifest alone never reaches the network through
  tls.svc. `tls-service` checks this natively: the same app launched without
  approval is denied its own name.
- **NetSurf.** The CuBit fetcher serves `https:` through tls.svc (endpoint
  slot 32, `(tls-scope "*:1-65535")`, installed only with approval) and
  `http:` through netstack as before. One state machine serves both, typed
  TLS failures become readable error pages, and there is never an
  HTTPS-to-HTTP fallback. The fix for a missing port in the `Host` header
  (RFC 9110 7.2) applies to both schemes. Regression `netsurf-https`: a
  NetSurf build whose homepage is the loopback HTTPS fixture (the default app
  is rebuilt afterwards) fetches `/` over TLS 1.3 through tls.svc and renders
  it. HTTPS-to-HTTP redirects are still NetSurf's own policy.
- **wget** is now HTTPS-only through tls.svc, with
  `(tls-scope "example.com:443")` and no network scope. `wget-https` is a
  network-dependent lane: it fetches `https://example.com/` against the
  production trust store and requires `HTTP/1.1 200 OK`. It passed on
  2026-09-24.
- **netstack capacity.** 16 TCP connections (was 4), 32 channel handles
  (was 8) and 32 deferred requests; tls.svc has 8 channels. The
  network-authority regression now runs 40 connection lifetimes, so table
  slots are still reused.

Not yet done: TLS for other schemes (for example WebSockets), client
certificates, a certificate-error UI with explicit exceptions, and
HTTPS-to-HTTP redirect policy.

## TLS client service

### Why a service

TLS runs in `tls.svc`, not inside applications:

- **Untrusted parsers never hold keys.** NetSurf is an untrusted HTML, CSS and
  image parser. A compromised renderer can still misuse its plaintext
  streams, but it cannot read session keys, skip certificate or hostname
  checks, or change the trust store.
- **Authority is expressed by host name.** Applications ask for a TLS stream to
  `example.com:443`. tls.svc resolves the name, connects and sends that name as
  SNI, then verifies it against the certificate. An application cannot resolve
  one name and verify another. Manifest scopes can name hosts instead of IPv4
  prefixes.
- **There is one policy point.** The trust store, revocation policy, minimum
  versions, audit events and future certificate exceptions all live in one
  place.
- **Any language can use it.** Ada, C, Rust and CCL clients speak IPC. No C FFI
  to SPARKTLS is needed.

### Costs and mitigations

| Risk | Mitigation |
|---|---|
| Every client shares the service's fate. One exploitable bug exposes every client's plaintext. SPARK proves absence of runtime errors, not isolation between clients. | Channel IDs are bound to owner, authority tag and generation, as in netstack. Session state is scrubbed on `Drop` when the client exits. Later option: procmgr starts one instance per principal. |
| One client starves others, since the handshake pool is shared and handshakes are CPU-heavy. | Per-client quotas on in-flight handshakes, established sessions and handshake CPU time, set by manifest and policy. Admission fails with `Quota_Exceeded` before a pool slot is taken. |
| Plaintext is copied twice (application ↔ tls.svc ↔ netstack). | Acceptable for browsing. Use generation-tagged grant buffers as netstack does. Measure before optimizing. |
| Single-threaded handshake CPU. | Measure first. Split into several instances before adding threads. |

The handshake pool size is set by SPARKTLS's `Max_Inflight` constant (16 today)
until upstream makes it configurable. The service divides that pool among its
clients through quotas, so its total admission limit is a service setting, not
a hidden library constant.

### Channel protocol

The protocol copies the netstack channel operations deliberately. NetSurf's
existing async fetcher state machine then works for `https:` with a different
endpoint and an extra connection-info step.

| Operation | Meaning |
|---|---|
| `TLS_OPEN` | Host name, port, profile ID, ALPN list, and a generation-tagged transfer grant. The deferred reply returns an opaque channel ID or a typed failure. |
| `TLS_READ` / `TLS_WRITE` | Plaintext through the transfer grant. Deferred replies, with a read posted before data arrives, as netstack does. |
| `TLS_SHUT` | Sends close_notify. The session is driven until the peer answers or times out, then dropped. |
| `TLS_CLOSE` | Drops the session and scrubs its keys, whatever state it is in. |
| `TLS_INFO` | Negotiated version, cipher suite, ALPN, key-exchange group, and a bounded chain description: subject, issuer, validity, SHA-256 fingerprint. No key material. |
| `TLS_UPGRADE` (stage 2) | Takes over an existing netstack TCP channel for STARTTLS. Needs a new netstack operation that transfers a channel to another holder, keeping the original authority tag. |

Failures are typed, never a bare "error":
`Name_Resolution`, `Connect_Refused`, `Timeout`, `Protocol_Alert`,
`Certificate_Expired`, `Certificate_Not_Yet_Valid`, `Untrusted_Root`,
`Hostname_Mismatch`, `Revoked`, `Weak_Key`, `Clock_Untrusted`,
`Quota_Exceeded` and `Scope_Denied`. Certificate failures carry the same bounded
chain description as `TLS_INFO`, so a browser can show an error page.

### Authority

Add manifest request kind `REQ_TLS`:

- a host pattern, either an exact name or a `*.suffix` wildcard, or the explicit
  `any` for a general-purpose browser;
- a port range;
- a profile. `webpki` is the default and uses `Mode_WebPKI`. A named private
  profile uses `Mode_RFC5280` with a trust store chosen by policy.

IP-literal destinations need an explicit flag in the scope, because they skip
the host-name binding. As with network scopes, a manifest request is not
approval. Approval comes from trusted boot configuration now, and from the
planned installation-approval path later.

tls.svc itself holds a broad outbound TCP scope with DNS permission. Its code
is the only code that uses it, and applications holding `REQ_TLS` gain no raw
TCP authority.

### Trust store

- The bundle is built from a pinned Mozilla/CCADB root set at image build time
  (Nix fetch plus a generator that outputs concatenated DER). It ships in the
  image and is managed by CCL like other image inputs.
- tls.svc loads it with `Cert_Verify.Load_Roots` from a read-only buffer. The
  buffer comes from the filesystem service through a scoped read-only handle
  granted by tls.svc's manifest (decided). Boot-module delivery can be added
  later if the TLS service must start before the filesystem.
- Revocation uses stapled OCSP only, with SPARKTLS's `Soft_Fail` default,
  until certmgr exists. Document this plainly; it matches curl's default
  behaviour.
- Certificate exceptions (proceed despite an untrusted certificate) are **not**
  part of the first version. When added, they are bound to one host and one
  certificate fingerprint, recorded by a trusted service, approved through the
  desktop, and visible for later review.

### Application integration

- **NetSurf:** register the `https:` fetcher only when the tls.svc endpoint is
  present. It never downgrades to HTTP. Error pages are built from the typed
  failure. Redirects remain NetSurf policy, but an HTTPS-to-HTTP redirect
  requires an explicit decision and is never followed silently.
- **wget:** gains `https://` through the same client library.
- **Client library:** `CuBit.TLS` (Ada), with a C header generated from the same
  schema, following the `CuBit.Filesystems` precedent. Parallel hand-written
  constants are not allowed.

### Testing

- Linux-hosted fixtures reached through QEMU user networking:
  - SPARKTLS example servers and OpenSSL `s_server` presenting valid, expired,
    not-yet-valid, wrong-host, untrusted-root, weak-key and revoked-with-staple
    certificates;
  - a server that drops mid-handshake and one that stalls;
  - a downgrade-attempt server.
- Quota exhaustion from one client while a second client completes its
  handshakes.
- Client death mid-handshake and mid-stream: the slot is released, keys are
  scrubbed (checked by inspecting service state), and the channel ID never
  resolves again.
- Real-site checks (a small list of HTTPS sites) as a manual, network-dependent
  lane, separate from deterministic regressions.

## Keys and certificate lifecycle (server direction)

These services come after the client milestone. They are designed now so the
client-certificate path in tls.svc does not need redesigning.

### keystore.svc

Hardware key stores (TPMs, HSMs, smart cards and security keys such as
YubiKeys) are first-class in CuBit, not an add-on to software keys. A fuller
design will follow; this section fixes the constraints the rest of this
roadmap must respect.

- keystore.svc is a **broker over key providers**. Clients name a key by an
  opaque handle and a purpose. They never learn or choose where the key lives.
  Providers:
  - software keys, sealed at rest (the fallback, and for development);
  - TPM 2.0, through a TPM driver found via ACPI (`MSFT0101`);
  - PIV smart cards and security keys, through a USB CCID class driver matched
    by devmgr (the `sparkpiv` crate and SPARKTLS's `tls_yubikey_server` example
    already use a YubiKey PIV slot);
  - network or PCIe HSMs, later.
- **Keys are never exported**, whatever the provider. keystore.svc returns
  signatures (and later decryption and key-agreement results) only, through
  SPARKTLS's external-signing interface (`Config.Sign`, `Set_Identity_Public`)
  on the tls.svc and gateway side.
- **Key provenance is part of the handle's metadata:** where the key was
  generated, whether it can be exported, and hardware attestation where the
  provider offers it. Policy can require hardware backing for a purpose, for
  example "the server identity must be TPM-backed".
- **Removable hardware is normal.** A security key can be unplugged, and a PIN
  can be locked out. Signing requests fail with typed errors (`Key_Absent`,
  `PIN_Required`, `Presence_Required`, `Locked`) instead of hanging. tls.svc
  reports them to its client as distinct TLS failures.
- **PIN entry and touch prompts go through a trusted desktop path.** They are
  never collected by the requesting application.
- **Use policy** decides which principal may sign with which key, for which
  purpose (TLS client, TLS server, ACME account), with rate limits. Every use
  is audited.
- **Sealing at rest.** Software keys are sealed under a TPM-held key when a TPM
  is present. Without one, at-rest protection is an explicit, recorded
  assumption.
- Small, SPARK-first, with no network authority. Provider drivers (TPM, CCID)
  are separate processes with their own device grants. keystore.svc holds only
  their endpoints.

### certmgr.svc

- Holds public certificates and chains, and ACME account state. It holds **no
  private keys**. ACME JWS signatures come from keystore.svc.
- Runs ACME (RFC 8555): order, challenge, finalize (with a CSR signed by
  keystore.svc), install and renew before expiry.
  - Challenge preference: **TLS-ALPN-01** first. It runs through the inbound
    SPARKTLS gateway from [web hosting](web-hosting.md) and uses ALPN, which
    SPARKTLS already supports.
  - **HTTP-01** once the static site path exists.
  - **DNS-01** only with a scoped provider-API integration.
- Fetches CRLs and OCSP responses (SPARKTLS never fetches them itself). It
  supplies staples to the server gateway and CRLs to tls.svc.
- Takes root-store updates as signed packages through the CCL package system.
  It does not download them ad hoc.
- Parses untrusted JSON and DER from the network. **That is why it holds no
  keys.**

### Client certificates

`TLS_OPEN` gains an optional identity reference. tls.svc checks that the
client's policy allows that identity, loads the public certificate from
certmgr.svc, and routes signing to keystore.svc. The application never sees
the key. Mutual TLS for [remote management](remote-management.md) uses the same
path.

## Device discovery and driver matching

### Goal

Once the bootstrap services are running, devmgr should match every discovered
device against a driver catalog. It starts matching drivers with exactly that
device's resources, and reports unmatched devices. A driver whose device is
absent is never started. This fixes the HDA-without-a-controller boot stall by
construction.

### Driver catalog

Each driver's CCL manifest declares **match rules** and **resource requests**:

```lisp
(executable-manifest v1
  (identity "com.cubit.driver.igc")
  (version "0.1.0")
  (driver
    # Illustrative IDs; confirm against lspci on the target.
    (match pci (vendor #x8086) (device #x125C #x125B))   # I226-V, I226-LM
    (resources (bar 0 mmio) (interrupt msix 4) (dma bounded))
    (provides "net.device.v1")))
```

- **Match precedence:** exact vendor and device ID, then subsystem ID, then
  class and prog-if. Ties are a catalog error found at image build time, not
  settled by boot order.
- The CCL image build checks the catalog: rules parse, no two drivers claim
  the same device at the same precedence, and every requested resource kind is
  known.
- **Matching approves nothing.** Resources are granted from the matched
  device's actual BARs and interrupts, never from the manifest's claims.
  Starting a driver from the catalog needs the same image or policy approval
  as other privileged startup.

### Phases

1. **Static catalog, boot-time only.** Keep the existing bootstrap order for
   the services needed to reach storage (storage drivers, filesystem, config,
   procmgr). After that, devmgr walks the PCI inventory, matches the rest, and
   starts drivers. HDA, the NICs, xHCI and GPUs move to this path. The Devices
   app lists every device: matched and running, matched and failed, or
   unmatched.
2. **Failure handling.** A driver that exits or faults marks its device
   quarantined, with bounded restarts and backoff. A failed optional driver
   never blocks boot.
3. **USB.** xhci.drv reports attached devices (class, vendor, product,
   interfaces) to devmgr as typed events. devmgr matches USB-class drivers
   and grants each one an endpoint scoped to its device's interfaces through
   xhci.drv. This is how security keys and smart-card readers (CCID), and a
   later USB Ethernet or Wi-Fi dongle, would attach.
4. **PCIe hotplug and ACPI-described devices** (TPM, embedded controller,
   battery, I2C touchpads). Later, alongside the
   [ACPI userspace](acpi-userspace.md) work.

### Firmware blobs

Some devices, including the Wireless-AC 3165, need firmware uploaded by their
driver at every power-up. The driver requests a **firmware resource** by name
and expected SHA-256 in its manifest. devmgr, or a small firmware store,
supplies a read-only buffer from the image's firmware directory only if the
hash matches. The driver gets no general filesystem access. Firmware files are
explicit local image inputs with their license texts. They are not committed
to the repository, following the local-ROM precedent.

### IOMMU

**Decided:** CuBit implements VT-d DMA confinement for every DMA-capable
driver. Today drivers get real physical addresses (`SYSCALL_ALLOC_DMA`,
`SYSCALL_VIRT_TO_PHYS`) and write them into device descriptors. Every DMA
driver (NVMe, xHCI, HDA, virtio) can therefore reach any physical memory
through its device, and so is trusted like the kernel regardless of running in
userspace. A new NIC driver adds to that existing gap; it does not create it.

The work splits into two parts:

1. **DMA-handle API (before the NUC driver).** Drivers receive opaque device
   addresses for buffers the kernel allocated or pinned for them, and never
   translate virtual addresses themselves. Without an IOMMU, the device address
   is the physical address. With one, it is an I/O virtual address in the
   device's own domain. The NUC driver is written against this API from the
   start, so it needs no change when confinement is turned on. Migrating
   existing drivers off `VIRT_TO_PHYS` is part of this work.
2. **VT-d confinement (a system-wide phase).**
   - Admit the DMAR table through the table-snapshot step of the
     [ACPI userspace plan](acpi-userspace.md). That step needs only static
     tables; the AML interpreter is not a prerequisite.
   - Build per-device domains with kernel-owned I/O page tables and IOTLB
     invalidation. Mapping lifetimes tie into the generation-tagged, pinned
     grant references planned in FS-001: a buffer stays mapped and pinned until
     DMA completes and the mapping is returned.
   - Enable interrupt remapping, so a device cannot forge MSI writes.
   - Honour firmware-reserved regions (RMRRs) with narrow identity mappings,
     recorded as firmware quirks and shown in diagnostics.
   - Develop with QEMU's `intel-iommu` device, then validate on the NUC
     (Alder Lake-N) and the laptop (Skylake). VT-d may need enabling in each
     machine's firmware setup.

Enforcement (I/O page tables, invalidation, interrupt remapping) stays in the
kernel. Discovery and domain assignment policy can live in userspace with the
ACPI service. Until confinement is on, NIC and storage drivers are documented
as trusted with respect to DMA. Confinement is a prerequisite for the
closed-firmware Wi-Fi card.

### Vector state (AVX): planned kernel change

The kernel enables and saves only SSE state (`FXSAVE`/`FXRSTOR`: x87, MMX,
XMM0-15, MXCSR). AVX, AVX2 and AVX-512 state (YMM upper halves, ZMM, opmask)
needs CR4.OSXSAVE, XCR0 and the `XSAVE` family. Plan:

- Processes declare vector-state needs (`avx`, `avx512`) in their manifest.
  The kernel keeps a per-process XCR0 and runs `XSETBV` on a context switch
  only when it differs. Undeclared use faults, so that state never needs
  saving, and AVX-512's large save area and possible clock-speed penalty
  apply only to processes that ask for them.
- Save and restore with `XSAVEC`/`XSAVEOPT` and `XRSTOR`, whose init and
  modified tracking skip unused components.
- No lazy switching via CR0.TS (the LazyFP side channel, CVE-2018-3665). Each
  process starts from the initial state, never another process's registers.

SPARKTLSCrypto already checks OSXSAVE and XCR0, so it starts using AVX paths
automatically once a process is granted AVX.

## NIC drivers

### Network device interface first

Before writing a hardware driver, replace the one-frame-per-IPC driver
interface with `Net.Device.V1`:

- RX and TX descriptor rings in a shared grant owned by netstack, with
  per-ring producer and consumer indexes and batched doorbell IPCs;
- link state, MAC address, MTU and offload capability reports (checksum
  offload optional, and never trusted on receive without validation);
- more than one interface per netstack, with the driver's authority tag bound
  to one interface;
- driver restart that tears the interface down and back up without restarting
  netstack.

Port virtio-net to it first, so the interface is proved out in QEMU before any
hardware driver depends on it. The ring index arithmetic and descriptor
ownership transitions are small enough to prove in SPARK, as the kernel
allocator cores were.

### NUC wired NIC

The exact controller is not known yet. N95 mini-PCs commonly ship either an
Intel I226-V (the Linux `igc` driver) or a Realtek RTL8111H or RTL8125 (Linux
`r8169`). **Run `lspci -nn` from a Linux live USB on the NUC before writing
code.**

QEMU emulates no exact match for either family. Its `igb` and `e1000e` models
share design lineage with `igc` and are useful for ring and interrupt plumbing,
but register-level behaviour must be validated on the NUC. The Realtek chips
have no QEMU model at all.

### Working with a hard-to-reach NUC

Development there uses media carried over by hand, so each hardware run must
bring back as much evidence as possible:

- Persist boot diagnostics and driver logs to a file on the boot USB, or to
  the NUC's NVMe, through logstore. That way the same stick carries the image
  there and the evidence back.
- Keep the on-screen boot panel informative enough to photograph.
- Once the NIC works: add a UDP log sink (a scoped, send-only UDP channel to a
  configured collector) so later runs report over the network. Network boot
  (PXE/iPXE) removes the manual media trip, and belongs with
  [remote management](remote-management.md).

## Wi-Fi (laptop, separate track)

The Wireless-AC 3165 belongs to the iwlwifi 7000 family (firmware file
`iwlwifi-7265D-<api>.ucode`, handled by `iwlmvm` on Linux). The card stores no
usable runtime firmware. The driver must load it at every power-up:

1. parse the firmware file's tagged sections;
2. DMA the code and data sections into the card;
3. wait for the card's "alive" notification;
4. use the firmware's command and notification queues from then on.

The firmware handles calibration, time-critical MAC work, retries, power
saving, scan offload, optional crypto offload and regulatory limits. The host
still implements:

- scan result handling, authentication and association (the job mac80211 does
  on Linux);
- a supplicant for the WPA2-PSK 4-way handshake, with WPA3-SAE later. It runs
  as a separate service (`wlan-auth.svc`) that holds network passphrases, which
  come from keystore.svc or Config.

Prerequisites: the devmgr firmware resource, IOMMU confinement, and
`Net.Device.V1`. The firmware is closed code on a DMA-capable device. CuBit's
claims must treat everything the card returns as untrusted input and its DMA
as confined only to the extent the IOMMU enforces.

**Decided:** the 3165 with its Intel firmware is the first Wi-Fi target.
CuBit does not exclude proprietary firmware or applications. It confines them
through the same capability, manifest and IOMMU boundaries as everything else.
Open-firmware hardware (an AR9271 USB dongle using `ath9k_htc`, or an `ath9k`
PCIe card that needs no firmware) remains an option for later.

## Phases

| Phase | Work | Test venue | Exit evidence |
|---|---|---|---|
| 1 | Netstack UDP scopes and datagram channels (**done 2026-09-23**) | QEMU | Authority denials, reply-source filtering, handle lifetime tests |
| 2 | timesync.svc (SNTP), clock adjustment endpoint, time floor (**done 2026-09-24**) | QEMU with the hosted fixture | Quality transitions, bounded steps, floor enforcement |
| 3 | tls.svc, `REQ_TLS`, trust-store bundle, `CuBit.TLS` | QEMU with hosted fixtures | All typed failure cases, quotas, client-death cleanup |
| 4 | NetSurf and wget HTTPS (**done 2026-09-24** in QEMU; see below) | QEMU, then a manual real-site lane | Milestone criterion 1 |
| 5 | devmgr driver catalog, phases 1–2 | QEMU with varied device sets | No driver starts without its device, and a failed driver doesn't block boot |
| 6 | `Net.Device.V1`, the DMA-handle API, and the virtio-net port | QEMU | Throughput and latency compared with the old interface; no driver translates addresses itself |
| 7 | NUC wired driver | NUC | Milestone criterion 2 |
| 8 | NTS | QEMU with the hosted NTS fixture | `Network_Authenticated` quality |
| 9 | ACPI table snapshots (DMAR), VT-d domains, interrupt remapping; existing drivers moved to DMA handles | QEMU (`intel-iommu`), then NUC and laptop | Out-of-range DMA and forged MSIs fault and are attributed to the device |
| 10 | Firmware resource, iwlwifi bring-up, supplicant | Laptop | Milestone criterion 3 |
| 11 | keystore.svc, certmgr.svc, client certificates | QEMU | Keys never leave keystore; ACME against a local test CA (Pebble) |

Phases 1–4 need no hardware and can proceed while the NUC is being set up.
Phase 5 is independent and can run in parallel. Phase 7 can start once
phase 6 lands and the NUC controller is identified.

## Verification boundaries

Following the project's rule of separating proof from testing:

- **Proof targets:** the time-sample
  validation and time-floor core, `REQ_TLS` scope matching (host pattern, port,
  profile), tls.svc channel-handle resolution and quota admission, device
  catalog matching and precedence, and `Net.Device.V1` ring index arithmetic.
- **Relied on from SPARKTLS:** its proofs cover the library. CuBit's
  integration (callbacks, buffer handoff, service event loop) is CuBit code
  and gets tested, not proved, unless it is extracted into SPARK cores.
- **Trusted and tested only:** `rdtsc` jitter quality on each machine, NIC and Wi-Fi hardware behaviour, Wi-Fi firmware, the IOMMU
  hardware, and remote NTP and CA infrastructure.
- **Linux-hosted:** the TLS and NTP fixture servers are Linux-hosted test
  infrastructure. Passing against them shows CuBit's behaviour against those
  fixtures, not live interoperability. The real-site lane is reported
  separately.

## Decisions

Decided (2026-09-23):

- NTS-KE: timesync.svc links SPARKTLS directly and validates its certificate
  using the RTC and SNTP cross-check described above.
- Trust-store delivery: a scoped read-only file handle.
- Wi-Fi: the Wireless-AC 3165 with its proprietary firmware, confined.
- IOMMU: VT-d confinement for all DMA drivers. The NUC driver ships first on
  the DMA-handle API, and confinement follows as a system-wide phase. The
  Wi-Fi card requires confinement.
- TLS service instances: CuBit may run several TLS services. tls.svc is
  the default outbound "securely connect" provider, not a mandatory chokepoint.
  The inbound SPARKTLS gateway from [web hosting](web-hosting.md) is the
  default "serve content" provider. Separate instances, per principal or per
  purpose, are a launch-policy choice. tls.svc keeps all ownership checks per
  client and never assumes it is the only instance.
- Applications may terminate TLS themselves (by linking SPARKTLS or another
  library) when they are granted raw TCP scopes. That is a distinct, broader
  grant than `REQ_TLS`: the application is then responsible for its own
  certificate validation, and approval and inspection tools must show that
  it manages its own TLS.

No open decisions remain in this document. Later work builds on these
services:

- **Developer API.** Two calls, "securely connect to a name" and "serve content
  under an identity". Applications get TLS, certificates, renewal and
  revocation without bundling a TLS library or handling keys.
- **Networked IPC.** Remote CuBit endpoints over mutual TLS, with peer
  identities from keystore.svc and certmgr.svc. Kernel capabilities do not
  cross machines. A proxy service exports specific, attenuated endpoints and
  maps remote calls onto local ones, as sketched in
  [remote management](remote-management.md).
