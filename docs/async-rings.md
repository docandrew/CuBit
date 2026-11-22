# Async rings: shared-memory data planes between services

Status: design (2026-09-28). Implementation is in progress; see "Plan"
for what exists.

IPC is the control plane: rare, authority-bearing requests. Bulk and
high-rate work goes over rings in memory that the two processes share.
Signals are sent only when the other side armed a wake before sleeping
(docs/netstack-redesign.md, "Control plane and data plane"). This
document defines one set of proved ring primitives for every service
that needs such a data plane: networking, storage and filesystems,
and later display and audio.

## Layers

1. **Byte ring** (`CuBit.Channel_Rings`, exists). A stream of bytes, for
   TCP data. It has no record boundaries.
2. **Slot ring** (`CuBit.Slot_Rings`, new, generic). A ring of
   fixed-size elements: frames, submission entries, completion entries.
   - The formals are the element type and the number of slots, which must
     be a power of two (`Slot_Bits`).
   - It replaces `Frame_Ring`'s hand-written bookkeeping in virtio-net
     and netstack.
3. **Submission/completion pair** (`CuBit.Submission_Queues`, new,
   generic). Two slot rings, one of requests and one of completions,
   like io_uring and NVMe.
   - The formals are each service's opcode, argument and result types.
   - The rings and signals are shared and proved once. Each service
     proves its own decoder and admission rule.

Hardware rings (virtqueues, NVMe, xHCI, GuC) keep their own formats,
which the device defines. Only the arithmetic may be shared: a used
index accepted only when it moves forward, with positions reduced to the
queue.

## Rules every ring follows

- **Free-running 32-bit indices.** An element's slot is its index modulo
  the slot count. The count is a power of two, so it divides 2 ** 32 and
  slots stay consistent across wrap-around.
  - Proved: indices less than a ring apart never share a slot.
  - Finding (2026-09-28): `Frame_Ring` used `2 ** k - 1` slots, because
    slot 0 held the counts. There, frames in flight across the 2 ** 32
    wrap could share a slot. The slot ring moves the counts to a header
    and requires a power of two.
- **Each side trusts only its own index.** It reads the peer's index once
  per step and accepts it only if it moves forward and keeps the fill
  within the ring. A peer that writes garbage can garble only its own
  traffic.
- **Snapshot only what decisions depend on.** Anything that is checked
  and then used, is read once into private memory first, so a peer
  cannot change it between check and use. That covers:
  - a submission entry (tens of bytes);
  - an index or a length word;
  - the protocol headers being parsed.

  Payload that is never used to decide anything (file contents, TCP data)
  stays in place. It is read exactly once, straight to its destination
  (a receive queue, the app's buffer, a DMA buffer), a copy that happens
  anyway. A peer that changes it meanwhile corrupts only its own data.
- **A header apart from the slots.** For each direction:
  - the producer's index;
  - a "space wanted" word the producer sets before it waits for room;
  - the consumer's index on its own cache line;
  - a wake word the consumer sets before it sleeps.

  A producer signals only if the wake word is armed. Memory ordering is
  the callers' job (volatile accesses and fences), as for the byte ring.
- **Bounded.** Sizes are fixed when the ring is set up. Nothing grows.

## Submission and completion entries

A submission entry holds:
- an opcode (the service's enumeration, rejected if unknown);
- flags;
- a token, which the client chooses and the completion returns;
- a handle naming an object the queue's owner already holds (a
  connection, a file);
- arguments.

Data buffers are named as (registered buffer, offset, length). A
registered buffer is a grant the owner lent when it set up the queue.
Entries never hold pointers or addresses, and the service checks every
reference against the registered extent.

A completion entry holds the token, a status (a named enumeration) and
a result: a length, a new handle, or a position.

There is no ordering promise between entries unless the opcode is a
barrier. Linked entries (io_uring's `IOSQE_IO_LINK`) come later if
needed.

## Authority

- **Setup is control plane.** A queue pair is created by an IPC request
  that the service admits against the caller's authority (network scope,
  filesystem grant). Its memory is the caller's own grant. The queue is
  bound to that one owner for its lifetime.
- **Every entry is admitted.** Each submission passes the same admission
  rule as the equivalent IPC request, preferably the same proved function
  called from both paths. A ring grants no authority of its own; it is a
  faster pipe for authority the owner already holds.
- **Handles are the owner's.** A handle in an entry resolves only within
  the queue owner's table (generation-checked), so a client cannot name
  another client's objects.
- **Exit.** When the owner exits, its queues and registered buffers are
  released. The service never touches them afterwards.
- **The kernel only wakes.** It does not parse rings. The kernel's
  completion queue remains for asynchronous IPC replies (control plane).

## Plan and status

1. **Done (2026-09-28):** `CuBit.Slot_Rings`, generic and proved
   (tests/channel-rings). `Frame_Ring` moved onto it
   (`CuBit.Frame_Rings`): a header page, power-of-two slots, and one
   layout package shared by virtio-net and netstack. It passes the
   native network tests and the benchmark.

   Still to do: netstack snapshots only a frame's headers and moves the
   payload straight from the slot into the connection's queue. Today it
   copies whole frames, because its parsers read the frame as one array.
2. **Done:** `CuBit.Submission_Queues`, generic and proved. The service
   takes a request only while it holds a completion slot for it, so
   answers never wait.
3. **In test:** netstack control queues (`OP_NET_QUEUE`,
   `CuBit.Net_Control_Queues`, cubit_net_channel.h).
   - OPEN and SHUT are queue entries, admitted like their IPC twins
     against the queue's bound authority tag.
   - Answers complete the owner's WAIT (`Wait_Answers`).
   - netstack arms a wake word before sleeping; clients KICK only when it
     is armed.
   - libc submits through a queue per scope endpoint, and uses messages
     when netstack has no queue to give.
4. **Storage and filesystem:** a queue pair per client with registered
   transfer buffers, many requests in flight (today `Storage_Channel`
   has one), down to NVMe's own queues. The benchmark against Linux
   comes first (tests/fs-bench).
