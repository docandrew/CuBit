-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- @summary
-- CuBitOS IPC
--
-- Multi-word register-based IPC (L4/seL4 style). Messages carry a tag
-- (opcode, length, flags, authority tag), a kernel-stamped capability authority tag, and up to
-- 4 64-bit data words, for a total of 48 bytes.
--
-- IPC lock ordering (acquire in this order, never reverse):
--   1. mailtab(pid).lock    (ring, completions, pending requests, request IDs,
--                          reply-slot changes and authorized cspace edits)
--      Two-mailbox publication acquires distinct PIDs in ascending order.
--      Explicit reply retirement releases the caller lock before locking its
--      target. PID-based reply selection holds both, then drops the caller
--      lock before the target-locked completion/handoff. Self-reply locks once.
--   2. Process.lock         (global process table)
--   3. individual process queue locks (ready, sleep, send, receive)
-- Queue locks are leaves: release the sleep lock before acquiring a ready
-- lock. See docs/kernel-locking.md for allocator/grant dependencies and the
-- process-lifetime / mailbox teardown protocol in kernel-process-retirement.md.
-------------------------------------------------------------------------------
with Capabilities;
with Memory_Grants;

package Process.IPC is
    -- Request/mixed receive variants share a mailbox-locked round-robin over
    -- queued messages, blocked synchronous senders, and persistent IRQ work.
    -- Service-only polls omit IRQs and preserve unsolicited events. Under
    -- continuous eligible traffic, neither synchronous calls nor queued
    -- submissions may monopolize receive selection. This is a dequeue-count
    -- fairness bound, not a scheduler/service-time or p99 latency guarantee.

    -- Victim is closed, off CPU and exclusively claimed by the reaper.
    -- Acquires each mailbox before Process.lock, never the reverse.
    procedure retireMailboxes (pid : ProcessID);
    procedure sendRetirementEvent
      (dest : ProcessID; generation : Capabilities.Generation; msg : Message);

    ---------------------------------------------------------------------------
    -- send
    -- Synchronous send: deliver a message to a process' mailbox. If a
    -- receiver is already waiting, hand the message off immediately.
    -- Otherwise enqueue as a sender and block.
    --
    -- The caller blocks in WAITINGFORREPLY until the receiver calls reply().
    -- @return the reply message tag.
    ---------------------------------------------------------------------------
    function send (dest : ProcessID; msg : Message;
                   expectedGeneration : Capabilities.Generation := 0) return MessageTag;

    ---------------------------------------------------------------------------
    -- sendEvent
    -- Non-blocking version of send, intended for interrupts. Sends a message
    -- to the destination process but does not block whatever process was
    -- active when the interrupt occurred.
    ---------------------------------------------------------------------------
    procedure sendEvent (dest : ProcessID; msg : Message);

    --  Non-blocking event publication with explicit backpressure. accepted is
    --  False when the destination is unavailable or its bounded event lane is
    --  full. Producers of stateful streams use this result to mark their next
    --  accepted report as a resynchronization snapshot.
    procedure trySendEvent (dest     : ProcessID;
                            msg      : Message;
                            accepted : out Boolean;
                            expectedGeneration : Capabilities.Generation := 0);

    -- Publish a coalescible, persistent device-work doorbell. Unlike
    -- sendEvent, this cannot be lost because a mailbox ring is full.
    procedure notifyIRQ (dest : ProcessID);

    ---------------------------------------------------------------------------
    -- receive
    -- Receive a message from one's mailbox. Block if no message available.
    -- On return, from contains the sender PID and msg contains the message.
    ---------------------------------------------------------------------------
    procedure receive (from : out ProcessID; msg : out Message);

    -- Wait without consuming messages or completions (or reply authority).
    -- 1 = work available, 0 = deadline, Last = closed mailbox.
    function waitForActivityUntil (deadlineMs : Unsigned_64)
      return Unsigned_64;

    --  Block until any IPC arrives or the absolute monotonic millisecond
    --  deadline is reached. Unlike a userspace sleep/poll loop, publication
    --  wakes the receiver immediately. received is False only on timeout.
    procedure receiveUntil
        (deadlineMs : in  Unsigned_64;
         from       : out ProcessID;
         msg        : out Message;
         received   : out Boolean);

    --  Called once per monotonic millisecond by the BSP timer. Timed receivers
    --  are removed from their one mailbox queue and made runnable when due.
    procedure expireReceiveDeadlines (nowMs : Unsigned_64);

    ---------------------------------------------------------------------------
    -- receiveEvent
    -- Block until an event is received, or return the event if one is already
    -- waiting. If multiple events are sent to this process between
    -- receiveEvent calls, only the latest one will be delivered.
    ---------------------------------------------------------------------------
    function receiveEvent return Message;

    ---------------------------------------------------------------------------
    -- receiveEventNB
    -- Non-blocking event receive. Pops from the event ring buffer.
    -- @param msg   - the event message (valid only if found is True)
    -- @param found - True if an event was available
    ---------------------------------------------------------------------------
    procedure receiveEventNB (msg : out Message; found : out Boolean);

    ---------------------------------------------------------------------------
    -- reply
    -- Reply to a process who called send() or submit() and unblock it.
    -- Dual-path: if sender is in WAITINGFORREPLY (sync send), stores reply
    -- and wakes them directly. If sender used submit() (async), enqueues a
    -- CompletionEntry matched by the reply cap's request ID and wakes them if
    -- in WAITINGFORCOMPLETION.
    -- @return 1 on success.
    ---------------------------------------------------------------------------
    function reply (replyTo : ProcessID; msg : Message) return Unsigned_64;

    ---------------------------------------------------------------------------
    -- replyCap
    -- Reply using a specific CAP_REPLY slot. This lets deferred-reply servers
    -- choose the exact saved reply authority/request ID to complete.
    -- A selected reply is consumed even if the peer is dead/stale or delivery
    -- otherwise fails. A non-reply slot is never changed by this operation.
    -- @return 1 on success.
    ---------------------------------------------------------------------------
    function replyCap
        (capSlot : Capabilities.CapabilitySlot;
         msg     : Message) return Unsigned_64;

    ---------------------------------------------------------------------------
    -- replyWait
    -- Reply then receive in one syscall; each operation has its own
    -- mailbox-locked publication boundary (not one atomic transaction).
    -- Replies to replyTo, then blocks receiving the next message.
    -- Avoids a second userspace syscall entry for server loops.
    -- @param replyTo  - PID to reply to
    -- @param replyMsg - message to send as reply
    -- @param from     - out: sender PID of next received message
    -- @param msg      - out: next received message
    ---------------------------------------------------------------------------
    procedure replyWait (replyTo  : in  ProcessID;
                         replyMsg : in  Message;
                         from     : out ProcessID;
                         msg      : out Message);

    ---------------------------------------------------------------------------
    -- receiveServiceRequestNB
    -- Non-blocking receive for the service-request lane only.
    --
    -- This procedure deliberately ignores events even though they share the
    -- same underlying ring. It may return:
    --   * synchronous send/call requests, which mint a reply capability;
    --   * async requests submitted with a completion token, which mint a
    --     reply capability carrying the async request ID;
    --   * one-way service messages, which have a sender but do not mint reply
    --     authority.
    --
    -- Use this for ordinary server dispatch loops. Input, IRQ, lifecycle, and
    -- other unsolicited traffic must be received through the event lane.
    ---------------------------------------------------------------------------
    procedure receiveServiceRequestNB (from  : out ProcessID;
                                       msg   : out Message;
                                       found : out Boolean);

    ---------------------------------------------------------------------------
    -- receiveAnyIpcNB
    -- Non-blocking mixed receive.
    --
    -- This is the explicit footgun. It may consume service requests, events,
    -- and one-way messages from the unified ring. Callers must
    -- inspect the returned message and sender and perform their own dispatch.
    --
    -- Prefer receiveServiceRequestNB, receiveEventNB, or completion polling
    -- unless the caller is intentionally implementing a mixed IPC event loop.
    ---------------------------------------------------------------------------
    procedure receiveAnyIpcNB (from  : out ProcessID;
                               msg   : out Message;
                               found : out Boolean);

    ---------------------------------------------------------------------------
    -- Async I/O Primitives
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- waitCompletion
    -- Block until at least minWait completions are available, then drain
    -- up to maxEntries from the caller's completion queue.
    -- @param entries     - output array of completion entries
    -- @param maxEntries  - max completions to drain
    -- @param minWait     - minimum completions before returning (blocks if fewer)
    -- @param numReturned - actual number of completions returned
    ---------------------------------------------------------------------------
    procedure waitCompletion (entries     : out CompletionRing;
                              maxEntries  : in  Natural;
                              minWait     : in  Natural;
                              numReturned : out Natural);

    ---------------------------------------------------------------------------
    -- pollCompletion
    -- Non-blocking single completion check.
    -- @param result - the completion entry if found
    -- @param found  - True if a completion was available
    ---------------------------------------------------------------------------
    procedure pollCompletion (result : out CompletionEntry;
                              found  : out Boolean);

    ---------------------------------------------------------------------------
    -- Shared Memory Grant Operations
    ---------------------------------------------------------------------------

    --  True when any byte in a page range lies in the virtual aperture used
    --  for received grants. Such pages are borrowed, not owned, and cannot be
    --  passed onward through ordinary createGrant. Future zero-copy chains use
    --  an explicit derived-loan operation with parent lifetime tracking.
    function overlapsGrantRegion (localAddr : System.Address;
                                  numPages  : Natural) return Boolean
        with Global     => null;

    ---------------------------------------------------------------------------
    -- createGrant
    -- Map pages from the caller's address space into grantee's address space.
    -- The caller (granter) owns the grant and can revoke it later.
    -- Borrowed pages in GRANT_REGION_BASE..GRANT_REGION_END are rejected.
    -- @param grantee   - PID of the process to grant access to
    -- @param localAddr - page-aligned virtual address in caller's space
    -- @param numPages  - number of 4K pages to grant
    -- @param perm      - read-only or read-write access
    -- @param id        - output: ID of the created grant
    -- @param success   - output: True on success
    ---------------------------------------------------------------------------
    procedure createGrant (grantee   : in  ProcessID;
                           localAddr : in  System.Address;
                           numPages  : in  Natural;
                           perm      : in  GrantPermission;
                           id        : out Natural;
                           success   : out Boolean;
                           expectedGeneration : Capabilities.Generation := 0);

    ---------------------------------------------------------------------------
    -- revokeGrant
    -- Unmap granted pages from the grantee's address space.
    -- Only the granter (caller) can revoke.
    -- @param id - ID of the grant to revoke
    ---------------------------------------------------------------------------
    procedure revokeGrant (id : GrantID);

    ---------------------------------------------------------------------------
    -- revokeAllGrants
    -- Revoke all active grants owned by the specified process.
    -- Called during process kill() to prevent dangling mappings.
    -- @param pid - PID of the process whose grants should be revoked
    ---------------------------------------------------------------------------
    procedure revokeAllGrants (pid : ProcessID);

    ---------------------------------------------------------------------------
    -- revokeAllGrantsTo
    -- Invalidate every grant whose grantee is the specified process. Called
    -- during process death so an owner cannot retain metadata naming a PID
    -- that may later be reused for an unrelated process.
    ---------------------------------------------------------------------------
    procedure revokeAllGrantsTo (pid : ProcessID);

    ---------------------------------------------------------------------------
    -- getOwnedGrantGeneration
    -- Return the generation for an active grant slot owned by the caller.
    ---------------------------------------------------------------------------
    procedure getOwnedGrantGeneration
      (slot       : Memory_Grants.Global_Slot;
       generation : out Memory_Grants.Grant_Generation;
       success    : out Boolean);

    ---------------------------------------------------------------------------
    -- acquireGrant
    -- Authoritatively validate and pin a generation-tagged grant for the
    -- current grantee.  The mapping remains valid until returnGrant.
    ---------------------------------------------------------------------------
    procedure acquireGrant
      (reference     : Memory_Grants.Reference;
       expectedOwner : ProcessID;
       byteOffset    : Unsigned_64;
       byteLength    : Unsigned_64;
       requiredWrite : Boolean;
       mappedAddress : out System.Address;
       success       : out Boolean);

    procedure returnGrant
      (reference : Memory_Grants.Reference;
       success   : out Boolean);

    procedure revokeGrantReference
      (reference : Memory_Grants.Reference;
       success   : out Boolean);

    -- Called by process teardown.  When acquired grants remain, retain the
    -- PID until teardown is complete and the final acquisition is returned.
    procedure prepareGrantProtectedTeardown
      (pid         : ProcessID;
       pidReusable : Boolean;
       deferred    : out Boolean);

    procedure finishGrantProtectedTeardown (pid : ProcessID);

    -- Release DMA blocks owned by a process.  Teardown calls this immediately
    -- when no grant is acquired, or after the final acquisition is returned.
    procedure releaseDMAAllocations (pid : ProcessID);

    ---------------------------------------------------------------------------
    -- Capability-Aware IPC
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- capSend
    -- Resolve the endpoint capability at capSlot in the caller's cap table,
    -- stamp the message authority tag from the capability, and perform a synchronous
    -- send to the resolved destination.
    -- @return the reply message tag (NULL_TAG on capability error).
    ---------------------------------------------------------------------------
    function capSend (capSlot : Capabilities.CapabilitySlot;
                      msg     : Message) return MessageTag;

    ---------------------------------------------------------------------------
    -- capCall
    -- Like capSend but writes the full reply message back via pointer.
    -- Resolves endpoint cap, stamps authority tag, sends, returns reply tag.
    -- The caller should read the full reply from proctab(pid).replyMsg.
    -- @return the reply message tag (NULL_TAG on capability error).
    ---------------------------------------------------------------------------
    function capCall (capSlot : Capabilities.CapabilitySlot;
                      msg     : Message) return MessageTag;

    ---------------------------------------------------------------------------
    -- capSubmit
    -- Resolve endpoint capability, stamp authority tag, perform async submit.
    -- @return True on success, False on capability error or mailbox full.
    ---------------------------------------------------------------------------
    function capSubmit (capSlot : Capabilities.CapabilitySlot;
                        msg     : Message;
                        token   : Unsigned_64) return Boolean;

    ---------------------------------------------------------------------------
    -- Supervisor Notification
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- notifySupervisor
    -- Send a non-blocking fault event to the supervisor of the given process.
    -- Safe to call before acquiring Process.lock (respects lock ordering).
    -- @param pid        - offending process
    -- @param faultLabel - EVENT_CAP_FAULT or EVENT_PROCESS_FAULT
    -- @param detail0    - syscall number or exception vector
    -- @param detail1    - what was attempted (arg0 or fault address)
    -- @param detail2    - additional context (arg1 or RIP)
    ---------------------------------------------------------------------------
    procedure notifySupervisor (pid        : ProcessID;
                                faultLabel : Unsigned_32;
                                detail0    : Unsigned_64;
                                detail1    : Unsigned_64;
                                detail2    : Unsigned_64);

end Process.IPC;
