-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- @summary
-- CuBitOS IPC
--
-- Multi-word register-based IPC with proper send-first/receive-first
-- handling, async submit/completion, and shared memory grants.
-- See process-ipc.ads for lock ordering documentation.
-------------------------------------------------------------------------------
with BuddyAllocator;
with Capabilities.Operations;
with Config;
with IPI;
with Memory_Grants;
with PerCPUData;
with Process.Queues;
with Process_Lifetime;
with Time;
with TLB_Shootdown;
with Util;
with Virtmem;
with x86;

use type Capabilities.CapabilityType;
use type Capabilities.Operations.OperationStatus;
use type Memory_Grants.Return_Result;
use type Memory_Grants.Revocation_Result;

-- Ada implementation: custom storage, address overlays or live context state.
-- Only separately annotated SPARK policy/state routines carry proof obligations.
package body Process.IPC is

    ---------------------------------------------------------------------------
    -- getReceiver
    -- Determine which mailbox to use for receive operations. If the caller
    -- is a thread, use the parent's mailbox.
    ---------------------------------------------------------------------------
    function getReceiver (pid : ProcessID) return ProcessID
    is
    begin
        if proctab(pid).isThread then
            return getParent (pid);
        else
            return pid;
        end if;
    end getReceiver;

    ---------------------------------------------------------------------------
    -- Async I/O Helpers
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- enqueueCompletion
    -- Add a completion entry to a process' completion queue.
    -- Caller must hold mailtab(owner).lock.
    -- @return True if enqueued, False if queue is full.
    ---------------------------------------------------------------------------
    procedure enqueueCompletion (owner   : in  ProcessID;
                                 item    : in  CompletionEntry;
                                 success : out Boolean)

    is
        cq : CompletionQueue renames completionTab(owner);
    begin
        if mailtab(owner).closed or else cq.count >= COMPLETION_QUEUE_SIZE then
            success := False;
            return;
        end if;

        cq.ring(cq.tail) := item;
        cq.tail  := (cq.tail + 1) mod COMPLETION_QUEUE_SIZE;
        cq.count := cq.count + 1;
        success  := True;
    end enqueueCompletion;

    ---------------------------------------------------------------------------
    -- dequeueCompletion
    -- Remove a completion entry from a process' completion queue.
    -- Caller must hold mailtab(owner).lock.
    ---------------------------------------------------------------------------
    procedure dequeueCompletion (owner   : in  ProcessID;
                                 item    : out CompletionEntry;
                                 success : out Boolean)

    is
        cq : CompletionQueue renames completionTab(owner);
    begin
        if cq.count = 0 then
            item    := NULL_COMPLETION;
            success := False;
            return;
        end if;

        item     := cq.ring(cq.head);
        cq.ring(cq.head) := NULL_COMPLETION;
        cq.head  := (cq.head + 1) mod COMPLETION_QUEUE_SIZE;
        cq.count := cq.count - 1;
        success  := True;
    end dequeueCompletion;

    ---------------------------------------------------------------------------
    -- findAndRemovePending
    -- Scan the sender's pending requests for a specific kernel request ID.
    -- Returns the token and removes the entry (swap-remove). A zero request ID
    -- falls back to the legacy destination match for kernel/internal replies.
    ---------------------------------------------------------------------------
    procedure findAndRemovePending (sender  : in  ProcessID;
                                    replier : in  ProcessID;
                                    requestId : in Unsigned_64;
                                    token   : out Unsigned_64;
                                    found   : out Boolean)

    is
    begin
        found := False;
        token := 0;

        for i in 0 .. proctab(sender).numPending - 1 loop
            if (requestId /= NO_REQUEST_ID and then
                proctab(sender).pendingRequests(i).requestId = requestId)
               or else
               (requestId = NO_REQUEST_ID and then
                proctab(sender).pendingRequests(i).dest = replier)
            then
                token := proctab(sender).pendingRequests(i).token;

                -- Swap-remove: replace with last entry
                proctab(sender).numPending := proctab(sender).numPending - 1;

                if i < proctab(sender).numPending then
                    proctab(sender).pendingRequests(i) :=
                        proctab(sender).pendingRequests(proctab(sender).numPending);
                end if;

                proctab(sender).pendingRequests(proctab(sender).numPending) :=
                    (NO_PROCESS, NO_REQUEST_ID, 0);

                found := True;
                return;
            end if;
        end loop;
    end findAndRemovePending;

    ---------------------------------------------------------------------------
    -- consumeReplyAuthority
    -- Validate and consume the caller's one-use reply cap for replyTo. Returns
    -- the request ID attached to that reply authority.
    ---------------------------------------------------------------------------
    procedure consumeReplyAuthority
        (caller    : in  ProcessID;
         replyTo   : in  ProcessID;
         requestId : out Unsigned_64;
         ok        : out Boolean)

    is
        cap        : Capabilities.Capability;
        foundSlot  : Capabilities.CapabilitySlot :=
            Capabilities.REPLY_CAP_SLOT;
    begin
        requestId := NO_REQUEST_ID;
        ok        := False;

        if proctab(caller).mode = KERNEL then
            ok := True;
            return;
        end if;

        -- Fast path: check well-known slot 63.
        cap := proctab(caller).caps(Capabilities.REPLY_CAP_SLOT);
        if cap.capType = Capabilities.CAP_REPLY
           and then cap.object.ref = Unsigned_64(replyTo)
           and then cap.gen = proctab(replyTo).capGeneration
        then
            ok := True;
            requestId := cap.object.param;
        else
            -- Slow path: iterate only deferred reply cap slots via bitmap.
            bitmapScan : declare
                remaining : Unsigned_64 := proctab(caller).deferredReplyCaps;
                s : Natural;
            begin
                while remaining /= 0 loop
                    s := Util.getFirstSetBit (remaining);
                    cap := proctab(caller).caps(s);
                    if cap.capType = Capabilities.CAP_REPLY
                       and then cap.object.ref = Unsigned_64(replyTo)
                       and then cap.gen = proctab(replyTo).capGeneration
                    then
                        ok := True;
                        foundSlot := s;
                        requestId := cap.object.param;
                        exit;
                    end if;

                    remaining := remaining and (remaining - 1);
                end loop;
            end bitmapScan;
        end if;

        if ok then
            Capabilities.Operations.takeReplyCap
              (table => proctab(caller).caps,
               slot  => foundSlot,
               cap   => cap,
               taken => ok);

            if not ok then
                requestId := NO_REQUEST_ID;
                return;
            end if;

            proctab(caller).deferredReplyCaps :=
                proctab(caller).deferredReplyCaps and
                not Shift_Left (Unsigned_64'(1), foundSlot);
        end if;
    end consumeReplyAuthority;

    ---------------------------------------------------------------------------
    -- Unified Ring Buffer Helpers
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- takeIRQDoorbell
    -- Consume the persistent device-work notification for owner. The caller
    -- holds mailtab(owner).lock. The message deliberately matches the legacy
    -- IRQ event payload while transport no longer depends on ring capacity.
    ---------------------------------------------------------------------------
    procedure takeIRQDoorbell (owner   : in  ProcessID;
                               item    : out RingEntry;
                               success : out Boolean)

    is
    begin
        if not proctab(owner).irqNotificationPending then
            item := NULL_RING_ENTRY;
            success := False;
            return;
        end if;

        proctab(owner).irqNotificationPending := False;
        item :=
          (msg       =>
             (tag      => (label => 1, length => 0, flags => 0, badge => 0),
              capBadge => 0,
              words    => (others => 0)),
           sender    => NO_PROCESS,
           kind      => RING_EVENT,
           requestId => NO_REQUEST_ID);
        success := True;
    end takeIRQDoorbell;

    ---------------------------------------------------------------------------
    -- enqueueRing
    -- Push an entry into a mailbox's unified ring buffer.
    -- Caller must hold mailtab(owner).lock.
    -- @return True if enqueued, False if queue is full (entry dropped).
    ---------------------------------------------------------------------------
    procedure enqueueRing (owner   : in  ProcessID;
                           item    : in  RingEntry;
                           success : out Boolean)

    is
        r : MessageRing renames mailtab(owner).ring;
    begin
        if mailtab(owner).closed or else r.count >= RING_SIZE then
            success := False;
            return;
        end if;

        r.entries(r.head) := item;
        r.head  := (r.head + 1) mod RING_SIZE;
        r.count := r.count + 1;
        success := True;
    end enqueueRing;

    ---------------------------------------------------------------------------
    -- dequeueRing
    -- Pop an entry from a mailbox's unified ring buffer.
    -- Caller must hold mailtab(owner).lock.
    ---------------------------------------------------------------------------
    procedure dequeueRing (owner   : in  ProcessID;
                           item    : out RingEntry;
                           success : out Boolean)

    is
        r : MessageRing renames mailtab(owner).ring;
    begin
        if r.count = 0 then
            item    := NULL_RING_ENTRY;
            success := False;
            return;
        end if;

        item     := r.entries(r.tail);
        r.entries(r.tail) := NULL_RING_ENTRY;
        r.tail  := (r.tail + 1) mod RING_SIZE;
        r.count := r.count - 1;
        success := True;
    end dequeueRing;

    ---------------------------------------------------------------------------
    -- dequeueRingKind
    -- Remove the first ring entry matching a specific kind without disturbing
    -- older entries of other kinds. Caller must hold mailtab(owner).lock.
    ---------------------------------------------------------------------------
    procedure dequeueRingKind (owner   : in  ProcessID;
                               kind    : in  RingEntryKind;
                               item    : out RingEntry;
                               success : out Boolean)

    is
        r     : MessageRing renames mailtab(owner).ring;
        idx   : RingIndex;
        next  : RingIndex;
        cur   : RingIndex;
    begin
        if r.count = 0 then
            item    := NULL_RING_ENTRY;
            success := False;
            return;
        end if;

        idx := r.tail;
        for n in 0 .. r.count - 1 loop
            if r.entries(idx).kind = kind then
                item := r.entries(idx);

                cur := idx;
                if n < r.count - 1 then
                    for m in n .. r.count - 2 loop
                        next := (cur + 1) mod RING_SIZE;
                        r.entries(cur) := r.entries(next);
                        cur := next;
                    end loop;
                end if;

                r.head := (r.head + RING_SIZE - 1) mod RING_SIZE;
                r.entries(r.head) := NULL_RING_ENTRY;
                r.count := r.count - 1;
                success := True;
                return;
            end if;

            idx := (idx + 1) mod RING_SIZE;
        end loop;

        item    := NULL_RING_ENTRY;
        success := False;
    end dequeueRingKind;

    ---------------------------------------------------------------------------
    -- dequeueRingServiceRequest
    -- Remove the oldest service-request entry from the unified ring.
    --
    -- The ring is shared by several semantic lanes for cache locality and a
    -- compact mailbox representation. Service code, however, must not consume
    -- unsolicited events while it is polling for client work. This helper keeps
    -- the internal ring unified while making the public receive path typed.
    --
    -- Request-like entries are:
    --   RING_SYNC          : a synchronous send/call that expects reply().
    --   RING_ASYNC_REQUEST : submit() with a completion token; reply()
    --                        completes the caller's async request.
    --   RING_ONEWAY        : fire-and-forget service traffic; no reply cap.
    ---------------------------------------------------------------------------
    procedure dequeueRingServiceRequest (owner   : in  ProcessID;
                                         item    : out RingEntry;
                                         success : out Boolean)

    is
        r     : MessageRing renames mailtab(owner).ring;
        idx   : RingIndex;
        next  : RingIndex;
        cur   : RingIndex;
        isRequest : Boolean;
    begin
        if r.count = 0 then
            item    := NULL_RING_ENTRY;
            success := False;
            return;
        end if;

        idx := r.tail;
        for n in 0 .. r.count - 1 loop
            isRequest :=
                r.entries(idx).kind = RING_SYNC or else
                r.entries(idx).kind = RING_ASYNC_REQUEST or else
                r.entries(idx).kind = RING_ONEWAY;

            if isRequest then
                item := r.entries(idx);

                cur := idx;
                if n < r.count - 1 then
                    for m in n .. r.count - 2 loop
                        next := (cur + 1) mod RING_SIZE;
                        r.entries(cur) := r.entries(next);
                        cur := next;
                    end loop;
                end if;

                r.head := (r.head + RING_SIZE - 1) mod RING_SIZE;
                r.entries(r.head) := NULL_RING_ENTRY;
                r.count := r.count - 1;
                success := True;
                return;
            end if;

            idx := (idx + 1) mod RING_SIZE;
        end loop;

        item    := NULL_RING_ENTRY;
        success := False;
    end dequeueRingServiceRequest;

    procedure receiveInternal
        (hasDeadline : in  Boolean;
         deadlineMs  : in  Unsigned_64;
         from        : out ProcessID;
         msg         : out Message;
         received    : out Boolean)

    is
        mypid    : constant ProcessID := PerCPUData.getCurrentPID;
        receiver : constant ProcessID := getReceiver (mypid);
        sender   : ProcessID;
        ignore   : ProcessID;
        re       : RingEntry;
        ok       : Boolean;
    begin
        -- Validate our own state
        if mypid = NO_PROCESS then
            from := NO_PROCESS;
            msg  := NULL_MESSAGE;
            received := False;
            return;
        end if;

        Spinlocks.enterCriticalSection (mailtab(receiver).lock);

        -- Check sendQueue FIRST (synchronous call/send senders).
        if not Queues.isEmpty (mailtab(receiver).sendQueue) then
            Queues.dequeue (mailtab(receiver).sendQueue, sender);

            msg  := proctab(sender).sendMsg;
            from := sender;
            re   := (msg       => msg,
                     sender    => sender,
                     kind      => RING_SYNC,
                     requestId => NO_REQUEST_ID);

            proctab(sender).state := WAITINGFORREPLY;

            -- Mint one-use reply cap for this sender
            proctab(mypid).caps(Capabilities.REPLY_CAP_SLOT) :=
                (capType  => Capabilities.CAP_REPLY,
                 rights   => Capabilities.ALL_RIGHTS,
                 capBadge => Capabilities.NO_BADGE,
                 object   => (ref   => Unsigned_64(from),
                              param => NO_REQUEST_ID),
                 gen      => proctab(from).capGeneration);

            Spinlocks.exitCriticalSection (mailtab(receiver).lock);
            received := True;
            return;
        end if;

        -- Device IRQs are persistent doorbells outside the lossy message
        -- ring. Prefer one bounded device drain before ordinary queued work.
        takeIRQDoorbell (receiver, re, ok);
        if not ok then
            -- Check unified ring (submit messages, events, send Path 1).
            dequeueRing (receiver, re, ok);
        end if;
        if ok then
            from := re.sender;
            msg  := re.msg;

            -- Mint reply cap only for messages that expect replies.
            if from /= NO_PROCESS and then
               (re.kind = RING_SYNC or else re.kind = RING_ASYNC_REQUEST)
            then
                proctab(mypid).caps(Capabilities.REPLY_CAP_SLOT) :=
                    (capType  => Capabilities.CAP_REPLY,
                     rights   => Capabilities.ALL_RIGHTS,
                     capBadge => Capabilities.NO_BADGE,
                     object   => (ref   => Unsigned_64(from),
                                  param => re.requestId),
                     gen      => proctab(from).capGeneration);
            else
                proctab(mypid).caps(Capabilities.REPLY_CAP_SLOT) :=
                    Capabilities.NULL_CAPABILITY;
            end if;

            Spinlocks.exitCriticalSection (mailtab(receiver).lock);
            received := True;
            return;
        end if;

        if hasDeadline and then Time.msTicks >= deadlineMs then
            from := NO_PROCESS;
            msg := NULL_MESSAGE;
            received := False;
            Spinlocks.exitCriticalSection (mailtab(receiver).lock);
            return;
        end if;

        -- No message and no sender waiting. Block as a receiver.
        proctab(mypid).queueKey := receiver;
        proctab(mypid).receiveDeadlineMs := deadlineMs;
        proctab(mypid).receiveDeadlineReceiver := receiver;
        proctab(mypid).receiveDeadlineActive := hasDeadline;
        Queues.enqueue (mailtab(receiver).recvQueue, mypid, ignore);
        proctab(mypid).state := RECEIVING;

        Spinlocks.exitCriticalSection (mailtab(receiver).lock);

        yield;

        -- Woken by send/submit/sendEvent. Check sendQueue first.
        Spinlocks.enterCriticalSection (mailtab(receiver).lock);
        proctab(mypid).receiveDeadlineActive := False;
        proctab(mypid).receiveDeadlineMs := 0;
        proctab(mypid).receiveDeadlineReceiver := NO_PROCESS;

        if not Queues.isEmpty (mailtab(receiver).sendQueue) then
            Queues.dequeue (mailtab(receiver).sendQueue, sender);

            msg  := proctab(sender).sendMsg;
            from := sender;
            re   := (msg       => msg,
                     sender    => sender,
                     kind      => RING_SYNC,
                     requestId => NO_REQUEST_ID);

            proctab(sender).state := WAITINGFORREPLY;
        else
            -- Woken by an IRQ doorbell, submit, sendEvent, or send Path 1.
            takeIRQDoorbell (receiver, re, ok);
            if not ok then
                dequeueRing (receiver, re, ok);
            end if;
            if ok then
                from := re.sender;
                msg  := re.msg;
            else
                from := NO_PROCESS;
                msg  := NULL_MESSAGE;
            end if;
        end if;

        -- Mint reply cap if real sender
        if from /= NO_PROCESS and then
           (re.kind = RING_SYNC or else re.kind = RING_ASYNC_REQUEST)
        then
            proctab(mypid).caps(Capabilities.REPLY_CAP_SLOT) :=
                (capType  => Capabilities.CAP_REPLY,
                 rights   => Capabilities.ALL_RIGHTS,
                 capBadge => Capabilities.NO_BADGE,
                 object   => (ref   => Unsigned_64(from),
                              param => re.requestId),
                 gen      => proctab(from).capGeneration);
        else
            proctab(mypid).caps(Capabilities.REPLY_CAP_SLOT) :=
                Capabilities.NULL_CAPABILITY;
        end if;

        received := from /= NO_PROCESS or else ok;
        Spinlocks.exitCriticalSection (mailtab(receiver).lock);
    end receiveInternal;

    ---------------------------------------------------------------------------
    -- receive
    --
    -- Check if a sender is already waiting in our sendQueue. If so, accept
    -- the message immediately and move the sender to WAITINGFORREPLY.
    -- Otherwise, enqueue ourselves as a receiver and block.
    ---------------------------------------------------------------------------
    procedure receive (from : out ProcessID; msg : out Message)
    is
        received : Boolean;
    begin
        receiveInternal (False, 0, from, msg, received);
    end receive;

    ---------------------------------------------------------------------------
    -- receiveUntil
    ---------------------------------------------------------------------------
    procedure receiveUntil
        (deadlineMs : in  Unsigned_64;
         from       : out ProcessID;
         msg        : out Message;
         received   : out Boolean)

    is
    begin
        receiveInternal (True, deadlineMs, from, msg, received);
    end receiveUntil;

    ---------------------------------------------------------------------------
    -- expireReceiveDeadlines
    --
    -- Timed receivers stay only on their mailbox queue. The deadline fields
    -- use an atomic active flag as a lock-free hint for this bounded scan; all
    -- decisions are repeated while holding the mailbox and process locks that
    -- serialize receive, publication, and teardown.
    ---------------------------------------------------------------------------
    procedure expireReceiveDeadlines (nowMs : Unsigned_64)
    is
        receiver : ProcessID;
        removed  : ProcessID;
    begin
        for pid in proctab'Range loop
            if proctab(pid).receiveDeadlineActive and then
               proctab(pid).receiveDeadlineMs <= nowMs
            then
                receiver := proctab(pid).receiveDeadlineReceiver;
                if receiver /= NO_PROCESS then
                    Spinlocks.enterCriticalSection (mailtab(receiver).lock);
                    Spinlocks.enterCriticalSection (lock);
                    if proctab(pid).state = RECEIVING and then
                       proctab(pid).receiveDeadlineActive and then
                       proctab(pid).receiveDeadlineReceiver = receiver and then
                       proctab(pid).receiveDeadlineMs <= nowMs
                    then
                        Queues.popItem
                          (mailtab(receiver).recvQueue, pid, removed);
                        if removed = pid then
                            proctab(pid).receiveDeadlineActive := False;
                            ready (pid);
                        end if;
                    end if;
                    Spinlocks.exitCriticalSection (lock);
                    Spinlocks.exitCriticalSection (mailtab(receiver).lock);
                end if;
            end if;
        end loop;
    end expireReceiveDeadlines;

    ---------------------------------------------------------------------------
    -- receiveEvent
    -- Blocking receive from the unified ring buffer.
    ---------------------------------------------------------------------------
    function receiveEvent return Message  is
        mypid    : constant ProcessID := PerCPUData.getCurrentPID;
        receiver : constant ProcessID := getReceiver (mypid);
        re       : RingEntry;
        ok       : Boolean;
    begin
        loop
            Spinlocks.enterCriticalSection (mailtab(receiver).lock);

            takeIRQDoorbell (receiver, re, ok);
            if not ok then
                dequeueRingKind (receiver, RING_EVENT, re, ok);
            end if;

            if ok then
                Spinlocks.exitCriticalSection (mailtab(receiver).lock);
                return re.msg;
            end if;

            -- No entry available, block
            proctab(mypid).state := WAITINGFOREVENT;
            Spinlocks.exitCriticalSection (mailtab(receiver).lock);

            yield;
        end loop;
    end receiveEvent;

    ---------------------------------------------------------------------------
    -- receiveEventNB
    -- Non-blocking receive from the unified ring buffer.
    ---------------------------------------------------------------------------
    procedure receiveEventNB (msg : out Message; found : out Boolean)
    is
        mypid    : constant ProcessID := PerCPUData.getCurrentPID;
        receiver : constant ProcessID := getReceiver (mypid);
        re       : RingEntry;
    begin
        msg   := NULL_MESSAGE;
        found := False;

        if mypid = NO_PROCESS then
            return;
        end if;

        Spinlocks.enterCriticalSection (mailtab(receiver).lock);

        takeIRQDoorbell (receiver, re, found);
        if not found then
            dequeueRingKind (receiver, RING_EVENT, re, found);
        end if;
        if found then
            msg := re.msg;
        end if;

        Spinlocks.exitCriticalSection (mailtab(receiver).lock);
    end receiveEventNB;

    ---------------------------------------------------------------------------
    -- replyWait
    -- Fused reply+receive: reply to previous sender, then check for next
    -- message immediately without yielding. In the common server pattern
    -- (next client already waiting in sendQueue), this handles the full
    -- round-trip with zero context switches.
    ---------------------------------------------------------------------------
    procedure replyWait (replyTo : in ProcessID; replyMsg : in Message;
                         from : out ProcessID; msg : out Message) is
        ignored : Unsigned_64;
    begin
        -- Use the same generation-checked, mailbox-locked reply handoff as
        -- standalone replies. Do not retain an unlocked reply target across
        -- the subsequent receive operation.
        if replyTo /= NO_PROCESS then
            ignored := reply (replyTo, replyMsg);
        end if;
        receive (from, msg);
    end replyWait;

    procedure receiveServiceRequestNB (from  : out ProcessID;
                                       msg   : out Message;
                                       found : out Boolean)
    is
        mypid    : constant ProcessID := PerCPUData.getCurrentPID;
        receiver : constant ProcessID := getReceiver (mypid);
        sender   : ProcessID;
        re       : RingEntry;
    begin
        Spinlocks.enterCriticalSection (mailtab(receiver).lock);

        -- Check sendQueue first (synchronous senders).
        if not Queues.isEmpty (mailtab(receiver).sendQueue) then
            Queues.dequeue (mailtab(receiver).sendQueue, sender);

            msg   := proctab(sender).sendMsg;
            from  := sender;
            re    := (msg       => msg,
                      sender    => sender,
                      kind      => RING_SYNC,
                      requestId => NO_REQUEST_ID);
            found := True;

            proctab(sender).state := WAITINGFORREPLY;

            proctab(mypid).caps(Capabilities.REPLY_CAP_SLOT) :=
                (capType  => Capabilities.CAP_REPLY,
                 rights   => Capabilities.ALL_RIGHTS,
                 capBadge => Capabilities.NO_BADGE,
                 object   => (ref   => Unsigned_64(from),
                              param => NO_REQUEST_ID),
                 gen      => proctab(from).capGeneration);
        else
            -- Service-request polling is intentionally typed. The mailbox ring
            -- may contain events, but this receive path must leave them queued
            -- for receiveEventNB.
            dequeueRingServiceRequest (receiver, re, found);
            if found then
                from := re.sender;
                msg  := re.msg;

                if from /= NO_PROCESS and then
                   (re.kind = RING_SYNC or else re.kind = RING_ASYNC_REQUEST)
                then
                    proctab(mypid).caps(Capabilities.REPLY_CAP_SLOT) :=
                        (capType  => Capabilities.CAP_REPLY,
                         rights   => Capabilities.ALL_RIGHTS,
                         capBadge => Capabilities.NO_BADGE,
                         object   => (ref   => Unsigned_64(from),
                                      param => re.requestId),
                         gen      => proctab(from).capGeneration);
                else
                    proctab(mypid).caps(Capabilities.REPLY_CAP_SLOT) :=
                        Capabilities.NULL_CAPABILITY;
                end if;
            else
                from  := NO_PROCESS;
                msg   := NULL_MESSAGE;
                proctab(mypid).caps(Capabilities.REPLY_CAP_SLOT) :=
                    Capabilities.NULL_CAPABILITY;
            end if;
        end if;

        Spinlocks.exitCriticalSection (mailtab(receiver).lock);
    end receiveServiceRequestNB;

    ---------------------------------------------------------------------------
    -- receiveAnyIpcNB
    -- Non-blocking mixed receive across all mailbox traffic classes:
    -- it may consume service requests, one-way messages, and events. Use only
    -- for intentionally mixed dispatch loops.
    ---------------------------------------------------------------------------
    procedure receiveAnyIpcNB (from  : out ProcessID;
                               msg   : out Message;
                               found : out Boolean)
    is
        mypid    : constant ProcessID := PerCPUData.getCurrentPID;
        receiver : constant ProcessID := getReceiver (mypid);
        sender   : ProcessID;
        re       : RingEntry;
    begin
        Spinlocks.enterCriticalSection (mailtab(receiver).lock);

        -- Check sendQueue first (synchronous senders).
        if not Queues.isEmpty (mailtab(receiver).sendQueue) then
            Queues.dequeue (mailtab(receiver).sendQueue, sender);

            msg   := proctab(sender).sendMsg;
            from  := sender;
            re    := (msg       => msg,
                      sender    => sender,
                      kind      => RING_SYNC,
                      requestId => NO_REQUEST_ID);
            found := True;

            proctab(sender).state := WAITINGFORREPLY;

            proctab(mypid).caps(Capabilities.REPLY_CAP_SLOT) :=
                (capType  => Capabilities.CAP_REPLY,
                 rights   => Capabilities.ALL_RIGHTS,
                 capBadge => Capabilities.NO_BADGE,
                 object   => (ref   => Unsigned_64(from),
                              param => NO_REQUEST_ID),
                 gen      => proctab(from).capGeneration);
        else
            -- Explicitly broad: this removes the next ring entry regardless of
            -- whether it is a request or event. Persistent device work is
            -- checked first so it cannot sit behind a full ordinary ring.
            takeIRQDoorbell (receiver, re, found);
            if not found then
                dequeueRing (receiver, re, found);
            end if;
            if found then
                from := re.sender;
                msg  := re.msg;

                if from /= NO_PROCESS and then
                   (re.kind = RING_SYNC or else re.kind = RING_ASYNC_REQUEST)
                then
                    proctab(mypid).caps(Capabilities.REPLY_CAP_SLOT) :=
                        (capType  => Capabilities.CAP_REPLY,
                         rights   => Capabilities.ALL_RIGHTS,
                         capBadge => Capabilities.NO_BADGE,
                         object   => (ref   => Unsigned_64(from),
                                      param => re.requestId),
                         gen      => proctab(from).capGeneration);
                else
                    proctab(mypid).caps(Capabilities.REPLY_CAP_SLOT) :=
                        Capabilities.NULL_CAPABILITY;
                end if;
            else
                from  := NO_PROCESS;
                msg   := NULL_MESSAGE;
            end if;
        end if;

        Spinlocks.exitCriticalSection (mailtab(receiver).lock);
    end receiveAnyIpcNB;

    ---------------------------------------------------------------------------
    -- send
    --
    -- Send-first IPC with two paths, each yielding exactly once:
    --
    -- Path 1 (receiver already waiting in recvQueue):
    --   Deliver message directly, wake receiver, set WAITINGFORREPLY,
    --   yield once. Woken when receiver calls reply().
    --
    -- Path 2 (no receiver waiting):
    --   Deposit message, enqueue in sendQueue, set SENDING, yield once.
    --   The receiver's receive()/receiveAnyIpcNB() will dequeue us and set
    --   our state to WAITINGFORREPLY. Then reply() calls notify() which adds
    --   us to the ready list. We resume with reply already delivered.
    ---------------------------------------------------------------------------
    function send (dest : ProcessID; msg : Message;
                   expectedGeneration : Capabilities.Generation := 0) return MessageTag

    is
        pid      : constant ProcessID := PerCPUData.getCurrentPID;
        receiver : ProcessID;
        replyTag : MessageTag;
        ignore   : ProcessID;
    begin
        -- Validate destination
        if dest = NO_PROCESS then
            return NULL_TAG;
        end if;

        if proctab(dest).state = INVALID then
            return NULL_TAG;
        end if;

        Spinlocks.enterCriticalSection (mailtab(dest).lock);
        if mailtab(dest).closed or else
           (expectedGeneration /= 0 and then
            expectedGeneration /= proctab(dest).capGeneration)
        then
            Spinlocks.exitCriticalSection (mailtab(dest).lock);
            return NULL_TAG;
        end if;

        -- Capability enforcement for legacy PID-based send.
        -- Kernel threads are exempt (they have no cap table).
        if Config.ENFORCE_IPC_CAPS
           and then proctab(pid).mode = USER
        then
            enforceCheck : declare
                found : Boolean := False;
            begin
                for i in Capabilities.CapabilitySlot loop
                    if proctab(pid).caps(i).capType = Capabilities.CAP_ENDPOINT
                       and then proctab(pid).caps(i).object.ref = Unsigned_64(dest)
                       and then proctab(pid).caps(i).rights(Capabilities.RIGHT_WRITE)
                       and then proctab(pid).caps(i).gen =
                                proctab(dest).capGeneration
                    then
                        found := True;
                        exit;
                    end if;
                end loop;

                if not found then
                    Spinlocks.exitCriticalSection (mailtab(dest).lock);
                    return NULL_TAG;
                end if;
            end enforceCheck;
        end if;

        -- Store our message in per-sender storage so it cannot be
        -- overwritten by another sender racing to the same destination.
        proctab(pid).sendMsg := msg;

        if not Queues.isEmpty (mailtab(dest).recvQueue) then
            -- Path 1: receiver already waiting. Enqueue message in
            -- unified ring so receiver can dequeue it after waking.
            enqueueP1 : declare
                ok : Boolean;
            begin
                enqueueRing (dest,
                             (msg       => msg,
                              sender    => pid,
                              kind      => RING_SYNC,
                              requestId => NO_REQUEST_ID),
                             ok);
                if not ok then
                    Spinlocks.exitCriticalSection (mailtab(dest).lock);
                    return NULL_TAG;
                end if;
            end enqueueP1;

            Queues.dequeue (mailtab(dest).recvQueue, receiver);

            -- Sender goes to WAITINGFORREPLY
            proctab(pid).state := WAITINGFORREPLY;

            -- Acquire Process.lock BEFORE releasing mailtab.lock to
            -- close the window where receiver could be killed/migrated.
            -- Lock ordering: mailtab < Process.lock (documented).
            if proctab(receiver).cpu = PerCPUData.getCPUNumber then
                Spinlocks.enterCriticalSection (lock);
                Spinlocks.exitCriticalSection (mailtab(dest).lock);
                directSwitch (pid, receiver);
                Spinlocks.exitCriticalSection (lock);

                -- Resumed: reply delivered via directSwitch from reply()
                replyTag := proctab(pid).replyMsg.tag;
                return replyTag;
            else
                -- Cross CPU: acquire Process.lock, release mailtab,
                -- enqueue receiver, release Process.lock.
                Spinlocks.enterCriticalSection (lock);
                Spinlocks.exitCriticalSection (mailtab(dest).lock);
                ready (receiver);
                Spinlocks.exitCriticalSection (lock);
            end if;
        else
            -- Path 2: no receiver yet. Enqueue ourselves as a sender.
            proctab(pid).queueKey := dest;
            Queues.enqueue (mailtab(dest).sendQueue, pid, ignore);
            proctab(pid).state := SENDING;

            Spinlocks.exitCriticalSection (mailtab(dest).lock);
        end if;

        -- Path 2: yield and wait for receiver to dequeue us
        yield;

        -- Reply delivered — replyMsg populated by reply()
        replyTag := proctab(pid).replyMsg.tag;

        return replyTag;
    end send;

    ---------------------------------------------------------------------------
    -- sendEvent
    -- Non-blocking send for interrupt context. Does not block the caller.
    -- Pushes to the event ring buffer; drops if full.
    ---------------------------------------------------------------------------
    procedure trySendEvent (dest     : ProcessID;
                            msg      : Message;
                            accepted : out Boolean;
                            expectedGeneration : Capabilities.Generation := 0)
         is
        removed : ProcessID;
    begin
        accepted := False;

        -- Validate destination
        if dest = NO_PROCESS then
            return;
        end if;

        if proctab(dest).state = INVALID then
            return;
        end if;

        Spinlocks.enterCriticalSection (mailtab(dest).lock);

        if mailtab(dest).closed or else
           (expectedGeneration /= 0 and then
            expectedGeneration /= proctab(dest).capGeneration)
        then
            Spinlocks.exitCriticalSection (mailtab(dest).lock);
            return;
        end if;

        enqueueRing (dest,
                     (msg       => msg,
                      sender    => NO_PROCESS,
                      kind      => RING_EVENT,
                      requestId => NO_REQUEST_ID),
                     accepted);

        if not accepted then
            proctab(dest).eventDrops := proctab(dest).eventDrops + 1;
        end if;

        --  receive() is the intentional mixed-lane wait primitive: it may
        --  consume events as well as requests. Wake both event-specific and
        --  mixed waiters whenever unsolicited work is published.
        if proctab(dest).state = RECEIVING then
            --  receive() placed the waiter in recvQueue. An event is not a
            --  synchronous sender and therefore must explicitly remove that
            --  queue membership before making the process runnable.
            Queues.popItem (mailtab(dest).recvQueue, dest, removed);
            notify (dest);
        elsif proctab(dest).state = WAITINGFOREVENT then
            notify (dest);
        end if;

        Spinlocks.exitCriticalSection (mailtab(dest).lock);
    end trySendEvent;

    procedure sendEvent (dest : ProcessID; msg : Message)

    is
        accepted : Boolean;
    begin
        trySendEvent (dest, msg, accepted);
    end sendEvent;

    ---------------------------------------------------------------------------
    -- notifyIRQ
    -- Publish a persistent, coalescing device-work doorbell. Drivers drain
    -- their authoritative controller/ring state after observing it, so one
    -- bit is sufficient regardless of how many interrupts arrived meanwhile.
    ---------------------------------------------------------------------------
    procedure notifyIRQ (dest : ProcessID)

    is
        removed : ProcessID;
    begin
        if dest = NO_PROCESS or else proctab(dest).state = INVALID then
            return;
        end if;

        Spinlocks.enterCriticalSection (mailtab(dest).lock);
        if mailtab(dest).closed then
            Spinlocks.exitCriticalSection (mailtab(dest).lock);
            return;
        end if;
        proctab(dest).irqNotificationPending := True;

        if proctab(dest).state = RECEIVING then
            Queues.popItem (mailtab(dest).recvQueue, dest, removed);
            notify (dest);
        elsif proctab(dest).state = WAITINGFOREVENT then
            notify (dest);
        end if;

        Spinlocks.exitCriticalSection (mailtab(dest).lock);
    end notifyIRQ;

    ---------------------------------------------------------------------------
    -- notifySupervisor
    -- Send a non-blocking fault event to the supervisor of the given process.
    ---------------------------------------------------------------------------
    procedure notifySupervisor (pid        : ProcessID;
                                faultLabel : Unsigned_32;
                                detail0    : Unsigned_64;
                                detail1    : Unsigned_64;
                                detail2    : Unsigned_64)

    is
        svpid : constant ProcessID := proctab(pid).svpid;
        faultMsg : Message := NULL_MESSAGE;
    begin
        if svpid = NO_PROCESS then
            return;
        end if;

        faultMsg.tag := (label  => faultLabel,
                         length => 4,
                         flags  => 0,
                         badge  => 0);
        faultMsg.words (0) := Unsigned_64 (pid);
        faultMsg.words (1) := detail0;
        faultMsg.words (2) := detail1;
        faultMsg.words (3) := detail2;

        sendEvent (svpid, faultMsg);
    end notifySupervisor;

    ---------------------------------------------------------------------------
    -- reply
    -- Dual-path reply:
    -- SYNC PATH: sender is in WAITINGFORREPLY (used send()), store reply
    --   in proctab and wake them directly.
    -- ASYNC PATH: sender used submit(), has a pending request. Look up
    --   the token, enqueue a CompletionEntry, wake sender if blocked in
    --   WAITINGFORCOMPLETION.
    ---------------------------------------------------------------------------
    -- Target mailbox remains locked from authority validation through result
    -- publication. Synchronous handoff transfers only Process.lock.
    function completeReplyLocked
      (replyTo : ProcessID; requestId : Unsigned_64; msg : Message)
      return Unsigned_64
    is
        mypid : constant ProcessID := PerCPUData.getCurrentPID;
        token : Unsigned_64;
        ok : Boolean;
    begin
        if requestId = NO_REQUEST_ID and then
           proctab(replyTo).state = WAITINGFORREPLY
        then
            Spinlocks.enterCriticalSection (lock);
            proctab(replyTo).replyMsg := msg;
            if proctab(replyTo).cpu = PerCPUData.getCPUNumber then
                ready (mypid);
                Spinlocks.exitCriticalSection (mailtab(replyTo).lock);
                directSwitch (mypid, replyTo);
                Spinlocks.exitCriticalSection (lock);
            else
                ready (replyTo);
                Spinlocks.exitCriticalSection (lock);
                Spinlocks.exitCriticalSection (mailtab(replyTo).lock);
            end if;
        else
            findAndRemovePending (replyTo, mypid, requestId, token, ok);
            if not ok then
                Spinlocks.exitCriticalSection (mailtab(replyTo).lock);
                return 0;
            end if;
            enqueueCompletion
              (owner => replyTo,
               item => (requestId => requestId, token => token, msg => msg,
                        from => mypid, status => COMPLETION_OK, valid => True),
               success => ok);
            if not ok then
                raise ProcessException with "Missing reserved completion slot";
            end if;
            if proctab(replyTo).state = WAITINGFORCOMPLETION then
                notify (replyTo);
            end if;
            Spinlocks.exitCriticalSection (mailtab(replyTo).lock);
        end if;
        return 1;
    end completeReplyLocked;

    function reply (replyTo : ProcessID; msg : Message) return Unsigned_64 is
        mypid : constant ProcessID := PerCPUData.getCurrentPID;
        requestId : Unsigned_64;
        ok : Boolean;
    begin
        if replyTo = NO_PROCESS then return 0; end if;
        Spinlocks.enterCriticalSection (mailtab(replyTo).lock);
        if mailtab(replyTo).closed then
            Spinlocks.exitCriticalSection (mailtab(replyTo).lock);
            return 0;
        end if;
        consumeReplyAuthority (mypid, replyTo, requestId, ok);
        if not ok then
            Spinlocks.exitCriticalSection (mailtab(replyTo).lock);
            return 0;
        end if;
        return completeReplyLocked (replyTo, requestId, msg);
    end reply;

    function replyCap
      (capSlot : Capabilities.CapabilitySlot; msg : Message)
      return Unsigned_64
    is
        mypid : constant ProcessID := PerCPUData.getCurrentPID;
        cap : Capabilities.Capability;
        replyTo : ProcessID;
        ok : Boolean;
    begin
        if mypid = NO_PROCESS then return 0; end if;
        cap := proctab(mypid).caps(capSlot);
        if cap.capType /= Capabilities.CAP_REPLY or else
           cap.object.ref = 0 or else cap.object.ref > Unsigned_64(ProcessID'Last)
        then return 0; end if;
        replyTo := ProcessID(cap.object.ref);
        Spinlocks.enterCriticalSection (mailtab(replyTo).lock);
        if mailtab(replyTo).closed or else
           cap.gen /= proctab(replyTo).capGeneration
        then
            Spinlocks.exitCriticalSection (mailtab(replyTo).lock);
            return 0;
        end if;
        Capabilities.Operations.takeReplyCap
          (proctab(mypid).caps, capSlot, cap, ok);
        if not ok then
            Spinlocks.exitCriticalSection (mailtab(replyTo).lock);
            return 0;
        end if;
        proctab(mypid).deferredReplyCaps :=
            proctab(mypid).deferredReplyCaps and
            not Shift_Left (Unsigned_64'(1), capSlot);
        return completeReplyLocked (replyTo, cap.object.param, msg);
    end replyCap;

    ---------------------------------------------------------------------------
    -- submit
    -- Non-blocking async send. Delivers message to dest's mailbox and
    -- returns immediately.  When token /= NO_COMPLETION_TOKEN, allocates a
    -- kernel request ID and records (dest, requestId, token) in caller's
    -- pending array so that a later reply() can enqueue a CompletionEntry.
    -- Fire-and-forget senders pass
    -- NO_COMPLETION_TOKEN to avoid leaking pending slots.
    ---------------------------------------------------------------------------
    -- Async publication updates two owners. Always acquire distinct mailboxes
    -- in ascending PID order, then Process.lock if a wakeup is needed.
    procedure lockMailboxes (A, B : ProcessID) is
    begin
        Spinlocks.enterCriticalSection (mailtab(ProcessID'Min (A, B)).lock);
        if A /= B then
            Spinlocks.enterCriticalSection (mailtab(ProcessID'Max (A, B)).lock);
        end if;
    end lockMailboxes;

    procedure unlockMailboxes (A, B : ProcessID) is
    begin
        if A /= B then
            Spinlocks.exitCriticalSection (mailtab(ProcessID'Max (A, B)).lock);
        end if;
        Spinlocks.exitCriticalSection (mailtab(ProcessID'Min (A, B)).lock);
    end unlockMailboxes;

    function submit (dest  : ProcessID;
                     msg   : Message;
                     token : Unsigned_64;
                     expectedGeneration : Capabilities.Generation := 0) return Boolean

    is
        pid      : constant ProcessID := PerCPUData.getCurrentPID;
        receiver : ProcessID;
        ok       : Boolean;
        wantCompletion : constant Boolean :=
            (token /= NO_COMPLETION_TOKEN);
        entryKind : constant RingEntryKind :=
            (if token = NO_COMPLETION_TOKEN
             then RING_ONEWAY
             else RING_ASYNC_REQUEST);
        requestId : Unsigned_64 := NO_REQUEST_ID;
    begin
        -- Validate destination
        if dest = NO_PROCESS then
            return False;
        end if;

        if proctab(dest).state = INVALID then
            return False;
        end if;

        lockMailboxes (pid, dest);
        if mailtab(pid).closed or else mailtab(dest).closed or else
           (expectedGeneration /= 0 and then
            expectedGeneration /= proctab(dest).capGeneration)
        then
            unlockMailboxes (pid, dest);
            return False;
        end if;

        -- Reserve a completion slot together with every pending request.
        -- This guarantees replies and target-death completions cannot overflow.
        -- Check we have room for another pending request
        if wantCompletion and then
           (proctab(pid).numPending >= MAX_PENDING_ASYNC or else
            proctab(pid).numPending + completionTab(pid).count >=
              COMPLETION_QUEUE_SIZE)
        then
            unlockMailboxes (pid, dest);
            return False;
        end if;

        if wantCompletion then
            requestId := proctab(pid).nextRequestId;
            if requestId = NO_REQUEST_ID then
                requestId := 1;
            end if;

            if proctab(pid).nextRequestId = Unsigned_64'Last then
                proctab(pid).nextRequestId := 1;
            else
                proctab(pid).nextRequestId :=
                    proctab(pid).nextRequestId + 1;
            end if;
        end if;

        -- Enqueue in unified ring
        enqueueRing (dest,
                     (msg       => msg,
                      sender    => pid,
                      kind      => entryKind,
                      requestId => requestId),
                     ok);

        if not ok then
            unlockMailboxes (pid, dest);
            return False;
        end if;

        -- Record pending request only when a completion is expected
        if wantCompletion then
            proctab(pid).pendingRequests(proctab(pid).numPending) :=
                (dest      => dest,
                 requestId => requestId,
                 token     => token);
            proctab(pid).numPending := proctab(pid).numPending + 1;
        end if;

        -- Wake receiver if one is waiting
        if not Queues.isEmpty (mailtab(dest).recvQueue) then
            Spinlocks.enterCriticalSection (lock);
            Queues.dequeue (mailtab(dest).recvQueue, receiver);
            ready (receiver);
            Spinlocks.exitCriticalSection (lock);
        elsif proctab(dest).state = SLEEPING then
            declare
                woken : Boolean;
            begin
                Queues.wakeFromSleep (dest, woken);
            end;
        end if;

        unlockMailboxes (pid, dest);

        -- Do NOT block — caller keeps running
        return True;
    end submit;

    ---------------------------------------------------------------------------
    -- waitCompletion
    -- Block until at least minWait completions are available, then drain
    -- up to maxEntries.
    ---------------------------------------------------------------------------
    procedure waitCompletion (entries     : out CompletionRing;
                              maxEntries  : in  Natural;
                              minWait     : in  Natural;
                              numReturned : out Natural)
        -- SPARK_Mode Off: uses x86.stac/clac for SMAP user memory access

    is
        mypid    : constant ProcessID := PerCPUData.getCurrentPID;
        receiver : constant ProcessID := getReceiver (mypid);
        drained  : Natural := 0;
        item     : CompletionEntry;
        ok       : Boolean;
        effectiveMax : Natural;
        effectiveMin : Natural;
    begin
        -- Clamp parameters
        if maxEntries > COMPLETION_QUEUE_SIZE then
            effectiveMax := COMPLETION_QUEUE_SIZE;
        else
            effectiveMax := maxEntries;
        end if;

        if minWait > effectiveMax then
            effectiveMin := effectiveMax;
        else
            effectiveMin := minWait;
        end if;

        -- Initialize only the caller-requested extent. Assigning the complete
        -- imported ring here used to overwrite 64 entries even when the
        -- caller supplied maxEntries = 1.
        x86.stac;
        for i in CompletionIndex loop
            exit when i >= effectiveMax;
            entries (i) := NULL_COMPLETION;
        end loop;
        x86.clac;
        numReturned := 0;

        if mypid = NO_PROCESS then
            return;
        end if;

        loop
            Spinlocks.enterCriticalSection (mailtab(receiver).lock);

            if completionTab(receiver).count >= effectiveMin then
                -- Drain up to effectiveMax entries into user buffer
                x86.stac;
                while drained < effectiveMax loop
                    dequeueCompletion (receiver, item, ok);
                    exit when not ok;
                    entries(drained) := item;
                    drained := drained + 1;
                end loop;
                x86.clac;

                Spinlocks.exitCriticalSection (mailtab(receiver).lock);
                numReturned := drained;
                return;
            end if;

            -- Not enough completions yet — block.
            -- EFLAGS.AC (SMAP) is cleared by context switch; re-set
            -- STAC when we loop back to drain.
            proctab(mypid).state := WAITINGFORCOMPLETION;
            Spinlocks.exitCriticalSection (mailtab(receiver).lock);

            yield;

            -- Woken by reply() enqueuing a completion. Loop back to check.
        end loop;
    end waitCompletion;

    ---------------------------------------------------------------------------
    -- pollCompletion
    -- Non-blocking single completion check.
    ---------------------------------------------------------------------------
    procedure pollCompletion (result : out CompletionEntry;
                              found  : out Boolean)

    is
        mypid    : constant ProcessID := PerCPUData.getCurrentPID;
        receiver : constant ProcessID := getReceiver (mypid);
    begin
        result := NULL_COMPLETION;
        found  := False;

        if mypid = NO_PROCESS then
            return;
        end if;

        Spinlocks.enterCriticalSection (mailtab(receiver).lock);

        dequeueCompletion (receiver, result, found);

        Spinlocks.exitCriticalSection (mailtab(receiver).lock);
    end pollCompletion;

    ---------------------------------------------------------------------------
    -- Shared Memory Grant Operations
    ---------------------------------------------------------------------------

    procedure invalidateGrant (value : in out Grant)

    is
        nextGeneration : Memory_Grants.Live_Grant_Generation :=
          value.generation;
        mayReuse : Boolean;
    begin
        Memory_Grants.Advance_Generation (nextGeneration, mayReuse);
        value :=
          (lifecycle   => Memory_Grants.Inactive_Lifecycle,
           reusable    => mayReuse,
           generation  => nextGeneration,
           granterPID  => NO_PROCESS,
           granteePID  => NO_PROCESS,
           granterAddr => System.Null_Address,
           granteeAddr => System.Null_Address,
           numPages    => 0,
           permission  => GRANT_READ);
    end invalidateGrant;

    function overlapsGrantRegion (localAddr : System.Address;
                                  numPages  : Natural) return Boolean

    is
    begin
        if numPages = 0 or else numPages > MAX_GRANT_PAGES then
            return False;
        end if;

        return Memory_Grants.Overlaps_Received_Region
          (Unsigned_64 (To_Integer (localAddr)),
           Memory_Grants.Page_Count (numPages));
    end overlapsGrantRegion;

    ---------------------------------------------------------------------------
    -- createGrant
    -- Map pages from caller's address space into grantee's address space.
    ---------------------------------------------------------------------------
    -- Serialized by grantLock. Keep the retirement snapshot off the 4 KiB
    -- kernel stack; a grant may contain 4096 pages. These addresses survive
    -- removal of the mappings and are used only after shootdown completion.
    Retiring_Frames : array (Memory_Grants.Page_Offset) of Virtmem.PhysAddress;

    procedure unmapGrantPages (g : Grant);

    procedure createGrant (grantee   : in ProcessID;
                           localAddr : in System.Address;
                           numPages  : in Natural;
                           perm      : in GrantPermission;
                           id        : out Natural;
                           success   : out Boolean;
                           expectedGeneration : Capabilities.Generation := 0)
    is
        pid : constant ProcessID := PerCPUData.getCurrentPID;
        owner : constant ProcessID :=
          (if proctab(pid).isThread then proctab(pid).ppid else pid);
        receiver : ProcessID;
        physical : Virtmem.PhysAddress;
        flags : Unsigned_64;
        ok : Boolean;
        found : Boolean := False;
        slot : GrantID := 0;
        globalId : Natural;
        staging : Grant;
        procedure mapPageInst is new Virtmem.mapPage (BuddyAllocator.allocFrame);
    begin
        id := 0;
        success := False;
        if grantee = NO_PROCESS or else numPages = 0 or else
           numPages > MAX_GRANT_PAGES or else
           overlapsGrantRegion (localAddr, numPages) or else
           (To_Integer (localAddr) and 16#FFF#) /= 0
        then
            return;
        end if;

        Spinlocks.enterCriticalSection (grantLock);
        receiver := (if proctab(grantee).isThread then proctab(grantee).ppid
                     else grantee);
        if not proctab(owner).admitted or else
           not proctab(grantee).admitted or else
           Process_Lifetime.Closing (proctab(owner).lifetime) or else
           Process_Lifetime.Closing (proctab(grantee).lifetime) or else
           (expectedGeneration /= 0 and then
            expectedGeneration /= proctab(grantee).capGeneration)
        then
            Spinlocks.exitCriticalSection (grantLock);
            return;
        end if;

        for candidate in GrantID loop
            if not Memory_Grants.Is_Active
              (proctab(owner).grants(candidate).lifecycle) and then
               proctab(owner).grants(candidate).reusable
            then
                slot := candidate;
                found := True;
                exit;
            end if;
        end loop;
        if not found then
            Spinlocks.exitCriticalSection (grantLock);
            return;
        end if;
        globalId := Natural (owner) * MAX_GRANTS_PER_PROCESS + slot;
        flags := (if perm = GRANT_READWRITE then Virtmem.PG_USERDATA
                  else Virtmem.PG_USERDATARO);
        staging := (granterPID => owner, granteePID => receiver,
                    granterAddr => localAddr,
                    granteeAddr => To_Address (GRANT_REGION_BASE +
                        Integer_Address (globalId) * GRANT_SLOT_SIZE),
                    permission => perm, others => <>);

        for page in 0 .. numPages - 1 loop
            physical := Virtmem.tableWalk
              (To_Integer (localAddr) +
                 Integer_Address (page) * Virtmem.PAGE_SIZE,
               addrtab(proctab(owner).pgTable));
            ok := False;
            if physical /= 0 then
                BuddyAllocator.pinOwnedFrame (physical, Unsigned_8 (owner), ok);
            end if;
            if not ok then
                unmapGrantPages (staging);
                Spinlocks.exitCriticalSection (grantLock);
                return;
            end if;

            -- The mapping itself owns this pin, even without an acquisition.
            mapPageInst
              (physical,
               To_Integer (staging.granteeAddr) +
                 Integer_Address (page) * Virtmem.PAGE_SIZE,
               flags, addrtab(proctab(receiver).pgTable), ok);
            if not ok then
                -- This page was never published. Prior pages require a real
                -- unmap/shootdown before dropping their mapping-owned pins.
                BuddyAllocator.unpinFrame (physical, ok);
                if not ok then
                    raise ProcessException with "Unpublished grant pin lost";
                end if;
                unmapGrantPages (staging);
                Spinlocks.exitCriticalSection (grantLock);
                return;
            end if;
            staging.numPages := staging.numPages + 1;
        end loop;

        staging.lifecycle := Memory_Grants.Available_Lifecycle;
        staging.generation := proctab(owner).grants(slot).generation;
        proctab(owner).grants(slot) := staging;
        id := globalId;
        success := True;
        Spinlocks.exitCriticalSection (grantLock);
    end createGrant;

    procedure unmapGrantPages (g : Grant) is
        physical : Virtmem.PhysAddress;
        virtual : Integer_Address;
        ok : Boolean;
    begin
        if g.numPages = 0 then
            return;
        end if;
        -- INVALID does not mean other CPUs have stopped using this address
        -- space. Remove mappings while the page tables still exist, even on
        -- process teardown, and acknowledge every online CPU before release.
        if proctab(g.granteePID).pgTable = NO_PROCESS then
            raise ProcessException with "Grant page tables destroyed before retirement";
        end if;
        for page in 0 .. g.numPages - 1 loop
            virtual := To_Integer (g.granteeAddr) +
                Integer_Address (page) * Virtmem.PAGE_SIZE;
            physical := Virtmem.tableWalk
              (virtual, addrtab(proctab(g.granteePID).pgTable));
            if physical = 0 then
                raise ProcessException with "Grant mapping lost before retirement";
            end if;
            Retiring_Frames (page) := physical;
            Virtmem.unmapPage
              (virtual, addrtab(proctab(g.granteePID).pgTable), ok);
            if not ok then
                raise ProcessException with "Grant mapping could not be removed";
            end if;
        end loop;
        TLB_Shootdown.Invalidate_All;
        -- No pin release (and hence no allocator reuse) before completion.
        for page in 0 .. g.numPages - 1 loop
            BuddyAllocator.unpinFrame (Retiring_Frames (page), ok);
            if not ok then
                raise ProcessException with "Retired grant lost its lifetime pin";
            end if;
        end loop;
    end unmapGrantPages;

    procedure revokeGrantLocked (g : in out Grant)

    is
        result : Memory_Grants.Revocation_Result;
    begin
        if not Memory_Grants.Is_Active (g.lifecycle) then
            return;
        end if;

        Memory_Grants.Request_Revocation (g.lifecycle, result);
        if result = Memory_Grants.Revocation_Pending then
            return;
        end if;

        unmapGrantPages (g);
        invalidateGrant (g);
    end revokeGrantLocked;

    ---------------------------------------------------------------------------
    -- revokeGrant
    ---------------------------------------------------------------------------
    procedure revokeGrant (id : GrantID)

    is
        pid   : constant ProcessID := PerCPUData.getCurrentPID;
        owner : constant ProcessID :=
            (if proctab(pid).isThread then proctab(pid).ppid else pid);
    begin
        Spinlocks.enterCriticalSection (grantLock);
        revokeGrantLocked (proctab(owner).grants(id));
        Spinlocks.exitCriticalSection (grantLock);
    end revokeGrant;

    ---------------------------------------------------------------------------
    -- revokeAllGrants
    -- Revoke all active grants owned by the specified process.
    -- Called during process kill().
    ---------------------------------------------------------------------------
    procedure revokeAllGrants (pid : ProcessID)

    is
    begin
        Spinlocks.enterCriticalSection (grantLock);
        for i in GrantID loop
            revokeGrantLocked (proctab(pid).grants(i));
        end loop;
        Spinlocks.exitCriticalSection (grantLock);
    end revokeAllGrants;

    procedure completeOwnerPIDIfReady (owner : ProcessID);

    ---------------------------------------------------------------------------
    -- revokeAllGrantsTo
    -- Teardown must retire received mappings while the page tables exist.
    -- INVALID is a scheduler state, not proof of remote TLB quiescence.
    -- Force-close stops acquisitions, then the ordinary acknowledged mapping
    -- retirement path drops the lifetime pins before invalidating the record.
    ---------------------------------------------------------------------------
    procedure revokeAllGrantsTo (pid : ProcessID)

    is
    begin
        Spinlocks.enterCriticalSection (grantLock);
        for owner in ProcessID range ProcessID'First + 1 .. ProcessID'Last loop
            for slot in GrantID loop
                if Memory_Grants.Is_Active
                  (proctab(owner).grants(slot).lifecycle) and then
                   proctab(owner).grants(slot).granteePID = pid
                then
                    declare
                        hadAcquisitions : Boolean;
                    begin
                        Memory_Grants.Force_Close
                          (proctab(owner).grants(slot).lifecycle,
                           hadAcquisitions);
                        unmapGrantPages (proctab(owner).grants(slot));
                        invalidateGrant (proctab(owner).grants(slot));
                    end;
                end if;
            end loop;
            completeOwnerPIDIfReady (owner);
        end loop;
        Spinlocks.exitCriticalSection (grantLock);
    end revokeAllGrantsTo;

    procedure getOwnedGrantGeneration
      (slot       : Memory_Grants.Global_Slot;
       generation : out Memory_Grants.Grant_Generation;
       success    : out Boolean)

    is
        pid : constant ProcessID := PerCPUData.getCurrentPID;
        owner : constant ProcessID :=
          (if proctab(pid).isThread then proctab(pid).ppid else pid);
        slotOwner : constant ProcessID := ProcessID
          (Memory_Grants.Owner_Of (slot));
        localSlot : constant GrantID := GrantID
          (Memory_Grants.Local_Slot_Of (slot));
        value : Grant renames proctab(slotOwner).grants(localSlot);
    begin
        generation := 0;
        success := False;

        Spinlocks.enterCriticalSection (grantLock);

        if slotOwner /= owner or else
           not Memory_Grants.Is_Active (value.lifecycle) or else
           value.granterPID /= owner
        then
            Spinlocks.exitCriticalSection (grantLock);
            return;
        end if;

        generation := value.generation;
        success := True;
        Spinlocks.exitCriticalSection (grantLock);
    end getOwnedGrantGeneration;

    procedure acquireGrant
      (reference     : Memory_Grants.Reference;
       expectedOwner : ProcessID;
       byteOffset    : Unsigned_64;
       byteLength    : Unsigned_64;
       requiredWrite : Boolean;
       mappedAddress : out System.Address;
       success       : out Boolean)

    is
        pid : constant ProcessID := PerCPUData.getCurrentPID;
        receiver : constant ProcessID :=
          (if proctab(pid).isThread then proctab(pid).ppid else pid);
        slotOwner : constant ProcessID := ProcessID
          (Memory_Grants.Owner_Of (reference.slot));
        localSlot : constant GrantID := GrantID
          (Memory_Grants.Local_Slot_Of (reference.slot));
        value : Grant renames proctab(slotOwner).grants(localSlot);
        mappedBytes : Unsigned_64;
    begin
        mappedAddress := System.Null_Address;
        success := False;

        Spinlocks.enterCriticalSection (grantLock);

        if expectedOwner = NO_PROCESS or else slotOwner /= expectedOwner or else
           not Memory_Grants.Can_Acquire (value.lifecycle) or else
           not value.reusable or else
           value.granterPID /= expectedOwner or else
           value.granteePID /= receiver or else
           not Memory_Grants.Is_Current (reference, value.generation) or else
           byteLength = 0 or else
           (requiredWrite and then value.permission /= GRANT_READWRITE)
        then
            Spinlocks.exitCriticalSection (grantLock);
            return;
        end if;

        mappedBytes := Unsigned_64 (value.numPages) * Memory_Grants.Page_Size;
        if byteOffset >= mappedBytes or else
           byteLength > mappedBytes - byteOffset
        then
            Spinlocks.exitCriticalSection (grantLock);
            return;
        end if;

        -- Mapping-owned lifetime pins already exist. Acquisitions govern
        -- deferred revocation, not whether a visible mapping owns its pages.
        Memory_Grants.Record_Acquire (value.lifecycle);

        mappedAddress := To_Address
          (To_Integer (value.granteeAddr) + Integer_Address (byteOffset));
        success := True;
        Spinlocks.exitCriticalSection (grantLock);
    end acquireGrant;

    procedure releaseDMAAllocations (pid : ProcessID)

    is
    begin
        for d in DMAAllocArray'Range loop
            if proctab(pid).dmaAllocs(d).active then
                declare
                    allocation : DMAAlloc renames proctab(pid).dmaAllocs(d);
                    pages : constant Natural := 2 ** Natural (allocation.order);
                begin
                    for page in 0 .. pages - 1 loop
                        BuddyAllocator.releaseUserFrame
                          (allocation.physAddr +
                             Virtmem.PhysAddress (page * Virtmem.PAGE_SIZE),
                           Unsigned_8 (pid));
                    end loop;
                    BuddyAllocator.free
                      (allocation.order, Virtmem.P2Va (allocation.physAddr));
                    allocation.active := False;
                end;
            end if;
        end loop;
    end releaseDMAAllocations;

    procedure completeOwnerPIDIfReady (owner : ProcessID)

    is
        activeGrant : Boolean := False;
    begin
        if not proctab(owner).grantTeardownPending or else
           not proctab(owner).grantTeardownReady
        then
            return;
        end if;

        for slot in GrantID loop
            if Memory_Grants.Is_Active
              (proctab(owner).grants(slot).lifecycle)
            then
                activeGrant := True;
                exit;
            end if;
        end loop;

        if not activeGrant then
            -- DMA blocks were deliberately retained while any acquisition
            -- could still pin a page within them.  The final return has now
            -- unpinned every such page, so whole buddy blocks are safe to
            -- release at their original order.
            releaseDMAAllocations (owner);
            declare
                reusable : constant Boolean := proctab(owner).pidReusableAfterGrants;
            begin
                proctab(owner).grantTeardownPending := False;
                proctab(owner).grantTeardownReady := False;
                proctab(owner).pidReusableAfterGrants := False;
                if reusable then
                    PIDTracker.freePID (owner);
                end if;
            end;
        end if;
    end completeOwnerPIDIfReady;

    procedure returnGrant
      (reference : Memory_Grants.Reference;
       success   : out Boolean)

    is
        pid : constant ProcessID := PerCPUData.getCurrentPID;
        receiver : constant ProcessID :=
          (if proctab(pid).isThread then proctab(pid).ppid else pid);
        slotOwner : constant ProcessID := ProcessID
          (Memory_Grants.Owner_Of (reference.slot));
        localSlot : constant GrantID := GrantID
          (Memory_Grants.Local_Slot_Of (reference.slot));
        value : Grant renames proctab(slotOwner).grants(localSlot);
        result : Memory_Grants.Return_Result;
    begin
        success := False;
        Spinlocks.enterCriticalSection (grantLock);

        if not Memory_Grants.Is_Active (value.lifecycle) or else
           value.granteePID /= receiver or else
           not Memory_Grants.Is_Current (reference, value.generation) or else
           Memory_Grants.Acquisition_Total (value.lifecycle) = 0
        then
            Spinlocks.exitCriticalSection (grantLock);
            return;
        end if;

        Memory_Grants.Record_Return (value.lifecycle, result);
        if Memory_Grants.Acquisition_Total (value.lifecycle) = 0 then
            if result = Memory_Grants.Revocation_Completed_On_Return then
                unmapGrantPages (value);
                invalidateGrant (value);
                completeOwnerPIDIfReady (slotOwner);
            end if;
        end if;

        success := True;
        Spinlocks.exitCriticalSection (grantLock);
    end returnGrant;

    procedure revokeGrantReference
      (reference : Memory_Grants.Reference;
       success   : out Boolean)

    is
        pid : constant ProcessID := PerCPUData.getCurrentPID;
        owner : constant ProcessID :=
          (if proctab(pid).isThread then proctab(pid).ppid else pid);
        slotOwner : constant ProcessID := ProcessID
          (Memory_Grants.Owner_Of (reference.slot));
        localSlot : constant GrantID := GrantID
          (Memory_Grants.Local_Slot_Of (reference.slot));
        value : Grant renames proctab(slotOwner).grants(localSlot);
    begin
        success := False;
        Spinlocks.enterCriticalSection (grantLock);
        if slotOwner /= owner or else
           not Memory_Grants.Is_Active (value.lifecycle) or else
           value.granterPID /= owner or else
           not Memory_Grants.Is_Current (reference, value.generation)
        then
            Spinlocks.exitCriticalSection (grantLock);
            return;
        end if;

        revokeGrantLocked (value);
        success := True;
        Spinlocks.exitCriticalSection (grantLock);
    end revokeGrantReference;

    procedure prepareGrantProtectedTeardown
      (pid         : ProcessID;
       pidReusable : Boolean;
       deferred    : out Boolean)

    is
    begin
        deferred := False;
        Spinlocks.enterCriticalSection (grantLock);
        for slot in GrantID loop
            if Memory_Grants.Is_Active
              (proctab(pid).grants(slot).lifecycle)
            then
                deferred := True;
                exit;
            end if;
        end loop;

        if deferred then
            proctab(pid).grantTeardownPending := True;
            proctab(pid).grantTeardownReady := False;
            proctab(pid).pidReusableAfterGrants := pidReusable;
        end if;
        Spinlocks.exitCriticalSection (grantLock);
    end prepareGrantProtectedTeardown;

    procedure finishGrantProtectedTeardown (pid : ProcessID)

    is
    begin
        Spinlocks.enterCriticalSection (grantLock);
        if proctab(pid).grantTeardownPending then
            proctab(pid).grantTeardownReady := True;
            completeOwnerPIDIfReady (pid);
        end if;
        Spinlocks.exitCriticalSection (grantLock);
    end finishGrantProtectedTeardown;

    ---------------------------------------------------------------------------
    -- Capability-Aware IPC
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- capSend
    ---------------------------------------------------------------------------
    function capSend (capSlot : Capabilities.CapabilitySlot;
                      msg     : Message) return MessageTag

    is
        pid          : constant ProcessID := PerCPUData.getCurrentPID;
        destPID      : Unsigned_64;
        candidatePID : ProcessID;
        badge        : Capabilities.Badge;
        status       : Capabilities.Operations.OperationStatus;
        stamped      : Message := msg;
    begin
        -- Validate the generic object reference before narrowing it to an
        -- index into proctab, whose first valid process entry is 1.
        destPID := proctab(pid).caps(capSlot).object.ref;
        if destPID < Unsigned_64(ProctabType'First) or else
           destPID > Unsigned_64(ProctabType'Last)
        then
            return NULL_TAG;
        end if;

        candidatePID := ProcessID(destPID);

        Capabilities.Operations.resolveCurrentEndpoint
          (table             => proctab(pid).caps,
           slot              => capSlot,
           rights            => Capabilities.READ_WRITE,
           currentGeneration => proctab(candidatePID).capGeneration,
           destPID           => destPID,
           capBadge          => badge,
           status            => status);

        if status /= Capabilities.Operations.OP_OK then
            return NULL_TAG;
        end if;

        stamped.capBadge := badge;
        return send (dest => candidatePID, msg => stamped,
                     expectedGeneration => proctab(pid).caps(capSlot).gen);
    end capSend;

    ---------------------------------------------------------------------------
    -- capCall
    ---------------------------------------------------------------------------
    function capCall (capSlot : Capabilities.CapabilitySlot;
                      msg     : Message) return MessageTag

    is
        pid          : constant ProcessID := PerCPUData.getCurrentPID;
        destPID      : Unsigned_64;
        candidatePID : ProcessID;
        badge        : Capabilities.Badge;
        status       : Capabilities.Operations.OperationStatus;
        stamped      : Message := msg;
    begin
        destPID := proctab(pid).caps(capSlot).object.ref;
        if destPID < Unsigned_64(ProctabType'First) or else
           destPID > Unsigned_64(ProctabType'Last)
        then
            return NULL_TAG;
        end if;

        candidatePID := ProcessID(destPID);

        Capabilities.Operations.resolveCurrentEndpoint
          (table             => proctab(pid).caps,
           slot              => capSlot,
           rights            => Capabilities.READ_WRITE,
           currentGeneration => proctab(candidatePID).capGeneration,
           destPID           => destPID,
           capBadge          => badge,
           status            => status);

        if status /= Capabilities.Operations.OP_OK then
            return NULL_TAG;
        end if;

        stamped.capBadge := badge;
        return send (dest => candidatePID, msg => stamped,
                     expectedGeneration => proctab(pid).caps(capSlot).gen);
    end capCall;

    ---------------------------------------------------------------------------
    -- capSubmit
    ---------------------------------------------------------------------------
    function capSubmit (capSlot : Capabilities.CapabilitySlot;
                        msg     : Message;
                        token   : Unsigned_64) return Boolean

    is
        pid          : constant ProcessID := PerCPUData.getCurrentPID;
        destPID      : Unsigned_64;
        candidatePID : ProcessID;
        badge        : Capabilities.Badge;
        status       : Capabilities.Operations.OperationStatus;
        stamped      : Message := msg;
    begin
        destPID := proctab(pid).caps(capSlot).object.ref;
        if destPID < Unsigned_64(ProctabType'First) or else
           destPID > Unsigned_64(ProctabType'Last)
        then
            return False;
        end if;

        candidatePID := ProcessID(destPID);

        Capabilities.Operations.resolveCurrentEndpoint
          (table             => proctab(pid).caps,
           slot              => capSlot,
           rights            => Capabilities.READ_WRITE,
           currentGeneration => proctab(candidatePID).capGeneration,
           destPID           => destPID,
           capBadge          => badge,
           status            => status);

        if status /= Capabilities.Operations.OP_OK then
            return False;
        end if;

        stamped.capBadge := badge;
        return submit (dest  => candidatePID,
                       msg   => stamped,
                       token => token,
                       expectedGeneration => proctab(pid).caps(capSlot).gen);
    end capSubmit;


    procedure retireMailboxes (pid : ProcessID) is
    begin
        for p in ProctabType'Range loop
            Spinlocks.enterCriticalSection (mailtab(p).lock);
            Spinlocks.enterCriticalSection (lock);
            if p = pid or else proctab(p).admitted then
                Queues.detach (mailtab(p).sendQueue, pid);
                Queues.detach (mailtab(p).recvQueue, pid);
                if p /= pid then
                    -- Remove old sender identities before the PID can be
                    -- reused and before a receiver can mint a reply cap.
                    declare
                        item : RingEntry;
                        ok : Boolean;
                        count : constant Natural := mailtab(p).ring.count;
                    begin
                        for n in 1 .. count loop
                            dequeueRing (p, item, ok);
                            if item.sender /= pid then
                                -- Structural compaction, not new admission:
                                -- preserve other senders even if p is closing.
                                mailtab(p).ring.entries(mailtab(p).ring.head) := item;
                                mailtab(p).ring.head := (mailtab(p).ring.head + 1) mod RING_SIZE;
                                mailtab(p).ring.count := mailtab(p).ring.count + 1;
                            end if;
                        end loop;
                    end;
                end if;
            end if;
            if p = pid then
                --  Wake processes blocked on our mailbox (sendQueue / recvQueue)
                drainMailQueues : declare
                    stuckPID : ProcessID;
                begin
                    --  Drain sendQueue: processes waiting to SEND to dying process
                    loop
                        exit when Queues.isEmpty (mailtab(pid).sendQueue);
                        Queues.dequeue (mailtab(pid).sendQueue, stuckPID);
                        proctab(stuckPID).replyMsg := NULL_MESSAGE;
                        ready (stuckPID);
                    end loop;

                    --  Drain recvQueue: processes waiting to RECEIVE from dying process
                    loop
                        exit when Queues.isEmpty (mailtab(pid).recvQueue);
                        Queues.dequeue (mailtab(pid).recvQueue, stuckPID);
                        proctab(stuckPID).replyMsg := NULL_MESSAGE;
                        ready (stuckPID);
                    end loop;

                    --  Wake senders in the ring that are WAITINGFORREPLY
                    --  (from send() Path 1, never dequeued by receive())
                    drainRingSenders : declare
                        r   : MessageRing renames mailtab(pid).ring;
                        idx : RingIndex;
                        s   : ProcessID;
                    begin
                        for i in 0 .. r.count - 1 loop
                            idx := (r.tail + i) mod RING_SIZE;
                            s   := r.entries(idx).sender;
                            if r.entries(idx).kind = RING_SYNC and then s /= NO_PROCESS
                               and then proctab(s).state = WAITINGFORREPLY
                            then
                                proctab(s).replyMsg := NULL_MESSAGE;
                                ready (s);
                            end if;
                        end loop;
                    end drainRingSenders;
                end drainMailQueues;

                --  Wake processes waiting for reply from dying process (CAP_REPLY scan)
                wakeWaiters : declare
                    use type Capabilities.CapabilityType;
                    cap : Capabilities.Capability;
                begin
                    for s in Capabilities.CapabilitySlot loop
                        cap := proctab(pid).caps(s);
                        if cap.capType = Capabilities.CAP_REPLY and then
                           cap.object.param = NO_REQUEST_ID and then
                           cap.object.ref > 0 and then
                           cap.object.ref <= Unsigned_64 (ProcessID'Last)
                        then
                            declare
                                senderPID : constant ProcessID :=
                                    ProcessID (cap.object.ref);
                            begin
                                if proctab(senderPID).state = WAITINGFORREPLY and then
                                   cap.gen = proctab(senderPID).capGeneration
                                then
                                    proctab(senderPID).replyMsg := NULL_MESSAGE;
                                    ready (senderPID);
                                end if;
                            end;
                        end if;
                    end loop;
                end wakeWaiters;

                --  Clear stale mailbox state
                mailtab(pid).ring := (others => <>);

                --  Clear completion queue and pending requests
                completionTab(pid) := (ring => (others => NULL_COMPLETION),
                                       head => 0, tail => 0, count => 0);
                proctab(pid).pendingRequests :=
                    (others => (NO_PROCESS, NO_REQUEST_ID, 0));
                proctab(pid).numPending := 0;
                proctab(pid).nextRequestId := 1;
                proctab(pid).irqNotificationPending := False;
                proctab(pid).receiveDeadlineActive := False;
                proctab(pid).receiveDeadlineMs := 0;
                proctab(pid).receiveDeadlineReceiver := NO_PROCESS;

            else
                if proctab(p).admitted and then proctab(p).state /= INVALID and then p /= pid then
                    declare
                        writeIdx : Natural := 0;
                        cq       : CompletionQueue renames completionTab(p);
                    begin
                        for r in 0 .. proctab(p).numPending - 1 loop
                            if proctab(p).pendingRequests(r).dest /= pid then
                                if writeIdx /= r then
                                    proctab(p).pendingRequests(writeIdx) :=
                                        proctab(p).pendingRequests(r);
                                end if;
                                writeIdx := writeIdx + 1;
                            else
                                if cq.count >= COMPLETION_QUEUE_SIZE then
                                    raise ProcessException with "Missing reserved completion slot";
                                end if;
                                cq.ring(cq.tail) :=
                                    (requestId =>
                                        proctab(p).pendingRequests(r)
                                            .requestId,
                                     token =>
                                        proctab(p).pendingRequests(r)
                                            .token,
                                     msg       => NULL_MESSAGE,
                                     from      => pid,
                                     status    => COMPLETION_TARGET_DIED,
                                     valid     => True);
                                cq.tail := (cq.tail + 1) mod
                                    COMPLETION_QUEUE_SIZE;
                                cq.count := cq.count + 1;
                                if proctab(p).state =
                                    WAITINGFORCOMPLETION
                                then
                                    ready (p);
                                end if;
                            end if;
                        end loop;
                        proctab(p).numPending := writeIdx;
                        for r in writeIdx .. MAX_PENDING_ASYNC - 1 loop
                            proctab(p).pendingRequests(r) :=
                                (NO_PROCESS, NO_REQUEST_ID, 0);
                        end loop;
                    end;
                end if;
            end if;
            Spinlocks.exitCriticalSection (lock);
            Spinlocks.exitCriticalSection (mailtab(p).lock);
        end loop;
    end retireMailboxes;

    procedure sendRetirementEvent
      (dest : ProcessID; generation : Capabilities.Generation; msg : Message) is
        ok : Boolean;
    begin
        Spinlocks.enterCriticalSection (mailtab(dest).lock);
        if not mailtab(dest).closed and then
           proctab(dest).capGeneration = generation
        then
            enqueueRing (dest, (msg => msg, sender => PerCPUData.getCurrentPID,
                         kind => RING_EVENT, requestId => NO_REQUEST_ID), ok);
            if ok then
                if proctab(dest).state = RECEIVING then
                    Queues.detach (mailtab(dest).recvQueue, dest);
                    notify (dest);
                elsif proctab(dest).state = WAITINGFOREVENT then
                    notify (dest);
                end if;
            end if;
        end if;
        Spinlocks.exitCriticalSection (mailtab(dest).lock);
    end sendRetirementEvent;

end Process.IPC;
