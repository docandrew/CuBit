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
with Grant_Page_Installation;
with IPI;
with Memory_Grants;
with Memory_Grants.Loans;
with Retained_Record_Blocks;
with System.Storage_Elements;
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
use type Memory_Grants.Reference;
use type Memory_Grants.Grant_Generation;
use type Memory_Grants.Hold_Release_Result;

-- Ada implementation: custom storage, address overlays or live context state.
-- Only separately annotated SPARK policy/state routines carry proof obligations.
package body Process.IPC is

    -- Multi-owner IPC operations use ascending PID order. A completing reply
    -- releases its caller lock before the target-locked scheduling handoff.
    procedure lockMailboxes (A, B : ProcessID);
    procedure unlockMailboxes (A, B : ProcessID);

    ---------------------------------------------------------------------------
    -- getReceiver
    -- Determine which mailbox to use for receive operations. If the caller
    -- is a thread, use the parent's mailbox.
    ---------------------------------------------------------------------------
    function getReceiver (pid : ProcessID) return ProcessID
    is
    begin
        return pid;
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
                                 thread  : in  ThreadID;
                                 success : out Boolean)

    is
        cq : CompletionQueue renames completionTab(owner);
    begin
        if mailtab(owner).closed or else cq.count >= COMPLETION_QUEUE_SIZE then
            success := False;
            return;
        end if;

        cq.ring(cq.tail) := item;
        cq.owners(cq.tail) := thread;
        cq.tail  := (cq.tail + 1) mod COMPLETION_QUEUE_SIZE;
        cq.count := cq.count + 1;
        success  := True;
    end enqueueCompletion;

    ---------------------------------------------------------------------------
    -- dequeueCompletion
    -- Remove a completion entry from a process' completion queue.
    -- Caller must hold mailtab(owner).lock.
    ---------------------------------------------------------------------------
    -- The oldest entry for thread (or, unless strict, one owned by no
    -- thread). Later entries shift toward the head, keeping FIFO order.
    procedure dequeueCompletion (owner   : in  ProcessID;
                                 thread  : in  ThreadID;
                                 item    : out CompletionEntry;
                                 success : out Boolean;
                                 strict  : in  Boolean := False)

    is
        cq : CompletionQueue renames completionTab(owner);
        idx, following : CompletionIndex;
    begin
        item    := NULL_COMPLETION;
        success := False;
        for i in 0 .. cq.count - 1 loop
            idx := (cq.head + i) mod COMPLETION_QUEUE_SIZE;
            if cq.owners(idx) = thread or else
               (not strict and then cq.owners(idx) = NO_THREAD)
            then
                item := cq.ring(idx);
                for j in i .. cq.count - 2 loop
                    idx := (cq.head + j) mod COMPLETION_QUEUE_SIZE;
                    following := (idx + 1) mod COMPLETION_QUEUE_SIZE;
                    cq.ring(idx) := cq.ring(following);
                    cq.owners(idx) := cq.owners(following);
                end loop;
                cq.tail := (cq.tail + COMPLETION_QUEUE_SIZE - 1) mod COMPLETION_QUEUE_SIZE;
                cq.ring(cq.tail) := NULL_COMPLETION;
                cq.owners(cq.tail) := NO_THREAD;
                cq.count := cq.count - 1;
                success := True;
                return;
            end if;
        end loop;
    end dequeueCompletion;

    -- Completions waiting for thread (or owned by no thread).
    function completionsFor (owner : ProcessID; thread : ThreadID) return Natural is
        cq : CompletionQueue renames completionTab(owner);
        n : Natural := 0;
        idx : CompletionIndex;
    begin
        for i in 0 .. cq.count - 1 loop
            idx := (cq.head + i) mod COMPLETION_QUEUE_SIZE;
            if cq.owners(idx) = thread or else cq.owners(idx) = NO_THREAD then
                n := n + 1;
            end if;
        end loop;
        return n;
    end completionsFor;

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
                                    thread  : out ThreadID;
                                    found   : out Boolean)

    is
    begin
        found := False;
        token := 0;
        thread := NO_THREAD;

        for i in 0 .. proctab(sender).numPending - 1 loop
            if (requestId /= NO_REQUEST_ID and then
                proctab(sender).pendingRequests(i).requestId = requestId)
               or else
               (requestId = NO_REQUEST_ID and then
                proctab(sender).pendingRequests(i).dest = replier)
            then
                token := proctab(sender).pendingRequests(i).token;
                thread := proctab(sender).pendingRequests(i).thread;

                -- Swap-remove: replace with last entry
                proctab(sender).numPending := proctab(sender).numPending - 1;

                if i < proctab(sender).numPending then
                    proctab(sender).pendingRequests(i) :=
                        proctab(sender).pendingRequests(proctab(sender).numPending);
                end if;

                proctab(sender).pendingRequests(proctab(sender).numPending) :=
                    NO_PENDING;

                found := True;
                return;
            end if;
        end loop;
    end findAndRemovePending;

    ---------------------------------------------------------------------------
    -- replyTargetOf
    -- Resolve a reply capability to the process and thread it answers. A
    -- synchronous reply (no request ID) names the blocked sender thread and
    -- carries that thread's generation; an asynchronous reply names the
    -- submitting process and carries its generation, since the completion
    -- belongs to the process. A stale capability resolves to NO_PROCESS.
    ---------------------------------------------------------------------------
    procedure replyTargetOf (cap : Capabilities.Capability;
                             pid : out ProcessID;
                             tid : out ThreadID)
    is
    begin
        pid := NO_PROCESS;
        tid := NO_THREAD;
        if cap.object.ref = 0 then
            return;
        end if;
        if cap.object.param = NO_REQUEST_ID then
            if cap.object.ref <= Unsigned_64 (ThreadID'Last) and then
               cap.gen = threadGenerationOf (ThreadID (cap.object.ref))
            then
                tid := ThreadID (cap.object.ref);
                pid := processOf (tid);
                if pid = NO_PROCESS then
                    tid := NO_THREAD;
                end if;
            end if;
        elsif cap.object.ref <= Unsigned_64 (ProcessID'Last) and then
              cap.gen = generationOf (ProcessID (cap.object.ref))
        then
            pid := ProcessID (cap.object.ref);
        end if;
    end replyTargetOf;

    ---------------------------------------------------------------------------
    -- consumeReplyAuthority
    -- Validate and consume one of the caller's one-use reply caps for
    -- replyTo: first the calling thread's own (from its latest receive),
    -- then deferred reply caps in the process table. Returns the request ID
    -- and, for a synchronous reply, the waiting thread. Deferred caps that
    -- answer different threads of replyTo are ambiguous by PID alone; such a
    -- reply fails and the server must use replyCap with an explicit slot.
    ---------------------------------------------------------------------------
    procedure consumeReplyAuthority
        (caller       : in  ProcessID;
         callerThread : in  ThreadID;
         replyTo      : in  ProcessID;
         replyThread  : out ThreadID;
         requestId    : out Unsigned_64;
         ok           : out Boolean)

    is
        cap        : Capabilities.Capability;
        targetPID  : ProcessID;
        targetTID  : ThreadID;
        foundSlot  : Capabilities.CapabilitySlot := 0;
        matches    : Natural := 0;
    begin
        requestId   := NO_REQUEST_ID;
        replyThread := NO_THREAD;
        ok          := False;

        if threadtab (callerThread).mode = KERNEL then
            replyThread := mainThreadOf (replyTo);
            ok := True;
            return;
        end if;

        -- Fast path: the calling thread's own reply authority.
        replyTargetOf (threadtab (callerThread).replyCap, targetPID, targetTID);
        if targetPID = replyTo then
            Capabilities.Operations.takeReplyCapFrom
              (source => threadtab (callerThread).replyCap,
               cap    => cap,
               taken  => ok);
            if ok then
                requestId := cap.object.param;
                replyThread := targetTID;
            end if;
            return;
        end if;

        -- Slow path: deferred reply cap slots, via the bitmap.
        bitmapScan : declare
            remaining : Unsigned_64 := proctab(caller).deferredReplyCaps;
            s : Natural;
        begin
            while remaining /= 0 loop
                s := Util.getFirstSetBit (remaining);
                replyTargetOf (proctab(caller).caps(s), targetPID, targetTID);
                if targetPID = replyTo then
                    matches := matches + 1;
                    foundSlot := s;
                end if;
                remaining := remaining and (remaining - 1);
            end loop;
        end bitmapScan;

        if matches /= 1 then
            return;
        end if;

        Capabilities.Operations.retireReplyCap
          (table => proctab(caller).caps,
           deferredSlots => proctab(caller).deferredReplyCaps,
           slot  => foundSlot,
           cap   => cap,
           taken => ok);
        if ok then
            replyTargetOf (cap, targetPID, replyThread);
            requestId := cap.object.param;
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
             (tag      => (label => 1, length => 0, flags => 0, reserved => 0),
              authorityTag => 0,
              words    => (others => 0)),
           sender    => NO_PROCESS,
           kind      => RING_EVENT,
           requestId => NO_REQUEST_ID,
           senderThread => NO_THREAD);
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

    -- Called with the receiver mailbox locked. Each successful take advances
    -- a persistent lane cursor. A continuously available eligible lane waits
    -- behind at most two other takes (one for service-only receive). FIFO
    -- order within each lane is preserved; this is not a wall-clock guarantee.
    procedure takeMailboxWork
      (receiver : ProcessID; serviceOnly : Boolean;
       item : out RingEntry; found : out Boolean)
    is
        lane : Receive_Lane := mailtab(receiver).nextReceiveLane;
        sender : ThreadID;
    begin
        item := NULL_RING_ENTRY;
        found := False;
        for probe in Receive_Lane loop
            case lane is
                when Queued_Messages =>
                    if serviceOnly then
                        dequeueRingServiceRequest (receiver, item, found);
                    else
                        dequeueRing (receiver, item, found);
                    end if;
                when Waiting_Senders =>
                    if not Queues.isEmpty (mailtab(receiver).sendQueue) then
                        Queues.dequeue (mailtab(receiver).sendQueue, sender);
                        item := (msg => threadtab (sender).sendMsg,
                                 sender => processOf (sender),
                                 kind => RING_SYNC,
                                 requestId => NO_REQUEST_ID,
                                 senderThread => sender);
                        threadtab (sender).state := WAITINGFORREPLY;
                        found := True;
                    end if;
                when IRQ_Doorbell =>
                    if not serviceOnly then
                        takeIRQDoorbell (receiver, item, found);
                    end if;
            end case;
            lane := (if lane = Receive_Lane'Last then Receive_Lane'First
                     else Receive_Lane'Succ (lane));
            if found then
                mailtab(receiver).nextReceiveLane := lane;
                return;
            end if;
        end loop;
    end takeMailboxWork;

    -- Only a successfully dequeued reply-bearing request mints authority.
    -- One-way messages, events and empty polls never inherit an older reply.
    -- The authority goes to the receiving thread. A synchronous request's
    -- authority names the blocked sender thread (see replyTargetOf).
    procedure installReceivedWork
      (me : ThreadID; item : RingEntry; found : Boolean;
       from : out ProcessID; msg : out Message)
    is
    begin
        from := (if found then item.sender else NO_PROCESS);
        msg := (if found then item.msg else NULL_MESSAGE);
        if found and then from /= NO_PROCESS and then
           item.kind = RING_SYNC and then item.senderThread /= NO_THREAD
        then
            threadtab (me).replyCap :=
                (capType => Capabilities.CAP_REPLY,
                 rights => Capabilities.ALL_RIGHTS,
                 authorityTag => Capabilities.NO_AUTHORITY_TAG,
                 object => (ref => Unsigned_64 (item.senderThread),
                            param => NO_REQUEST_ID),
                 gen => threadGenerationOf (item.senderThread));
        elsif found and then from /= NO_PROCESS and then
           item.kind = RING_ASYNC_REQUEST
        then
            threadtab (me).replyCap :=
                (capType => Capabilities.CAP_REPLY,
                 rights => Capabilities.ALL_RIGHTS,
                 authorityTag => Capabilities.NO_AUTHORITY_TAG,
                 object => (ref => Unsigned_64 (from), param => item.requestId),
                 gen => generationOf (from));
        else
            threadtab (me).replyCap := Capabilities.NULL_CAPABILITY;
        end if;
    end installReceivedWork;

    -- Caller holds mailtab(owner).lock AND Process.lock. Queue membership is
    -- stable under these locks; detach before making a waiter runnable.
    procedure wakeActivityWaitersLocked (owner : ProcessID) is
        waiter : ThreadID := mailtab(owner).recvQueue.head;
        following : ThreadID;
    begin
        -- Walk waiting threads (not processes): every thread waiting for
        -- activity on this mailbox is woken.
        while waiter /= NO_THREAD loop
            following := threadtab (waiter).next;
            if threadtab (waiter).waitsForIPCActivity then
                Queues.detach (mailtab(owner).recvQueue, waiter);
                threadtab (waiter).receiveDeadlineActive := False;
                ready (waiter);
            end if;
            waiter := following;
        end loop;
    end wakeActivityWaitersLocked;

    -- Caller holds the mailbox lock. The common no-waiter path does not
    -- acquire the scheduler lock just to discover an empty receive queue.
    procedure wakeActivityWaiters (owner : ProcessID) is
    begin
        if not Queues.isEmpty (mailtab(owner).recvQueue) then
            Spinlocks.enterCriticalSection (lock);
            wakeActivityWaitersLocked (owner);
            Spinlocks.exitCriticalSection (lock);
        end if;
    end wakeActivityWaiters;

    -- Caller holds mailtab(owner).lock and Process.lock. Every thread waiting
    -- for an event or completion rechecks its condition when it runs.
    procedure wakeNotifyWaitersLocked (owner : ProcessID) is
        waiter : ThreadID;
    begin
        while not Queues.isEmpty (mailtab(owner).notifyQueue) loop
            Queues.dequeue (mailtab(owner).notifyQueue, waiter);
            if threadtab (waiter).state in WAITINGFOREVENT | WAITINGFORCOMPLETION
               and then not Process_Lifetime.Closing (threadtab (waiter).lifetime)
            then
                ready (waiter);
            end if;
        end loop;
    end wakeNotifyWaitersLocked;

    -- Caller holds mailtab(owner).lock.
    procedure wakeNotifyWaiters (owner : ProcessID) is
    begin
        if not Queues.isEmpty (mailtab(owner).notifyQueue) then
            Spinlocks.enterCriticalSection (lock);
            wakeNotifyWaitersLocked (owner);
            Spinlocks.exitCriticalSection (lock);
        end if;
    end wakeNotifyWaiters;

    -- Caller holds mailtab(owner).lock. Unsolicited work (an event, IRQ
    -- doorbell or one-way message) wakes one blocked receiver, which may
    -- consume it, and every event waiter.
    procedure wakeForUnsolicitedWork (owner : ProcessID) is
        receiver : ThreadID;
    begin
        Spinlocks.enterCriticalSection (lock);
        if not Queues.isEmpty (mailtab(owner).recvQueue) then
            -- Prefer a receiving thread on this CPU (no IPI, no idle exit).
            Queues.dequeuePreferring
              (mailtab(owner).recvQueue, PerCPUData.getCPUNumber, receiver);
            threadtab (receiver).receiveDeadlineActive := False;
            if threadtab (receiver).state = RECEIVING and then
               not Process_Lifetime.Closing (threadtab (receiver).lifetime)
            then
                ready (receiver);
            end if;
        end if;
        wakeNotifyWaitersLocked (owner);
        wakeActivityWaitersLocked (owner);
        Spinlocks.exitCriticalSection (lock);
    end wakeForUnsolicitedWork;

    function waitForActivityUntil (deadlineMs : Unsigned_64)
      return Unsigned_64
    is
        mypid : constant ProcessID := PerCPUData.getCurrentPID;
        me    : constant ThreadID := PerCPUData.getCurrentThread;
        receiver : constant ProcessID := getReceiver (mypid);
        ignored : ThreadID;
    begin
        loop
            Spinlocks.enterCriticalSection (mailtab(receiver).lock);
            if mailtab(receiver).closed then
                Spinlocks.exitCriticalSection (mailtab(receiver).lock);
                return Unsigned_64'Last;
            end if;
            if mailtab(receiver).ring.count /= 0 or else
               not Queues.isEmpty (mailtab(receiver).sendQueue) or else
               proctab(receiver).irqNotificationPending or else
               completionsFor (receiver, me) /= 0
            then
                Spinlocks.exitCriticalSection (mailtab(receiver).lock);
                return 1;
            end if;
            if Time.msTicks >= deadlineMs then
                Spinlocks.exitCriticalSection (mailtab(receiver).lock);
                return 0;
            end if;
            threadtab (me).queueKey := receiver;
            threadtab (me).waitsForIPCActivity := True;
            threadtab (me).receiveDeadlineMs := deadlineMs;
            threadtab (me).receiveDeadlineReceiver := receiver;
            threadtab (me).receiveDeadlineActive :=
              deadlineMs /= Unsigned_64'Last;
            Queues.enqueue (mailtab(receiver).recvQueue, me, ignored);
            threadtab (me).state := RECEIVING;
            Spinlocks.exitCriticalSection (mailtab(receiver).lock);
            yield;
            Spinlocks.enterCriticalSection (mailtab(receiver).lock);
            threadtab (me).waitsForIPCActivity := False;
            threadtab (me).receiveDeadlineActive := False;
            threadtab (me).receiveDeadlineMs := 0;
            threadtab (me).receiveDeadlineReceiver := NO_PROCESS;
            Spinlocks.exitCriticalSection (mailtab(receiver).lock);
            -- A sibling receiver may have consumed the work. Recheck under
            -- the lock before sleeping again; no dequeue or reply-cap mint.
        end loop;
    end waitForActivityUntil;

    procedure receiveInternal
        (hasDeadline : in  Boolean;
         deadlineMs  : in  Unsigned_64;
         from        : out ProcessID;
         msg         : out Message;
         received    : out Boolean)
    is
        mypid    : constant ProcessID := PerCPUData.getCurrentPID;
        me       : constant ThreadID := PerCPUData.getCurrentThread;
        receiver : constant ProcessID := getReceiver (mypid);
        ignore   : ThreadID;
        re       : RingEntry;
    begin
        if mypid = NO_PROCESS then
            from := NO_PROCESS;
            msg := NULL_MESSAGE;
            received := False;
            return;
        end if;

        Spinlocks.enterCriticalSection (mailtab(receiver).lock);
        takeMailboxWork (receiver, False, re, received);
        if received then
            installReceivedWork (me, re, True, from, msg);
            Spinlocks.exitCriticalSection (mailtab(receiver).lock);
            return;
        end if;

        if hasDeadline and then Time.msTicks >= deadlineMs then
            installReceivedWork (me, re, False, from, msg);
            Spinlocks.exitCriticalSection (mailtab(receiver).lock);
            return;
        end if;

        -- Publication and registration of this wait share the mailbox lock.
        threadtab (me).queueKey := receiver;
        threadtab (me).receiveDeadlineMs := deadlineMs;
        threadtab (me).receiveDeadlineReceiver := receiver;
        threadtab (me).receiveDeadlineActive := hasDeadline;
        Queues.enqueue (mailtab(receiver).recvQueue, me, ignore);
        threadtab (me).state := RECEIVING;
        Spinlocks.exitCriticalSection (mailtab(receiver).lock);
        yield;

        Spinlocks.enterCriticalSection (mailtab(receiver).lock);
        threadtab (me).receiveDeadlineActive := False;
        threadtab (me).receiveDeadlineMs := 0;
        threadtab (me).receiveDeadlineReceiver := NO_PROCESS;
        takeMailboxWork (receiver, False, re, received);
        installReceivedWork (me, re, received, from, msg);
        Spinlocks.exitCriticalSection (mailtab(receiver).lock);
    end receiveInternal;

    ---------------------------------------------------------------------------
    -- receive
    --
    -- Fairly select available queued traffic, synchronous senders, and IRQ
    -- doorbells. If none is available, atomically register a blocking receive.
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
        removed  : ThreadID;
    begin
        for tid in ThreadID range 1 .. ThreadID (Thread_Table.High_Water) loop
            if threadtab (tid).receiveDeadlineActive and then
               threadtab (tid).receiveDeadlineMs <= nowMs
            then
                receiver := threadtab (tid).receiveDeadlineReceiver;
                if receiver /= NO_PROCESS then
                    Spinlocks.enterCriticalSection (mailtab(receiver).lock);
                    Spinlocks.enterCriticalSection (lock);
                    if threadtab (tid).state = RECEIVING and then
                       threadtab (tid).receiveDeadlineActive and then
                       threadtab (tid).receiveDeadlineReceiver = receiver and then
                       threadtab (tid).receiveDeadlineMs <= nowMs
                    then
                        Queues.popItem
                          (mailtab(receiver).recvQueue, tid, removed);
                        if removed = tid then
                            threadtab (tid).receiveDeadlineActive := False;
                            ready (tid);
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
        me       : constant ThreadID := PerCPUData.getCurrentThread;
        receiver : constant ProcessID := getReceiver (mypid);
        re       : RingEntry;
        ok       : Boolean;
        ignore   : ThreadID;
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
            threadtab (me).state := WAITINGFOREVENT;
            Queues.enqueue (mailtab(receiver).notifyQueue, me, ignore);
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

    procedure receiveServiceRequestNB (from : out ProcessID;
                                       msg : out Message;
                                       found : out Boolean)
    is
        mypid : constant ProcessID := PerCPUData.getCurrentPID;
        me    : constant ThreadID := PerCPUData.getCurrentThread;
        receiver : constant ProcessID := getReceiver (mypid);
        re : RingEntry;
    begin
        Spinlocks.enterCriticalSection (mailtab(receiver).lock);
        takeMailboxWork (receiver, True, re, found);
        installReceivedWork (me, re, found, from, msg);
        Spinlocks.exitCriticalSection (mailtab(receiver).lock);
    end receiveServiceRequestNB;

    ---------------------------------------------------------------------------
    -- receiveAnyIpcNB
    -- Non-blocking mixed receive across all mailbox traffic classes:
    -- it may consume service requests, one-way messages, and events. Use only
    -- for intentionally mixed dispatch loops.
    ---------------------------------------------------------------------------
    procedure receiveAnyIpcNB (from : out ProcessID;
                                       msg : out Message;
                                       found : out Boolean)
    is
        mypid : constant ProcessID := PerCPUData.getCurrentPID;
        me    : constant ThreadID := PerCPUData.getCurrentThread;
        receiver : constant ProcessID := getReceiver (mypid);
        re : RingEntry;
    begin
        Spinlocks.enterCriticalSection (mailtab(receiver).lock);
        takeMailboxWork (receiver, False, re, found);
        installReceivedWork (me, re, found, from, msg);
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
        me       : constant ThreadID := PerCPUData.getCurrentThread;
        receiver : ThreadID;
        replyTag : MessageTag;
        ignore   : ThreadID;
    begin
        -- Validate destination
        if dest = NO_PROCESS then
            return NULL_TAG;
        end if;

        if threadOf (dest).state = INVALID then
            return NULL_TAG;
        end if;

        Spinlocks.enterCriticalSection (mailtab(dest).lock);
        if mailtab(dest).closed or else
           (expectedGeneration /= 0 and then
            expectedGeneration /= generationOf (dest))
        then
            Spinlocks.exitCriticalSection (mailtab(dest).lock);
            return NULL_TAG;
        end if;

        -- Capability enforcement for legacy PID-based send.
        -- Kernel threads are exempt (they have no cap table).
        if Config.ENFORCE_IPC_CAPS
           and then threadtab (me).mode = USER
        then
            enforceCheck : declare
                found : Boolean := False;
            begin
                for i in Capabilities.CapabilitySlot loop
                    if proctab(pid).caps(i).capType = Capabilities.CAP_ENDPOINT
                       and then proctab(pid).caps(i).object.ref = Unsigned_64(dest)
                       and then proctab(pid).caps(i).rights(Capabilities.RIGHT_WRITE)
                       and then proctab(pid).caps(i).gen =
                                generationOf (dest)
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
        threadtab (me).sendMsg := msg;

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
                              requestId => NO_REQUEST_ID,
                              senderThread => me),
                             ok);
                if not ok then
                    Spinlocks.exitCriticalSection (mailtab(dest).lock);
                    return NULL_TAG;
                end if;
            end enqueueP1;

            -- A receiving thread on this CPU, if the destination has one,
            -- so the call can be a direct handoff (else the longest waiter).
            Queues.dequeuePreferring
              (mailtab(dest).recvQueue, PerCPUData.getCPUNumber, receiver);

            -- Sender goes to WAITINGFORREPLY
            threadtab (me).state := WAITINGFORREPLY;

            -- Acquire Process.lock BEFORE releasing mailtab.lock to
            -- close the window where receiver could be killed/migrated.
            -- Lock ordering: mailtab < Process.lock (documented).
            if threadtab (receiver).cpu = PerCPUData.getCPUNumber then
                Spinlocks.enterCriticalSection (lock);
                Spinlocks.exitCriticalSection (mailtab(dest).lock);
                directSwitch (me, receiver);
                Spinlocks.exitCriticalSection (lock);

                -- Resumed: reply delivered via directSwitch from reply()
                replyTag := threadtab (me).replyMsg.tag;
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
            threadtab (me).queueKey := dest;
            Queues.enqueue (mailtab(dest).sendQueue, me, ignore);
            threadtab (me).state := SENDING;

            Spinlocks.exitCriticalSection (mailtab(dest).lock);
        end if;

        -- Path 2: yield and wait for receiver to dequeue us
        yield;

        -- Reply delivered — replyMsg populated by reply()
        replyTag := threadtab (me).replyMsg.tag;

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
    begin
        accepted := False;

        -- Validate destination
        if dest = NO_PROCESS then
            return;
        end if;

        if threadOf (dest).state = INVALID then
            return;
        end if;

        Spinlocks.enterCriticalSection (mailtab(dest).lock);

        if mailtab(dest).closed or else
           (expectedGeneration /= 0 and then
            expectedGeneration /= generationOf (dest))
        then
            Spinlocks.exitCriticalSection (mailtab(dest).lock);
            return;
        end if;

        enqueueRing (dest,
                     (msg       => msg,
                      sender    => NO_PROCESS,
                      kind      => RING_EVENT,
                      requestId => NO_REQUEST_ID,
                      senderThread => NO_THREAD),
                     accepted);

        if not accepted then
            proctab(dest).eventDrops := proctab(dest).eventDrops + 1;
        end if;

        --  receive() is the intentional mixed-lane wait primitive: it may
        --  consume events as well as requests. Wake both event-specific and
        --  mixed waiters whenever unsolicited work is published.
        wakeForUnsolicitedWork (dest);

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
    begin
        if dest = NO_PROCESS or else threadOf (dest).state = INVALID then
            return;
        end if;

        Spinlocks.enterCriticalSection (mailtab(dest).lock);
        if mailtab(dest).closed then
            Spinlocks.exitCriticalSection (mailtab(dest).lock);
            return;
        end if;
        proctab(dest).irqNotificationPending := True;

        wakeForUnsolicitedWork (dest);

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
                         reserved  => 0);
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
      (replyTo : ProcessID; replyThread : ThreadID;
       requestId : Unsigned_64; msg : Message)
      return Unsigned_64
    is
        me : constant ThreadID := PerCPUData.getCurrentThread;
        mypid : constant ProcessID := processOf (me);
        token : Unsigned_64;
        submitter : ThreadID;
        ok : Boolean;
    begin
        if requestId = NO_REQUEST_ID then
            if replyThread = NO_THREAD or else
               processOf (replyThread) /= replyTo or else
               threadtab (replyThread).state /= WAITINGFORREPLY
            then
                Spinlocks.exitCriticalSection (mailtab(replyTo).lock);
                return 0;
            end if;
            Spinlocks.enterCriticalSection (lock);
            threadtab (replyThread).replyMsg := msg;
            if threadtab (replyThread).cpu = PerCPUData.getCPUNumber then
                ready (me);
                Spinlocks.exitCriticalSection (mailtab(replyTo).lock);
                directSwitch (me, replyThread);
                Spinlocks.exitCriticalSection (lock);
            else
                ready (replyThread);
                Spinlocks.exitCriticalSection (lock);
                Spinlocks.exitCriticalSection (mailtab(replyTo).lock);
            end if;
        else
            findAndRemovePending (replyTo, mypid, requestId, token, submitter, ok);
            if not ok then
                Spinlocks.exitCriticalSection (mailtab(replyTo).lock);
                return 0;
            end if;
            enqueueCompletion
              (owner => replyTo,
               item => (requestId => requestId, token => token, msg => msg,
                        from => Unsigned_64 (mypid),
                        status => COMPLETION_OK, valid => True,
                        reserved => (others => 0)),
               thread => submitter,
               success => ok);
            if not ok then
                raise ProcessException with "Missing reserved completion slot";
            end if;
            wakeNotifyWaiters (replyTo);
            wakeActivityWaiters (replyTo);
            Spinlocks.exitCriticalSection (mailtab(replyTo).lock);
        end if;
        return 1;
    end completeReplyLocked;

    function reply (replyTo : ProcessID; msg : Message) return Unsigned_64 is
        mypid : constant ProcessID := PerCPUData.getCurrentPID;
        me    : constant ThreadID := PerCPUData.getCurrentThread;
        requestId : Unsigned_64;
        replyThread : ThreadID;
        ok : Boolean;
    begin
        if replyTo = NO_PROCESS then return 0; end if;
        lockMailboxes (mypid, replyTo);
        if mailtab(replyTo).closed then
            unlockMailboxes (mypid, replyTo);
            return 0;
        end if;
        consumeReplyAuthority (mypid, me, replyTo, replyThread, requestId, ok);
        if not ok then
            unlockMailboxes (mypid, replyTo);
            return 0;
        end if;
        if mypid /= replyTo then
            Spinlocks.exitCriticalSection (mailtab(mypid).lock);
        end if;
        return completeReplyLocked (replyTo, replyThread, requestId, msg);
    end reply;

    function replyCap
      (capSlot : Capabilities.CapabilitySlot; msg : Message)
      return Unsigned_64
    is
        mypid : constant ProcessID := PerCPUData.getCurrentPID;
        cap : Capabilities.Capability;
        replyTo : ProcessID;
        lockedPID : ProcessID;
        replyThread : ThreadID;
        ok : Boolean;
    begin
        if mypid = NO_PROCESS then return 0; end if;
        -- Consume the explicitly selected one-use reply even when delivery
        -- will fail. Otherwise a departed caller strands the server's slot.
        -- Non-reply authority is preserved. Serialize with policy-authorized
        -- edits of this cspace, not just other executions of this process.
        -- REPLY_CAP_SLOT names the calling thread's current reply authority.
        Spinlocks.enterCriticalSection (mailtab(mypid).lock);
        if capSlot = Capabilities.REPLY_CAP_SLOT then
            Capabilities.Operations.takeReplyCapFrom
              (threadtab (PerCPUData.getCurrentThread).replyCap, cap, ok);
        else
            Capabilities.Operations.retireReplyCap
              (proctab(mypid).caps, proctab(mypid).deferredReplyCaps,
               capSlot, cap, ok);
        end if;
        Spinlocks.exitCriticalSection (mailtab(mypid).lock);
        if not ok then return 0; end if;
        replyTargetOf (cap, lockedPID, replyThread);
        if lockedPID = NO_PROCESS then return 0; end if;
        Spinlocks.enterCriticalSection (mailtab(lockedPID).lock);
        -- Recheck under the target's mailbox lock: teardown closes it
        -- before either generation can advance.
        replyTargetOf (cap, replyTo, replyThread);
        if replyTo /= lockedPID or else mailtab(lockedPID).closed then
            Spinlocks.exitCriticalSection (mailtab(lockedPID).lock);
            return 0;
        end if;
        return completeReplyLocked (replyTo, replyThread, cap.object.param, msg);
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

    --  Body-local: only capSubmit may enqueue a resolved endpoint request.
    function submitResolvedEndpoint (dest  : ProcessID;
                     msg   : Message;
                     token : Unsigned_64;
                     expectedGeneration : Capabilities.Generation) return Boolean

    is
        pid      : constant ProcessID := PerCPUData.getCurrentPID;
        receiver : ThreadID;
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

        if threadOf (dest).state = INVALID then
            return False;
        end if;

        lockMailboxes (pid, dest);
        if mailtab(pid).closed or else mailtab(dest).closed or else
           expectedGeneration /= generationOf (dest)
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
            declare
                allocation : constant IPC_Request_Ids.Allocation :=
                  IPC_Request_Ids.Next (proctab(pid).requestSequence);
            begin
                if not allocation.Available then
                    unlockMailboxes (pid, dest);
                    return False;
                end if;
                requestId := allocation.Id;
                -- Commit under the caller's mailbox lock, together with
                -- pending/completion reservation and publication. A failed
                -- enqueue may burn an ID but must never permit its reuse.
                proctab(pid).requestSequence :=
                  IPC_Request_Ids.Sequence (allocation.Id);
            end;
        end if;

        -- Enqueue in unified ring
        enqueueRing (dest,
                     (msg       => msg,
                      sender    => pid,
                      kind      => entryKind,
                      requestId => requestId,
                      senderThread => NO_THREAD),
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
                 token     => token,
                 thread    => PerCPUData.getCurrentThread);
            proctab(pid).numPending := proctab(pid).numPending + 1;
        end if;

        -- Wake receiver if one is waiting
        if not Queues.isEmpty (mailtab(dest).recvQueue) then
            Spinlocks.enterCriticalSection (lock);
            Queues.dequeue (mailtab(dest).recvQueue, receiver);
            ready (receiver);
            Spinlocks.exitCriticalSection (lock);
        elsif threadOf (dest).state = SLEEPING then
            declare
                woken : Boolean;
            begin
                Queues.wakeFromSleep (mainThreadOf (dest), woken);
            end;
        end if;

        unlockMailboxes (pid, dest);

        -- Do NOT block — caller keeps running
        return True;
    end submitResolvedEndpoint;

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
        me       : constant ThreadID := PerCPUData.getCurrentThread;
        receiver : constant ProcessID := getReceiver (mypid);
        drained  : Natural := 0;
        item     : CompletionEntry;
        ok       : Boolean;
        effectiveMax : Natural;
        effectiveMin : Natural;
        ignore       : ThreadID;
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

            if completionsFor (receiver, me) >= effectiveMin then
                -- Drain up to effectiveMax entries into user buffer
                x86.stac;
                while drained < effectiveMax loop
                    dequeueCompletion (receiver, me, item, ok);
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
            threadtab (me).state := WAITINGFORCOMPLETION;
            Queues.enqueue (mailtab(receiver).notifyQueue, me, ignore);
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

        dequeueCompletion (receiver, PerCPUData.getCurrentThread, result, found);

        Spinlocks.exitCriticalSection (mailtab(receiver).lock);
    end pollCompletion;

    ---------------------------------------------------------------------------
    -- Shared Memory Grant Operations
    ---------------------------------------------------------------------------

    package Grant_Loans is new Memory_Grants.Loans;
    use type Grant_Loans.Parent_Phase;
    use type Grant_Loans.Loan_Phase;
    use type Grant_Loans.Reservation_Result;
    -- Only grantLock accesses these tables. Keep scope state off kernel stacks
    -- and reset it only when the associated grant identity is invalidated.
    Empty_Forwarding_Scope : Grant_Loans.State; -- never mutated
    -- Allocate forwarding metadata only for admitted delegation. Page blocks
    -- remain at stable kernel addresses for the kernel lifetime; invalidation
    -- resets only the retired identity's scope, never a live neighbour.
    Scopes_Per_Block : constant Positive :=
      4096 / (Grant_Loans.State'Object_Size / System.Storage_Unit);
    function allocateGrantMetadataBlock
      (Bytes, Alignment : System.Storage_Elements.Storage_Count)
       return System.Address
    is
        address : System.Address;
    begin
        if Bytes > 4096 or else Alignment > 4096 then
            return System.Null_Address;
        end if;
        BuddyAllocator.alloc (0, address);
        return address;
    end allocateGrantMetadataBlock;

    package Grant_Storage is new Retained_Record_Blocks
      (Grant, GrantID'Last, 64, allocateGrantMetadataBlock);
    subtype Grant_Pointer is Grant_Storage.Element_Access;
    use type Grant_Pointer;
    use type Memory_Grants.Process_Generation;
    Grant_Stores : array (ProcessID) of Grant_Storage.Store;
    Grant_Lives : array (ProcessID) of Memory_Grants.Process_Generation := (others => 0);
    Grant_Life_Ready : array (ProcessID) of Boolean := (others => False);

    -- These stores outlive process-table pages. In particular, endpoint
    -- invalidation must not hide grants still awaiting their final reader.
    function grantFor (owner : ProcessID; slot : GrantID) return Grant_Pointer is
      (Grant_Storage.Find (Grant_Stores (owner), slot));

    function grantFor (slot : Memory_Grants.Global_Slot) return Grant_Pointer is
      (grantFor (ProcessID (Memory_Grants.Owner_Of (slot)),
        GrantID (Memory_Grants.Local_Slot_Of (slot))));

    function initializeGrantLife
      (pid : ProcessID; life : Memory_Grants.Process_Generation) return Boolean
    is
        slot : GrantID := GrantID'First;
        present : Boolean;
        value : Grant_Pointer;
    begin
        if pid = NO_PROCESS then return False; end if;
        Spinlocks.enterCriticalSection (grantLock);
        if Grant_Life_Ready (pid) and then life <= Grant_Lives (pid) then
            Spinlocks.exitCriticalSection (grantLock);
            return False;
        end if;
        -- Two passes: rejection must leave all existing records unchanged.
        loop
            Grant_Storage.Next_Present (Grant_Stores (pid), slot, slot, present);
            exit when not present;
            value := grantFor (pid, slot);
            if Memory_Grants.Is_Active (value.lifecycle) then
                Spinlocks.exitCriticalSection (grantLock);
                return False;
            end if;
            exit when slot = GrantID'Last;
            slot := slot + 1;
        end loop;
        slot := GrantID'First;
        loop
            Grant_Storage.Next_Present (Grant_Stores (pid), slot, slot, present);
            exit when not present;
            value := grantFor (pid, slot);
            value.all := (generation => Memory_Grants.Life_Base (life), others => <>);
            exit when slot = GrantID'Last;
            slot := slot + 1;
        end loop;
        Grant_Lives (pid) := life;
        Grant_Life_Ready (pid) := True;
        Spinlocks.exitCriticalSection (grantLock);
        return True;
    end initializeGrantLife;

    procedure reserveGrantRecord
      (owner : ProcessID; slot : out GrantID; value : out Grant_Pointer)
    is
        result : Grant_Storage.Allocation_Result;
    begin
        slot := GrantID'First;
        value := null;
        if not Grant_Life_Ready (owner) then return; end if;
        for candidate in GrantID loop
            value := grantFor (owner, candidate);
            if value = null then
                Grant_Storage.Ensure
                  (Grant_Stores (owner), candidate,
                   (generation => Memory_Grants.Life_Base (Grant_Lives (owner)), others => <>),
                   Grant_Storage.Maximum_Blocks, value, result);
                if value = null then return; end if;
            end if;
            if not Memory_Grants.Is_Active (value.lifecycle) and then value.reusable then
                slot := candidate;
                return;
            end if;
        end loop;
        value := null;
    end reserveGrantRecord;

    function hasActiveGrants (owner : ProcessID) return Boolean is
        slot : GrantID := GrantID'First;
        present : Boolean;
    begin
        loop
            Grant_Storage.Next_Present (Grant_Stores (owner), slot, slot, present);
            exit when not present;
            if Memory_Grants.Is_Active (grantFor (owner, slot).lifecycle) then
                return True;
            end if;
            exit when slot = GrantID'Last;
            slot := slot + 1;
        end loop;
        return False;
    end hasActiveGrants;

    package Scope_Storage is new Retained_Record_Blocks
      (Grant_Loans.State, Memory_Grants.Global_Slot'Last, Scopes_Per_Block,
       allocateGrantMetadataBlock);
    subtype Scope_Pointer is Scope_Storage.Element_Access;
    use type Scope_Pointer;
    Scopes : Scope_Storage.Store;

    -- All callers hold grantLock. A missing scope is normal for ordinary
    -- grants. A live parent link, conversely, always has a published scope.
    function scopeFor (slot : Memory_Grants.Global_Slot) return Scope_Pointer is
    begin
        return Scope_Storage.Find (Scopes, slot);
    end scopeFor;

    function ensureScope (slot : Memory_Grants.Global_Slot) return Scope_Pointer is
        value : Scope_Pointer;
        result : Scope_Storage.Allocation_Result;
    begin
        -- All failure results return null before the caller takes a parent
        -- hold or installs a child mapping. Existing scopes remain untouched.
        Scope_Storage.Ensure
          (Scopes, slot, Empty_Forwarding_Scope, Scope_Storage.Maximum_Blocks,
           value, result);
        return value;
    end ensureScope;
    type Parent_Link is record
        active : Boolean := False;
        parent : Memory_Grants.Reference := (0, Memory_Grants.Initial_Generation);
        loan : Grant_Loans.Loan_Reference := Grant_Loans.No_Loan;
    end record;
    Empty_Parent_Link : constant Parent_Link := (others => <>);
    Links_Per_Block : constant Positive :=
      4096 / (Parent_Link'Object_Size / System.Storage_Unit);
    package Link_Storage is new Retained_Record_Blocks
      (Parent_Link, Memory_Grants.Global_Slot'Last, Links_Per_Block,
       allocateGrantMetadataBlock);
    use type Link_Storage.Element_Access;
    Links : Link_Storage.Store;

    -- Like scopeFor, only called under grantLock. Missing storage denotes
    -- an ordinary grant, not an error and never an implicit allocation.
    function linkFor (slot : Memory_Grants.Global_Slot) return Parent_Link is
        value : constant Link_Storage.Element_Access :=
          Link_Storage.Find (Links, slot);
    begin
        if value = null then return Empty_Parent_Link; end if;
        return value.all;
    end linkFor;

    function ensureLink (slot : Memory_Grants.Global_Slot)
      return Link_Storage.Element_Access
    is
        value : Link_Storage.Element_Access;
        result : Link_Storage.Allocation_Result;
    begin
        Link_Storage.Ensure
          (Links, slot, Empty_Parent_Link, Link_Storage.Maximum_Blocks,
           value, result);
        return value;
    end ensureLink;

    function grantReference (value : Grant) return Memory_Grants.Reference is
      ((value.globalSlot, value.generation));

    procedure invalidateGrant (value : in out Grant)

    is
        nextGeneration : Memory_Grants.Live_Grant_Generation :=
          value.generation;
        mayReuse : Boolean;
        scope : constant Scope_Pointer := scopeFor (value.globalSlot);
    begin
        if linkFor(value.globalSlot).active or else
           (scope /= null and then Grant_Loans.Holds_Parent (scope.all))
        then
            raise ProcessException with "Grant invalidated before child retirement";
        end if;
        if scope /= null then scope.all := Empty_Forwarding_Scope; end if;
        -- Stay within this life's generation range (its high half); at the
        -- ceiling the slot retires for this life instead of stepping into the
        -- next process's range.
        Memory_Grants.Advance_Generation_Within
          (nextGeneration, Memory_Grants.Ceiling_Of (value.generation), mayReuse);
        value :=
          (globalSlot  => 0,
           lifecycle   => Memory_Grants.Inactive_Lifecycle,
           reusable    => mayReuse,
           generation  => nextGeneration,
           granterPID  => NO_PROCESS,
           granteePID  => NO_PROCESS,
           granterAddr => System.Null_Address,
           granteeAddr => System.Null_Address,
           numPages    => 0,
           permission  => GRANT_READ,
           forwardable => False);
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
                           expectedGeneration : Capabilities.Generation := 0;
                           forwardable : Boolean := False)
    is
        pid : constant ProcessID := PerCPUData.getCurrentPID;
        owner : constant ProcessID :=
          pid;
        receiver : ProcessID;
        flags : Unsigned_64;
        ok : Boolean;
        slot : GrantID := 0;
        target : Grant_Pointer;
        globalId : Natural;
        staging : Grant;
        procedure mapPageInst is new Virtmem.mapPage (BuddyAllocator.allocFrame);

        procedure resolveAndPin
          (page : Natural; physical : out Virtmem.PhysAddress; success : out Boolean)
        is
        begin
            -- Lookup and ownership-checked pin share the source address-space
            -- lock. Release it before the receiver lock, including self-grants.
            lockAddressSpace (owner);
            physical := Virtmem.tableWalk
              (To_Integer (localAddr) +
                 Integer_Address (page) * Virtmem.PAGE_SIZE,
               addrtab(proctab(owner).pgTable), Allow_Big => True);
            -- Resolve a 4 KiB constituent of an owned 2 MiB leaf. The pin is
            -- still per frame, and installPage creates a 4 KiB recipient leaf.
            -- Retirement must never unmap the owner's large leaf.
            success := False;
            if physical /= 0 then
                BuddyAllocator.pinOwnedFrame
                  (physical, Unsigned_8 (owner), success);
            end if;
            unlockAddressSpace (owner);
        end resolveAndPin;

        procedure installPage
          (page : Natural; physical : Virtmem.PhysAddress; success : out Boolean)
        is
        begin
            lockAddressSpace (receiver);
            mapPageInst
              (physical,
               To_Integer (staging.granteeAddr) +
                 Integer_Address (page) * Virtmem.PAGE_SIZE,
               flags, addrtab(proctab(receiver).pgTable), success);
            unlockAddressSpace (receiver);
        end installPage;

        procedure releaseUnpublished (physical : Virtmem.PhysAddress) is
            released : Boolean;
        begin
            BuddyAllocator.unpinFrame (physical, released);
            if not released then
                raise ProcessException with "Unpublished grant pin lost";
            end if;
        end releaseUnpublished;

        procedure retirePrefix (pages : Positive) is
        begin
            staging.numPages := pages;
            unmapGrantPages (staging);
            staging.numPages := 0;
        end retirePrefix;

        procedure installPages is new Grant_Page_Installation
          (Virtmem.PhysAddress, resolveAndPin, installPage,
           releaseUnpublished, retirePrefix);
        installed : Natural;
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
        receiver := grantee;
        if not proctab(owner).admitted or else
           not proctab(grantee).admitted or else
           Process_Lifetime.Closing (threadOf (owner).lifetime) or else
           Process_Lifetime.Closing (threadOf (grantee).lifetime) or else
           (expectedGeneration /= 0 and then
            expectedGeneration /= generationOf (grantee))
        then
            Spinlocks.exitCriticalSection (grantLock);
            return;
        end if;

        reserveGrantRecord (owner, slot, target);
        if target = null then
            Spinlocks.exitCriticalSection (grantLock);
            return;
        end if;
        globalId := Natural (owner) * MAX_GRANTS_PER_PROCESS + slot;
        flags := (if perm = GRANT_READWRITE then Virtmem.PG_USERDATA
                  else Virtmem.PG_USERDATARO);
        staging := (globalSlot => globalId,
                    granterPID => owner, granteePID => receiver,
                    granterAddr => localAddr,
                    granteeAddr => To_Address (GRANT_REGION_BASE +
                        Integer_Address (globalId) * GRANT_SLOT_SIZE),
                    permission => perm, forwardable => forwardable, others => <>);

        installPages (numPages, installed, ok);
        if not ok then
            Spinlocks.exitCriticalSection (grantLock);
            return;
        end if;
        staging.numPages := installed;

        staging.lifecycle := Memory_Grants.Available_Lifecycle;
        staging.generation := target.generation;
        target.all := staging;
        id := globalId;
        success := True;
        Spinlocks.exitCriticalSection (grantLock);
    end createGrant;

    procedure deriveGrant
      (parent : Memory_Grants.Reference; grantee : ProcessID;
       expectedGeneration : Capabilities.Generation;
       pageOffset : Memory_Grants.Page_Offset;
       numPages : Memory_Grants.Page_Count; perm : GrantPermission;
       derived : out Memory_Grants.Reference; success : out Boolean)
    is
        caller : constant ProcessID := PerCPUData.getCurrentPID;
        rootOwner : constant ProcessID := ProcessID (Memory_Grants.Owner_Of (parent.slot));
        source : Grant_Pointer;
        scope : Scope_Pointer;
        requested : constant Grant_Loans.Terms :=
          (pageOffset, numPages, (if perm = GRANT_READWRITE then
           Memory_Grants.Borrowed_Read_Write else Memory_Grants.Borrowed_Read_Only));
        loan : Grant_Loans.Loan_Reference;
        reservation : Grant_Loans.Reservation_Result;
        applied, mapped : Boolean;
        target : Grant_Pointer;
        childLocal : GrantID := 0;
        childSlot : Memory_Grants.Global_Slot;
        staging : Grant;
        childLink : Link_Storage.Element_Access;
        installed : Natural;
        flags : constant Unsigned_64 := (if perm = GRANT_READWRITE then
          Virtmem.PG_USERDATA else Virtmem.PG_USERDATARO);
        procedure mapPageInst is new Virtmem.mapPage (BuddyAllocator.allocFrame);

        procedure resolveAndPin
          (page : Natural; physical : out Virtmem.PhysAddress; success : out Boolean)
        is
        begin
            lockAddressSpace (caller);
            physical := Virtmem.tableWalk
              (To_Integer (source.granteeAddr) +
               Integer_Address (pageOffset + page) * Virtmem.PAGE_SIZE,
               addrtab(proctab(caller).pgTable));
            success := False;
            if physical /= 0 then
                -- New mapping owns a NEW pin, checked against the actual
                -- root owner, never re-labeling borrowed pages as caller-owned.
                BuddyAllocator.pinOwnedFrame
                  (physical, Unsigned_8 (rootOwner), success);
            end if;
            unlockAddressSpace (caller);
        end resolveAndPin;

        procedure installPage
          (page : Natural; physical : Virtmem.PhysAddress; success : out Boolean)
        is
        begin
            lockAddressSpace (grantee);
            mapPageInst (physical, To_Integer (staging.granteeAddr) +
              Integer_Address (page) * Virtmem.PAGE_SIZE, flags,
              addrtab(proctab(grantee).pgTable), success);
            unlockAddressSpace (grantee);
        end installPage;

        procedure releaseUnpublished (physical : Virtmem.PhysAddress) is
            released : Boolean;
        begin
            BuddyAllocator.unpinFrame (physical, released);
            if not released then
                raise ProcessException with "Unpublished child pin lost";
            end if;
        end releaseUnpublished;

        procedure retirePrefix (pages : Positive) is
        begin
            staging.numPages := pages;
            unmapGrantPages (staging);
            staging.numPages := 0;
        end retirePrefix;

        procedure installPages is new Grant_Page_Installation
          (Virtmem.PhysAddress, resolveAndPin, installPage,
           releaseUnpublished, retirePrefix);
    begin
        derived := (0, Memory_Grants.Initial_Generation);
        success := False;
        if rootOwner = NO_PROCESS or else grantee = NO_PROCESS or else
           expectedGeneration = 0
        then
            return;
        end if;
        Spinlocks.enterCriticalSection (grantLock);
        source := grantFor (parent.slot);
        if source = null or else not proctab(caller).admitted or else not proctab(rootOwner).admitted or else
           not proctab(grantee).admitted or else
           Process_Lifetime.Closing (threadOf (caller).lifetime) or else
           Process_Lifetime.Closing (threadOf (rootOwner).lifetime) or else
           Process_Lifetime.Closing (threadOf (grantee).lifetime) or else
           expectedGeneration /= generationOf (grantee) or else
           not Memory_Grants.Is_Current (parent, source.generation) or else
           not Memory_Grants.Is_Available (source.lifecycle) or else
           Memory_Grants.Acquisition_Total (source.lifecycle) = 0 or else
           source.granteePID /= caller or else source.granterPID /= rootOwner or else
           not source.forwardable or else linkFor(parent.slot).active or else
           source.numPages = 0 or else
           not Memory_Grants.Range_Attenuates
             (Memory_Grants.Page_Count (source.numPages), pageOffset, numPages) or else
           (perm = GRANT_READWRITE and then source.permission /= GRANT_READWRITE)
        then
            Spinlocks.exitCriticalSection (grantLock);
            return;
        end if;
        reserveGrantRecord (caller, childLocal, target);
        if target = null then
            Spinlocks.exitCriticalSection (grantLock);
            return;
        end if;
        childSlot := Memory_Grants.Make_Global_Slot
          (Memory_Grants.Process_Index (caller), childLocal);
        childLink := ensureLink (childSlot);
        if childLink = null then
            Spinlocks.exitCriticalSection (grantLock);
            return;
        end if;
        scope := ensureScope (parent.slot);
        if scope = null then
            Spinlocks.exitCriticalSection (grantLock);
            return;
        end if;
        if Grant_Loans.Phase (scope.all) = Grant_Loans.Unconfigured then
            Grant_Loans.Open_Forwarding (scope.all, source.lifecycle, parent,
              Memory_Grants.Page_Count (source.numPages),
              (if source.permission = GRANT_READWRITE then Memory_Grants.Borrowed_Read_Write
               else Memory_Grants.Borrowed_Read_Only), Grant_Loans.Forward_Once, applied);
            if not applied then
                Spinlocks.exitCriticalSection (grantLock);
                return;
            end if;
        end if;
        Grant_Loans.Reserve (scope.all, requested, loan, reservation);
        if reservation /= Grant_Loans.Reserved then
            Spinlocks.exitCriticalSection (grantLock);
            return;
        end if;
        staging := (globalSlot => childSlot, granterPID => caller,
          granteePID => grantee, permission => perm, forwardable => False,
          granterAddr => To_Address (To_Integer (source.granteeAddr) +
            Integer_Address (pageOffset) * Virtmem.PAGE_SIZE),
          granteeAddr => To_Address (GRANT_REGION_BASE +
            Integer_Address (childSlot) * GRANT_SLOT_SIZE), others => <>);
        installPages (numPages, installed, mapped);
        if not mapped then
            -- Transaction has already removed its partial mappings and waited
            -- for shootdown. Only now can the reserved loan retire.
            Grant_Loans.Revoke (scope.all, loan, applied);
            if not applied then
                raise ProcessException with "Child rollback lost reservation";
            end if;
            Grant_Loans.Finish_Retirement (scope.all, loan, applied);
            if not applied then
                raise ProcessException with "Child rollback retirement failed";
            end if;
            Spinlocks.exitCriticalSection (grantLock);
            return;
        end if;
        staging.numPages := installed;
        staging.generation := target.generation;
        staging.lifecycle := Memory_Grants.Available_Lifecycle;
        Grant_Loans.Publish (scope.all, loan, applied);
        if not applied then
            raise ProcessException with "Child publication lost reservation";
        end if;
        childLink.all := (True, parent, loan);
        target.all := staging;
        derived := grantReference (staging);
        success := True;
        Spinlocks.exitCriticalSection (grantLock);
    end deriveGrant;

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
        lockAddressSpace (g.granteePID);
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
        unlockAddressSpace (g.granteePID);
        TLB_Shootdown.Invalidate_All;
        -- No pin release (and hence no allocator reuse) before completion.
        for page in 0 .. g.numPages - 1 loop
            BuddyAllocator.unpinFrame (Retiring_Frames (page), ok);
            if not ok then
                raise ProcessException with "Retired grant lost its lifetime pin";
            end if;
        end loop;
    end unmapGrantPages;

    procedure completeOwnerPIDIfReady (owner : ProcessID);
    procedure releaseForwardingIfReady (reference : Memory_Grants.Reference);

    procedure retireGrantLocked (slot : Memory_Grants.Global_Slot) is
        owner : constant ProcessID := ProcessID (Memory_Grants.Owner_Of (slot));
        value : constant Grant_Pointer := grantFor (slot);
        link : constant Parent_Link := linkFor(slot);
        applied : Boolean;
    begin
        if value = null then return; end if;
        unmapGrantPages (value.all);
        if link.active then
            Grant_Loans.Finish_Retirement
              (scopeFor (link.parent.slot).all, link.loan, applied);
            if not applied then
                raise ProcessException with "Child retired in invalid loan phase";
            end if;
            Link_Storage.Find (Links, slot).all := Empty_Parent_Link;
        end if;
        invalidateGrant (value.all);
        if link.active then
            releaseForwardingIfReady (link.parent);
        end if;
        completeOwnerPIDIfReady (owner);
    end retireGrantLocked;

    procedure releaseForwardingIfReady (reference : Memory_Grants.Reference) is
        value : constant Grant_Pointer := grantFor (reference.slot);
        scope : constant Scope_Pointer := scopeFor (reference.slot);
        applied : Boolean;
        result : Memory_Grants.Hold_Release_Result;
    begin
        if value = null or else scope = null or else value.generation /= reference.generation or else
           Grant_Loans.Phase (scope.all) /= Grant_Loans.Closing or else
           not Grant_Loans.Empty (scope.all)
        then
            return;
        end if;
        Grant_Loans.Release_Forwarding
          (scope.all, value.lifecycle, reference, applied, result);
        if not applied then
            raise ProcessException with "Forwarding scope lost its parent hold";
        end if;
        if result = Memory_Grants.Revocation_Completed_On_Hold_Release then
            retireGrantLocked (reference.slot);
        end if;
    end releaseForwardingIfReady;

    procedure revokeGrantLocked (slot : Memory_Grants.Global_Slot);

    procedure closeForwardingLocked (slot : Memory_Grants.Global_Slot) is
        value : constant Grant_Pointer := grantFor (slot);
        reference : Memory_Grants.Reference;
        receiver : ProcessID;
        applied : Boolean;
        childSlot : Memory_Grants.Global_Slot;
        scope : constant Scope_Pointer := scopeFor (slot);
        local : GrantID := GrantID'First;
        present : Boolean;
    begin
        if value = null or else scope = null or else Grant_Loans.Phase (scope.all) /= Grant_Loans.Accepting then
            return;
        end if;
        reference := grantReference (value.all);
        receiver := value.granteePID;
        Grant_Loans.Close_Forwarding (scope.all, reference, applied);
        if not applied then
            raise ProcessException with "Forwarding close rejected current identity";
        end if;
        -- Children are terminal and owned by the original receiver. Its PID
        -- cannot recycle while any owned grant still retains its backing.
        loop
            Grant_Storage.Next_Present (Grant_Stores (receiver), local, local, present);
            exit when not present;
            childSlot := Memory_Grants.Make_Global_Slot
              (Memory_Grants.Process_Index (receiver), local);
            if linkFor(childSlot).active and then
               linkFor(childSlot).parent = reference
            then
                revokeGrantLocked (childSlot);
            end if;
            exit when local = GrantID'Last;
            local := local + 1;
        end loop;
        -- Last child retirement may already have released/inactivated parent.
        releaseForwardingIfReady (reference);
    end closeForwardingLocked;

    procedure revokeGrantLocked (slot : Memory_Grants.Global_Slot)

    is
        g : constant Grant_Pointer := grantFor (slot);
        result : Memory_Grants.Revocation_Result;
        applied : Boolean;
        link : constant Parent_Link := linkFor(slot);
    begin
        if g = null or else not Memory_Grants.Is_Active (g.lifecycle) then
            return;
        end if;

        if link.active and then Grant_Loans.Phase_Of
          (scopeFor (link.parent.slot).all, link.loan) = Grant_Loans.Available
        then
            Grant_Loans.Revoke
              (scopeFor (link.parent.slot).all, link.loan, applied);
            if not applied then
                raise ProcessException with "Child revocation lost parent scope";
            end if;
        end if;
        Memory_Grants.Request_Revocation (g.lifecycle, result);
        if result = Memory_Grants.Revocation_Pending then
            closeForwardingLocked (slot);
            return;
        end if;

        retireGrantLocked (slot);
    end revokeGrantLocked;

    ---------------------------------------------------------------------------
    -- revokeGrant
    ---------------------------------------------------------------------------
    procedure revokeGrant (id : GrantID; success : out Boolean)

    is
        pid   : constant ProcessID := PerCPUData.getCurrentPID;
        owner : constant ProcessID :=
            pid;
    begin
        success := False;
        Spinlocks.enterCriticalSection (grantLock);
        if owner = NO_PROCESS or else grantFor (owner, id) = null or else
          not Memory_Grants.Is_Active (grantFor (owner, id).lifecycle)
        then
            Spinlocks.exitCriticalSection (grantLock);
            return;
        end if;
        revokeGrantLocked (Memory_Grants.Make_Global_Slot
          (Memory_Grants.Process_Index (owner), id));
        success := True;
        Spinlocks.exitCriticalSection (grantLock);
    end revokeGrant;

    ---------------------------------------------------------------------------
    -- revokeAllGrants
    -- Revoke all active grants owned by the specified process.
    -- Called during process kill().
    ---------------------------------------------------------------------------
    procedure revokeAllGrants (pid : ProcessID)

    is
        slot : GrantID := GrantID'First;
        present : Boolean;
    begin
        Spinlocks.enterCriticalSection (grantLock);
        loop
            Grant_Storage.Next_Present (Grant_Stores (pid), slot, slot, present);
            exit when not present;
            revokeGrantLocked (Memory_Grants.Make_Global_Slot
              (Memory_Grants.Process_Index (pid), slot));
            exit when slot = GrantID'Last;
            slot := slot + 1;
        end loop;
        Spinlocks.exitCriticalSection (grantLock);
    end revokeAllGrants;

    ---------------------------------------------------------------------------
    -- revokeAllGrantsTo
    -- Teardown must retire received mappings while the page tables exist.
    -- INVALID is a scheduler state, not proof of remote TLB quiescence.
    -- Receiver close stops its acquisitions. Retire its mappings while the
    -- page tables exist, but do not discard a kernel-owned forwarding hold.
    -- Every downstream mapping owns independent frame pins. A dead forwarding
    -- receiver closes its children before its own page tables can be destroyed.
    ---------------------------------------------------------------------------
    procedure revokeAllGrantsTo (pid : ProcessID)

    is
        slot : GrantID;
        present : Boolean;
    begin
        Spinlocks.enterCriticalSection (grantLock);
        for owner in ProcessID range ProcessID'First + 1 .. ProcessID'Last loop
            slot := GrantID'First;
            loop
                Grant_Storage.Next_Present (Grant_Stores (owner), slot, slot, present);
                exit when not present;
                if grantFor (owner, slot) /= null and then Memory_Grants.Is_Active
                  (grantFor (owner, slot).lifecycle) and then
                   grantFor (owner, slot).granteePID = pid
                then
                    declare
                        hadAcquisitions : Boolean;
                        value : constant Grant_Pointer := grantFor (owner, slot);
                        reference : constant Memory_Grants.Reference :=
                          grantReference (value.all);
                        link : constant Parent_Link := linkFor(reference.slot);
                        applied : Boolean;
                    begin
                        if link.active then
                            if Grant_Loans.Phase_Of
                              (scopeFor (link.parent.slot).all, link.loan) =
                                Grant_Loans.Available
                            then
                                Grant_Loans.Revoke
                                  (scopeFor (link.parent.slot).all, link.loan, applied);
                                if not applied then
                                    raise ProcessException with "Dead child revocation failed";
                                end if;
                            end if;
                            -- Kernel-confirmed receiver death ends its CPU
                            -- readers; this is never exposed as a user request.
                            for reader in 1 .. Grant_Loans.Readers
                              (scopeFor (link.parent.slot).all, link.loan)
                            loop
                                Grant_Loans.Return_Reader
                                  (scopeFor (link.parent.slot).all, link.loan, applied);
                                if not applied then
                                    raise ProcessException with "Dead child reader lost";
                                end if;
                            end loop;
                        end if;
                        Memory_Grants.Close_Receiver
                          (value.lifecycle, hadAcquisitions);
                        closeForwardingLocked (reference.slot);
                        -- Child closure may already have retired this parent.
                        if value.generation = reference.generation and then
                           value.granterPID = owner and then
                           value.globalSlot = reference.slot
                        then
                            if Memory_Grants.Is_Active (value.lifecycle) then
                                unmapGrantPages (value.all);
                                value.numPages := 0;
                            else
                                retireGrantLocked (reference.slot);
                            end if;
                        end if;
                    end;
                end if;
                exit when slot = GrantID'Last;
                slot := slot + 1;
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
          pid;
        slotOwner : constant ProcessID := ProcessID
          (Memory_Grants.Owner_Of (slot));
        localSlot : constant GrantID := GrantID
          (Memory_Grants.Local_Slot_Of (slot));
        value : Grant_Pointer;
    begin
        generation := 0;
        success := False;

        Spinlocks.enterCriticalSection (grantLock);

        if slotOwner /= owner then
            Spinlocks.exitCriticalSection (grantLock);
            return;
        end if;

        value := grantFor (slotOwner, localSlot);
        if value = null or else not Memory_Grants.Is_Active (value.lifecycle) then
            --  Zero is an owned, retired slot, not a lookup failure. All
            --  invalidation paths retire mappings/TLBs before marking inactive.
            success := True;
        elsif value.granterPID = owner then
            generation := value.generation;
            success := True;
        end if;
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
          pid;
        slotOwner : constant ProcessID := ProcessID
          (Memory_Grants.Owner_Of (reference.slot));
        localSlot : constant GrantID := GrantID
          (Memory_Grants.Local_Slot_Of (reference.slot));
        value : Grant_Pointer;
        mappedBytes : Unsigned_64;
        applied : Boolean;
        link : Parent_Link;
    begin
        mappedAddress := System.Null_Address;
        success := False;

        Spinlocks.enterCriticalSection (grantLock);
        link := linkFor (reference.slot);
        value := grantFor (slotOwner, localSlot);

        if value = null or else expectedOwner = NO_PROCESS or else slotOwner /= expectedOwner or else
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
        if link.active then
            Grant_Loans.Acquire
              (scopeFor (link.parent.slot).all, link.loan, applied);
            if not applied then
                Spinlocks.exitCriticalSection (grantLock);
                return;
            end if;
        end if;
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
                    if not allocation.retainUntilReboot then
                        BuddyAllocator.free
                          (allocation.order, Virtmem.P2Va (allocation.physAddr));
                    end if;
                    -- Retained blocks remain allocated in the buddy map.
                    -- Clear process ownership above before PID reuse, but do
                    -- not recycle backing possibly still reachable by DMA.
                    allocation.active := False;
                end;
            end if;
        end loop;
    end releaseDMAAllocations;

    procedure completeOwnerPIDIfReady (owner : ProcessID)

    is
    begin
        if not proctab(owner).grantTeardownPending or else
           not proctab(owner).grantTeardownReady
        then
            return;
        end if;

        if not hasActiveGrants (owner) then
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
                    PIDTracker.freePID (owner, invalidated => True);
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
          pid;
        slotOwner : constant ProcessID := ProcessID
          (Memory_Grants.Owner_Of (reference.slot));
        localSlot : constant GrantID := GrantID
          (Memory_Grants.Local_Slot_Of (reference.slot));
        value : Grant_Pointer;
        result : Memory_Grants.Return_Result;
        applied : Boolean;
        link : Parent_Link;
    begin
        success := False;
        Spinlocks.enterCriticalSection (grantLock);

        link := linkFor (reference.slot);
        value := grantFor (slotOwner, localSlot);
        if value = null or else not Memory_Grants.Is_Active (value.lifecycle) or else
           value.granteePID /= receiver or else
           not Memory_Grants.Is_Current (reference, value.generation) or else
           Memory_Grants.Acquisition_Total (value.lifecycle) = 0
        then
            Spinlocks.exitCriticalSection (grantLock);
            return;
        end if;

        if link.active then
            Grant_Loans.Return_Reader
              (scopeFor (link.parent.slot).all, link.loan, applied);
            if not applied then
                raise ProcessException with "Child return lost parent reader";
            end if;
        end if;
        Memory_Grants.Record_Return (value.lifecycle, result);
        if Memory_Grants.Acquisition_Total (value.lifecycle) = 0 then
            if result = Memory_Grants.Revocation_Completed_On_Return then
                retireGrantLocked (reference.slot);
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
          pid;
        slotOwner : constant ProcessID := ProcessID
          (Memory_Grants.Owner_Of (reference.slot));
        localSlot : constant GrantID := GrantID
          (Memory_Grants.Local_Slot_Of (reference.slot));
        value : Grant_Pointer;
    begin
        success := False;
        Spinlocks.enterCriticalSection (grantLock);
        value := grantFor (slotOwner, localSlot);
        if value = null or else slotOwner /= owner or else
           not Memory_Grants.Is_Active (value.lifecycle) or else
           value.granterPID /= owner or else
           not Memory_Grants.Is_Current (reference, value.generation)
        then
            Spinlocks.exitCriticalSection (grantLock);
            return;
        end if;

        revokeGrantLocked (reference.slot);
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
        deferred := hasActiveGrants (pid);

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
    -- Resolve an endpoint slot of the calling process under its mailbox
    -- lock, which serializes every edit of the capability table: a sibling
    -- thread cannot change the slot between the checks and the generation
    -- the send then pins.
    procedure resolveEndpointSlot
      (pid          : ProcessID;
       capSlot      : Capabilities.CapabilitySlot;
       candidatePID : out ProcessID;
       authorityTag : out Capabilities.Authority_Tag;
       generation   : out Capabilities.Generation;
       ok           : out Boolean)
    is
        destPID : Unsigned_64;
        status  : Capabilities.Operations.OperationStatus;
    begin
        candidatePID := NO_PROCESS;
        authorityTag := Capabilities.NO_AUTHORITY_TAG;
        generation := 0;
        ok := False;
        Spinlocks.enterCriticalSection (mailtab(pid).lock);
        -- Validate the generic object reference before narrowing it to an
        -- index into proctab, whose first valid process entry is 1.
        destPID := proctab(pid).caps(capSlot).object.ref;
        if destPID >= Unsigned_64(ProctabRange'First) and then
           destPID <= Unsigned_64(ProctabRange'Last)
        then
            candidatePID := ProcessID(destPID);
            Capabilities.Operations.resolveCurrentEndpoint
              (table             => proctab(pid).caps,
               slot              => capSlot,
               rights            => Capabilities.READ_WRITE,
               currentGeneration => generationOf (candidatePID),
               destPID           => destPID,
               authorityTag      => authorityTag,
               status            => status);
            ok := status = Capabilities.Operations.OP_OK;
            generation := proctab(pid).caps(capSlot).gen;
        end if;
        Spinlocks.exitCriticalSection (mailtab(pid).lock);
    end resolveEndpointSlot;

    function capSend (capSlot : Capabilities.CapabilitySlot;
                      msg     : Message) return MessageTag
    is
        candidatePID : ProcessID;
        authorityTag : Capabilities.Authority_Tag;
        generation   : Capabilities.Generation;
        ok           : Boolean;
        stamped      : Message := msg;
    begin
        resolveEndpointSlot (PerCPUData.getCurrentPID, capSlot, candidatePID,
                             authorityTag, generation, ok);
        if not ok then
            return NULL_TAG;
        end if;
        stamped.authorityTag := authorityTag;
        return send (dest => candidatePID, msg => stamped,
                     expectedGeneration => generation);
    end capSend;

    ---------------------------------------------------------------------------
    -- capCall
    ---------------------------------------------------------------------------
    function capCall (capSlot : Capabilities.CapabilitySlot;
                      msg     : Message) return MessageTag
    is
    begin
        return capSend (capSlot, msg);
    end capCall;

    ---------------------------------------------------------------------------
    -- capSubmit
    ---------------------------------------------------------------------------
    function capSubmit (capSlot : Capabilities.CapabilitySlot;
                        msg     : Message;
                        token   : Unsigned_64) return Boolean
    is
        candidatePID : ProcessID;
        authorityTag : Capabilities.Authority_Tag;
        generation   : Capabilities.Generation;
        ok           : Boolean;
        stamped      : Message := msg;
    begin
        resolveEndpointSlot (PerCPUData.getCurrentPID, capSlot, candidatePID,
                             authorityTag, generation, ok);
        if not ok then
            return False;
        end if;
        stamped.authorityTag := authorityTag;
        return submitResolvedEndpoint (dest  => candidatePID,
                       msg   => stamped,
                       token => token,
                       expectedGeneration => generation);
    end capSubmit;


    -- Caller holds Process.lock. Wake the thread a synchronous reply
    -- capability answers, if it is still waiting: its server is dying.
    procedure failReplyWaiter (cap : Capabilities.Capability) is
        waiterPID : ProcessID;
        waiter    : ThreadID;
    begin
        if cap.capType = Capabilities.CAP_REPLY and then
           cap.object.param = NO_REQUEST_ID
        then
            replyTargetOf (cap, waiterPID, waiter);
            if waiter /= NO_THREAD and then
               threadtab (waiter).state = WAITINGFORREPLY
            then
                threadtab (waiter).replyMsg := NULL_MESSAGE;
                ready (waiter);
            end if;
        end if;
    end failReplyWaiter;

    procedure retireThread (tid : ThreadID) is
        pid : constant ProcessID := processOf (tid);
    begin
        Spinlocks.enterCriticalSection (mailtab(pid).lock);
        Spinlocks.enterCriticalSection (lock);
        failReplyWaiter (threadtab (tid).replyCap);
        threadtab (tid).replyCap := Capabilities.NULL_CAPABILITY;
        Spinlocks.exitCriticalSection (lock);
        -- Its outstanding requests are cancelled, never handed to a
        -- sibling: a later reply finds no pending request and fails.
        declare
            kept : Natural := 0;
            item : CompletionEntry;
            found : Boolean;
        begin
            for r in 0 .. proctab(pid).numPending - 1 loop
                if proctab(pid).pendingRequests(r).thread /= tid then
                    proctab(pid).pendingRequests(kept) := proctab(pid).pendingRequests(r);
                    kept := kept + 1;
                end if;
            end loop;
            for r in kept .. MAX_PENDING_ASYNC - 1 loop
                proctab(pid).pendingRequests(r) := NO_PENDING;
            end loop;
            proctab(pid).numPending := kept;
            loop
                dequeueCompletion (pid, tid, item, found, strict => True);
                exit when not found;
            end loop;
        end;
        Spinlocks.exitCriticalSection (mailtab(pid).lock);
    end retireThread;

    procedure retireMailboxes (pid : ProcessID) is
        t : ThreadID;
    begin
        for p in ProctabRange loop
            Spinlocks.enterCriticalSection (mailtab(p).lock);
            Spinlocks.enterCriticalSection (lock);
            if p = pid or else proctab(p).admitted then
                t := mainThreadOf (pid);
                while t /= NO_THREAD loop
                    Queues.detach (mailtab(p).sendQueue, t);
                    Queues.detach (mailtab(p).recvQueue, t);
                    t := threadtab (t).nextSibling;
                end loop;
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
                    stuck : ThreadID;
                begin
                    --  Drain sendQueue: threads waiting to SEND to dying process
                    loop
                        exit when Queues.isEmpty (mailtab(pid).sendQueue);
                        Queues.dequeue (mailtab(pid).sendQueue, stuck);
                        threadtab (stuck).replyMsg := NULL_MESSAGE;
                        ready (stuck);
                    end loop;

                    --  Drain recvQueue: threads waiting to RECEIVE from dying process
                    loop
                        exit when Queues.isEmpty (mailtab(pid).recvQueue);
                        Queues.dequeue (mailtab(pid).recvQueue, stuck);
                        threadtab (stuck).replyMsg := NULL_MESSAGE;
                        ready (stuck);
                    end loop;

                    --  The dying process's own event/completion waiters.
                    loop
                        exit when Queues.isEmpty (mailtab(pid).notifyQueue);
                        Queues.dequeue (mailtab(pid).notifyQueue, stuck);
                    end loop;

                    --  Wake senders in the ring that are WAITINGFORREPLY
                    --  (from send() Path 1, never dequeued by receive())
                    drainRingSenders : declare
                        r   : MessageRing renames mailtab(pid).ring;
                        idx : RingIndex;
                        s   : ThreadID;
                    begin
                        for i in 0 .. r.count - 1 loop
                            idx := (r.tail + i) mod RING_SIZE;
                            s   := r.entries(idx).senderThread;
                            if r.entries(idx).kind = RING_SYNC and then s /= NO_THREAD
                               and then threadtab (s).state = WAITINGFORREPLY
                            then
                                threadtab (s).replyMsg := NULL_MESSAGE;
                                ready (s);
                            end if;
                        end loop;
                    end drainRingSenders;
                end drainMailQueues;

                --  Wake threads waiting for a reply from the dying process:
                --  its threads' current and deferred reply capabilities.
                wakeWaiters : declare
                    w : ThreadID := mainThreadOf (pid);
                begin
                    for slot in Capabilities.CapabilitySlot loop
                        failReplyWaiter (proctab(pid).caps(slot));
                    end loop;
                    while w /= NO_THREAD loop
                        failReplyWaiter (threadtab (w).replyCap);
                        threadtab (w).replyCap := Capabilities.NULL_CAPABILITY;
                        w := threadtab (w).nextSibling;
                    end loop;
                end wakeWaiters;

                --  Clear stale mailbox state
                mailtab(pid).ring := (others => <>);
                mailtab(pid).nextReceiveLane := Queued_Messages;

                --  Clear completion queue and pending requests
                completionTab(pid) := (ring => (others => NULL_COMPLETION),
                                       head => 0, tail => 0, count => 0,
                                       owners => (others => NO_THREAD));
                proctab(pid).pendingRequests :=
                    (others => NO_PENDING);
                proctab(pid).numPending := 0;
                proctab(pid).requestSequence := IPC_Request_Ids.Initial_Sequence;
                proctab(pid).irqNotificationPending := False;
                t := mainThreadOf (pid);
                while t /= NO_THREAD loop
                    threadtab (t).receiveDeadlineActive := False;
                    threadtab (t).waitsForIPCActivity := False;
                    threadtab (t).receiveDeadlineMs := 0;
                    threadtab (t).receiveDeadlineReceiver := NO_PROCESS;
                    t := threadtab (t).nextSibling;
                end loop;

            else
                if proctab(p).admitted and then threadOf (p).state /= INVALID and then p /= pid then
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
                                     from      => Unsigned_64 (pid),
                                     status    => COMPLETION_TARGET_DIED,
                                     valid     => True,
                                     reserved  => (others => 0));
                                cq.owners(cq.tail) :=
                                    proctab(p).pendingRequests(r).thread;
                                cq.tail := (cq.tail + 1) mod
                                    COMPLETION_QUEUE_SIZE;
                                cq.count := cq.count + 1;
                                wakeActivityWaitersLocked (p);
                                wakeNotifyWaitersLocked (p);
                            end if;
                        end loop;
                        proctab(p).numPending := writeIdx;
                        for r in writeIdx .. MAX_PENDING_ASYNC - 1 loop
                            proctab(p).pendingRequests(r) :=
                                NO_PENDING;
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
           generationOf (dest) = generation
        then
            enqueueRing (dest, (msg => msg, sender => PerCPUData.getCurrentPID,
                         kind => RING_EVENT, requestId => NO_REQUEST_ID,
                         senderThread => NO_THREAD), ok);
            if ok then
                wakeForUnsolicitedWork (dest);
            end if;
        end if;
        Spinlocks.exitCriticalSection (mailtab(dest).lock);
    end sendRetirementEvent;

end Process.IPC;
