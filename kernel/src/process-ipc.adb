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
with PerCPUData;
with Process.Queues;
with Util;
with Virtmem;
with x86;

use type Capabilities.CapabilityType;
use type Capabilities.Operations.OperationStatus;

package body Process.IPC with
    SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- getReceiver
    -- Determine which mailbox to use for receive operations. If the caller
    -- is a thread, use the parent's mailbox.
    ---------------------------------------------------------------------------
    function getReceiver (pid : ProcessID) return ProcessID with
        SPARK_Mode => On
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
        with SPARK_Mode => On
    is
        cq : CompletionQueue renames completionTab(owner);
    begin
        if cq.count >= COMPLETION_QUEUE_SIZE then
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
        with SPARK_Mode => On
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
        with SPARK_Mode => On
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
        with SPARK_Mode => On
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
    -- enqueueRing
    -- Push an entry into a mailbox's unified ring buffer.
    -- Caller must hold mailtab(owner).lock.
    -- @return True if enqueued, False if queue is full (entry dropped).
    ---------------------------------------------------------------------------
    procedure enqueueRing (owner   : in  ProcessID;
                           item    : in  RingEntry;
                           success : out Boolean)
        with SPARK_Mode => On
    is
        r : MessageRing renames mailtab(owner).ring;
    begin
        if r.count >= RING_SIZE then
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
        with SPARK_Mode => On
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
        with SPARK_Mode => On
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
        with SPARK_Mode => On
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

    ---------------------------------------------------------------------------
    -- takeBoundNotification
    -- If the caller has a bound notification with pending bits, synthesize a
    -- receive message and clear the notification word. Caller must hold the
    -- receiver mailbox lock.
    ---------------------------------------------------------------------------
    procedure takeBoundNotification (pid     : in  ProcessID;
                                     found   : out Boolean;
                                     msg     : out Message)
        with SPARK_Mode => On
    is
    begin
        found := False;
        msg   := NULL_MESSAGE;

        if proctab(pid).boundNotification /= NO_PROCESS then
            checkBound : declare
                bn : constant ProcessID := proctab(pid).boundNotification;
            begin
                if mailtab(bn).notifyWord /= 0 then
                    msg := (tag      => (label  => 0,
                                         length => 1,
                                         flags  => 0,
                                         badge  => 0),
                            capBadge => 0,
                            words    => (0 => mailtab(bn).notifyWord,
                                         others => 0));
                    mailtab(bn).notifyWord := 0;
                    found := True;
                end if;
            end checkBound;
        end if;
    end takeBoundNotification;

    ---------------------------------------------------------------------------
    -- receive
    --
    -- Check if a sender is already waiting in our sendQueue. If so, accept
    -- the message immediately and move the sender to WAITINGFORREPLY.
    -- Otherwise, enqueue ourselves as a receiver and block.
    ---------------------------------------------------------------------------
    procedure receive (from : out ProcessID; msg : out Message) with
        SPARK_Mode => On
    is
        mypid    : constant ProcessID := PerCPUData.getCurrentPID;
        receiver : constant ProcessID := getReceiver (mypid);
        sender   : ProcessID;
        ignore   : ProcessID;
        re       : RingEntry;
        ok       : Boolean;
        notifyMsg : Message;
        notifyFound : Boolean;
    begin
        -- Validate our own state
        if mypid = NO_PROCESS then
            from := NO_PROCESS;
            msg  := NULL_MESSAGE;
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
            return;
        end if;

        -- Check unified ring (submit messages, events, send Path 1).
        dequeueRing (receiver, re, ok);
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
            return;
        end if;

        -- Check bound notification before blocking (seL4-style).
        takeBoundNotification (mypid, notifyFound, notifyMsg);
        if notifyFound then
            from := NO_PROCESS;
            msg  := notifyMsg;

            proctab(mypid).caps(Capabilities.REPLY_CAP_SLOT) :=
                Capabilities.NULL_CAPABILITY;

            Spinlocks.exitCriticalSection (mailtab(receiver).lock);
            return;
        end if;

        -- No message and no sender waiting. Block as a receiver.
        proctab(mypid).queueKey := receiver;
        Queues.enqueue (mailtab(receiver).recvQueue, mypid, ignore);
        proctab(mypid).state := RECEIVING;

        Spinlocks.exitCriticalSection (mailtab(receiver).lock);

        yield;

        -- Woken by send/submit/sendEvent. Check sendQueue first.
        Spinlocks.enterCriticalSection (mailtab(receiver).lock);

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
            -- Woken by submit/sendEvent/send Path 1 — dequeue from ring.
            dequeueRing (receiver, re, ok);
            if ok then
                from := re.sender;
                msg  := re.msg;
            else
                takeBoundNotification (mypid, notifyFound, notifyMsg);
                if notifyFound then
                    from := NO_PROCESS;
                    msg  := notifyMsg;
                else
                    from := NO_PROCESS;
                    msg  := NULL_MESSAGE;
                end if;
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

        Spinlocks.exitCriticalSection (mailtab(receiver).lock);
    end receive;

    ---------------------------------------------------------------------------
    -- receiveEvent
    -- Blocking receive from the unified ring buffer.
    ---------------------------------------------------------------------------
    function receiveEvent return Message with SPARK_Mode => On is
        mypid    : constant ProcessID := PerCPUData.getCurrentPID;
        receiver : constant ProcessID := getReceiver (mypid);
        re       : RingEntry;
        ok       : Boolean;
    begin
        loop
            Spinlocks.enterCriticalSection (mailtab(receiver).lock);

            dequeueRingKind (receiver, RING_EVENT, re, ok);
            if not ok then
                dequeueRingKind (receiver, RING_NOTIFY, re, ok);
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
    procedure receiveEventNB (msg : out Message; found : out Boolean) with
        SPARK_Mode => On
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

        dequeueRingKind (receiver, RING_EVENT, re, found);
        if not found then
            dequeueRingKind (receiver, RING_NOTIFY, re, found);
        end if;
        if found then
            msg := re.msg;
        end if;

        Spinlocks.exitCriticalSection (mailtab(receiver).lock);
    end receiveEventNB;

    ---------------------------------------------------------------------------
    -- receiveNB
    -- Non-blocking receive. Checks for a pending message or queued sender.
    ---------------------------------------------------------------------------
    ---------------------------------------------------------------------------
    -- replyWait
    -- Fused reply+receive: reply to previous sender, then check for next
    -- message immediately without yielding. In the common server pattern
    -- (next client already waiting in sendQueue), this handles the full
    -- round-trip with zero context switches.
    ---------------------------------------------------------------------------
    procedure replyWait (replyTo  : in  ProcessID;
                         replyMsg : in  Message;
                         from     : out ProcessID;
                         msg      : out Message) with
        SPARK_Mode => On
    is
        mypid    : constant ProcessID := PerCPUData.getCurrentPID;
        receiver : constant ProcessID := getReceiver (mypid);
        ignore   : Unsigned_64;
        sender   : ProcessID;
        ign      : ProcessID;
        rtState  : ProcessState;
        re       : RingEntry;
        ok       : Boolean;
        notifyMsg : Message;
        notifyFound : Boolean;
        directReply : Boolean := False;
    begin
        -----------------------------------------------------------------------
        -- Phase 1: Reply to previous sender
        -----------------------------------------------------------------------
        if replyTo /= NO_PROCESS then
            rtState := proctab(replyTo).state;

            if rtState /= INVALID then
                if rtState = WAITINGFORREPLY then
                    validateRW : declare
                        doReply  : Boolean;
                        requestId : Unsigned_64;
                        pragma Unreferenced (requestId);
                    begin
                        consumeReplyAuthority
                          (caller    => mypid,
                           replyTo   => replyTo,
                           requestId => requestId,
                           ok        => doReply);

                        if doReply then
                            proctab(replyTo).replyMsg := replyMsg;
                            if proctab(replyTo).cpu =
                               PerCPUData.getCPUNumber
                            then
                                -- Same-CPU replyWait can hand control
                                -- directly back to the caller if this server
                                -- has to block for the next request.
                                directReply := True;
                            else
                                notify (replyTo);
                            end if;
                        end if;
                    end validateRW;
                else
                    ignore := reply (replyTo, replyMsg);
                end if;
            end if;
        end if;

        -----------------------------------------------------------------------
        -- Phase 2: Receive next message
        -----------------------------------------------------------------------
        Spinlocks.enterCriticalSection (mailtab(receiver).lock);

        -- Check sendQueue first (synchronous senders).
        if not Queues.isEmpty (mailtab(receiver).sendQueue) then
            if directReply then
                notify (replyTo);
                directReply := False;
            end if;

            Queues.dequeue (mailtab(receiver).sendQueue, sender);

            msg  := proctab(sender).sendMsg;
            from := sender;
            re   := (msg       => msg,
                     sender    => sender,
                     kind      => RING_SYNC,
                     requestId => NO_REQUEST_ID);

            proctab(sender).state := WAITINGFORREPLY;

            proctab(mypid).caps(Capabilities.REPLY_CAP_SLOT) :=
                (capType  => Capabilities.CAP_REPLY,
                 rights   => Capabilities.ALL_RIGHTS,
                 capBadge => Capabilities.NO_BADGE,
                 object   => (ref   => Unsigned_64(from),
                              param => NO_REQUEST_ID),
                 gen      => proctab(from).capGeneration);

            Spinlocks.exitCriticalSection (mailtab(receiver).lock);
            return;
        end if;

        -- Check unified ring (submit, events, send Path 1).
        dequeueRing (receiver, re, ok);
        if ok then
            if directReply then
                notify (replyTo);
                directReply := False;
            end if;

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

            Spinlocks.exitCriticalSection (mailtab(receiver).lock);
            return;
        end if;

        -- No message available — block as receiver
        takeBoundNotification (mypid, notifyFound, notifyMsg);
        if notifyFound then
            if directReply then
                notify (replyTo);
                directReply := False;
            end if;

            from := NO_PROCESS;
            msg  := notifyMsg;

            proctab(mypid).caps(Capabilities.REPLY_CAP_SLOT) :=
                Capabilities.NULL_CAPABILITY;

            Spinlocks.exitCriticalSection (mailtab(receiver).lock);
            return;
        end if;

        -- No message available — block as receiver
        proctab(mypid).queueKey := receiver;
        Queues.enqueue (mailtab(receiver).recvQueue, mypid, ign);
        proctab(mypid).state := RECEIVING;

        if directReply then
            Spinlocks.enterCriticalSection (lock);
            Spinlocks.exitCriticalSection (mailtab(receiver).lock);
            directSwitch (mypid, replyTo);
            Spinlocks.exitCriticalSection (lock);
        else
            Spinlocks.exitCriticalSection (mailtab(receiver).lock);

            yield;
        end if;

        -- Woken — check sendQueue first.
        Spinlocks.enterCriticalSection (mailtab(receiver).lock);

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
            dequeueRing (receiver, re, ok);
            if ok then
                from := re.sender;
                msg  := re.msg;
            else
                takeBoundNotification (mypid, notifyFound, notifyMsg);
                if notifyFound then
                    from := NO_PROCESS;
                    msg  := notifyMsg;
                else
                    from := NO_PROCESS;
                    msg  := NULL_MESSAGE;
                end if;
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

        Spinlocks.exitCriticalSection (mailtab(receiver).lock);
    end replyWait;

    procedure receiveServiceRequestNB (from  : out ProcessID;
                                       msg   : out Message;
                                       found : out Boolean) with
        SPARK_Mode => On
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
            -- may contain events and notifications, but this receive path must
            -- leave them queued for receiveEventNB/notification APIs.
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
    -- Non-blocking mixed receive. This is the historical receiveNB behavior:
    -- it may consume service requests, one-way messages, events, and bound
    -- notifications. Use only for intentionally mixed dispatch loops.
    ---------------------------------------------------------------------------
    procedure receiveAnyIpcNB (from  : out ProcessID;
                               msg   : out Message;
                               found : out Boolean) with
        SPARK_Mode => On
    is
        mypid    : constant ProcessID := PerCPUData.getCurrentPID;
        receiver : constant ProcessID := getReceiver (mypid);
        sender   : ProcessID;
        re       : RingEntry;
        notifyMsg : Message;
        notifyFound : Boolean;
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
            -- whether it is a request, event, or notification.
            dequeueRing (receiver, re, found);
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
                takeBoundNotification (mypid, notifyFound, notifyMsg);
                if notifyFound then
                    from  := NO_PROCESS;
                    msg   := notifyMsg;
                    found := True;
                    proctab(mypid).caps(Capabilities.REPLY_CAP_SLOT) :=
                        Capabilities.NULL_CAPABILITY;
                else
                    from  := NO_PROCESS;
                    msg   := NULL_MESSAGE;
                end if;
            end if;
        end if;

        Spinlocks.exitCriticalSection (mailtab(receiver).lock);
    end receiveAnyIpcNB;

    procedure receiveNB (from  : out ProcessID;
                         msg   : out Message;
                         found : out Boolean) with
        SPARK_Mode => On
    is
    begin
        receiveAnyIpcNB (from, msg, found);
    end receiveNB;

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
    --   The receiver's receive()/receiveNB() will dequeue us and set our
    --   state to WAITINGFORREPLY. Then reply() calls notify() which adds
    --   us to the ready list. We resume with reply already delivered.
    ---------------------------------------------------------------------------
    function send (dest : ProcessID; msg : Message) return MessageTag
        with SPARK_Mode => On
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
                    return NULL_TAG;
                end if;
            end enforceCheck;
        end if;

        -- Store our message in per-sender storage so it cannot be
        -- overwritten by another sender racing to the same destination.
        proctab(pid).sendMsg := msg;

        Spinlocks.enterCriticalSection (mailtab(dest).lock);

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
    procedure sendEvent (dest : ProcessID; msg : Message)
        with SPARK_Mode => On is
        ok      : Boolean;
        removed : ProcessID;
    begin
        -- Validate destination
        if dest = NO_PROCESS then
            return;
        end if;

        if proctab(dest).state = INVALID then
            return;
        end if;

        Spinlocks.enterCriticalSection (mailtab(dest).lock);

        enqueueRing (dest,
                     (msg       => msg,
                      sender    => NO_PROCESS,
                      kind      => RING_EVENT,
                      requestId => NO_REQUEST_ID),
                     ok);

        --  receive() is the intentional mixed-lane wait primitive: it may
        --  consume events as well as requests. Wake both the event-specific
        --  waiter and a process blocked in mixed receive, otherwise an IRQ can
        --  remain queued forever while its driver sleeps in RECEIVING.
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
    end sendEvent;

    ---------------------------------------------------------------------------
    -- notifySupervisor
    -- Send a non-blocking fault event to the supervisor of the given process.
    ---------------------------------------------------------------------------
    procedure notifySupervisor (pid        : ProcessID;
                                faultLabel : Unsigned_32;
                                detail0    : Unsigned_64;
                                detail1    : Unsigned_64;
                                detail2    : Unsigned_64)
        with SPARK_Mode => On
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
    function reply (replyTo : ProcessID; msg : Message) return Unsigned_64
        with SPARK_Mode => On
    is
        mypid : constant ProcessID := PerCPUData.getCurrentPID;
        token     : Unsigned_64;
        requestId : Unsigned_64;
        ok        : Boolean;
    begin
        -- Validate target
        if replyTo = NO_PROCESS then
            return 0;
        end if;

        if proctab(replyTo).state = INVALID then
            return 0;
        end if;

        consumeReplyAuthority (caller    => mypid,
                               replyTo   => replyTo,
                               requestId => requestId,
                               ok        => ok);
        if not ok then
            return 0;
        end if;

        if proctab(replyTo).state = WAITINGFORREPLY then
            -- SYNC PATH: store reply, wake sender
            proctab(replyTo).replyMsg := msg;
            if proctab(replyTo).cpu = PerCPUData.getCPUNumber then
                -- Same CPU: fast path directSwitch to sender
                Spinlocks.enterCriticalSection (lock);
                ready (mypid);
                directSwitch (mypid, replyTo);
                Spinlocks.exitCriticalSection (lock);
            else
                -- Cross CPU: wake sender on its home CPU, keep running
                notify (replyTo);
            end if;
        else
            -- ASYNC PATH: look up token from sender's pendingRequests,
            -- enqueue CompletionEntry, wake if WAITINGFORCOMPLETION.
            findAndRemovePending (sender  => replyTo,
                                  replier => mypid,
                                  requestId => requestId,
                                  token   => token,
                                  found   => ok);

            if not ok then
                -- No pending request found for this replier — not an error,
                -- the sender may have already been cleaned up.
                return 0;
            end if;

            -- Enqueue completion into sender's queue.
            -- completionTab is protected by mailtab(replyTo).lock when
            -- accessed from reply(). We acquire it here since the server
            -- (caller) doesn't already hold it.
            Spinlocks.enterCriticalSection (mailtab(replyTo).lock);

            enqueueCompletion (
                owner   => replyTo,
                item    => (requestId => requestId,
                            token     => token,
                            msg       => msg,
                            from      => mypid,
                            status    => COMPLETION_OK,
                            valid     => True),
                success => ok);

            Spinlocks.exitCriticalSection (mailtab(replyTo).lock);

            if not ok then
                -- Completion queue full — completion lost.
                return 0;
            end if;

            -- Wake sender if blocked in waitCompletion
            if proctab(replyTo).state = WAITINGFORCOMPLETION then
                notify (replyTo);
            end if;
        end if;

        return 1;
    end reply;

    ---------------------------------------------------------------------------
    -- replyCap
    -- Complete a request using a specific saved reply capability slot.
    ---------------------------------------------------------------------------
    function replyCap
        (capSlot : Capabilities.CapabilitySlot;
         msg     : Message) return Unsigned_64
        with SPARK_Mode => On
    is
        mypid     : constant ProcessID := PerCPUData.getCurrentPID;
        cap       : Capabilities.Capability;
        replyTo   : ProcessID;
        requestId : Unsigned_64;
        token     : Unsigned_64;
        ok        : Boolean;
    begin
        if mypid = NO_PROCESS then
            return 0;
        end if;

        cap := proctab(mypid).caps(capSlot);
        if cap.capType /= Capabilities.CAP_REPLY then
            return 0;
        end if;

        if cap.object.ref = 0 or else
           cap.object.ref > Unsigned_64(ProcessID'Last)
        then
            return 0;
        end if;

        replyTo := ProcessID(cap.object.ref);
        if proctab(replyTo).state = INVALID or else
           cap.gen /= proctab(replyTo).capGeneration
        then
            return 0;
        end if;

        -- Consume the exact one-use reply cap selected by userspace before
        -- causing any externally visible completion.
        Capabilities.Operations.takeReplyCap
          (table => proctab(mypid).caps,
           slot  => capSlot,
           cap   => cap,
           taken => ok);
        if not ok then
            return 0;
        end if;

        requestId := cap.object.param;
        proctab(mypid).deferredReplyCaps :=
            proctab(mypid).deferredReplyCaps and
            not Shift_Left (Unsigned_64'(1), capSlot);

        if proctab(replyTo).state = WAITINGFORREPLY then
            proctab(replyTo).replyMsg := msg;
            if proctab(replyTo).cpu = PerCPUData.getCPUNumber then
                Spinlocks.enterCriticalSection (lock);
                ready (mypid);
                directSwitch (mypid, replyTo);
                Spinlocks.exitCriticalSection (lock);
            else
                notify (replyTo);
            end if;
        else
            findAndRemovePending (sender    => replyTo,
                                  replier   => mypid,
                                  requestId => requestId,
                                  token     => token,
                                  found     => ok);

            if not ok then
                return 0;
            end if;

            Spinlocks.enterCriticalSection (mailtab(replyTo).lock);
            enqueueCompletion (
                owner   => replyTo,
                item    => (requestId => requestId,
                            token     => token,
                            msg       => msg,
                            from      => mypid,
                            status    => COMPLETION_OK,
                            valid     => True),
                success => ok);
            Spinlocks.exitCriticalSection (mailtab(replyTo).lock);

            if not ok then
                return 0;
            end if;

            if proctab(replyTo).state = WAITINGFORCOMPLETION then
                notify (replyTo);
            end if;
        end if;

        return 1;
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
    function submit (dest  : ProcessID;
                     msg   : Message;
                     token : Unsigned_64) return Boolean
        with SPARK_Mode => On
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

        -- Check we have room for another pending request
        if wantCompletion and
           proctab(pid).numPending >= MAX_PENDING_ASYNC
        then
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

        Spinlocks.enterCriticalSection (mailtab(dest).lock);

        -- Enqueue in unified ring
        enqueueRing (dest,
                     (msg       => msg,
                      sender    => pid,
                      kind      => entryKind,
                      requestId => requestId),
                     ok);

        if not ok then
            Spinlocks.exitCriticalSection (mailtab(dest).lock);
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
            Queues.dequeue (mailtab(dest).recvQueue, receiver);
            ready (receiver);
        elsif proctab(dest).state = SLEEPING then
            declare
                woken : Boolean;
            begin
                Queues.wakeFromSleep (dest, woken);
            end;
        end if;

        Spinlocks.exitCriticalSection (mailtab(dest).lock);

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
        with SPARK_Mode => Off
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
        with SPARK_Mode => On
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

    ---------------------------------------------------------------------------
    -- createGrant
    -- Map pages from caller's address space into grantee's address space.
    ---------------------------------------------------------------------------
    procedure createGrant (grantee   : in  ProcessID;
                           localAddr : in  System.Address;
                           numPages  : in  Natural;
                           perm      : in  GrantPermission;
                           id        : out Natural;
                           success   : out Boolean)
        with SPARK_Mode => On
    is
        pid      : constant ProcessID := PerCPUData.getCurrentPID;
        -- Threads share parent's address space and grant table
        owner    : constant ProcessID :=
            (if proctab(pid).isThread then proctab(pid).ppid else pid);
        physAddr : Virtmem.PhysAddress;
        granteeVirt : Integer_Address;
        flags    : Unsigned_64;
        ok       : Boolean;
        slotFound : Boolean := False;
        granterSlot : GrantID := 0;

        --  Globally unique grant ID: ownerPID * MAX_GRANTS + granterSlot.
        --  This ensures each grant maps to a unique address in the grantee,
        --  even when multiple granters create grants to the same grantee.
        globalId : Natural;

        procedure mapPageInst is new Virtmem.mapPage (BuddyAllocator.allocFrame);
    begin
        id      := 0;
        success := False;

        -- Validate parameters
        if grantee = NO_PROCESS then
            return;
        end if;

        if proctab(grantee).state = INVALID then
            return;
        end if;

        if numPages = 0 or numPages > MAX_GRANT_PAGES then
            return;
        end if;

        -- Check page alignment
        if (To_Integer (localAddr) and 16#FFF#) /= 0 then
            return;
        end if;

        -- Find a free grant slot in owner's array
        for i in GrantID loop
            if not proctab(owner).grants(i).active then
                granterSlot := i;
                slotFound   := True;
                exit;
            end if;
        end loop;

        if not slotFound then
            return;
        end if;

        --  Compute globally unique ID: each owner PID gets its own
        --  region in the grantee's address space.
        globalId := Natural (owner) * MAX_GRANTS_PER_PROCESS + granterSlot;

        -- Determine permission flags
        if perm = GRANT_READWRITE then
            flags := Virtmem.PG_USERDATA;
        else
            flags := Virtmem.PG_USERDATARO;
        end if;

        -- Map each page from granter's space into grantee's space
        for i in 0 .. numPages - 1 loop
            -- Look up physical address behind owner's virtual address
            physAddr := Virtmem.tableWalk (
                virt => To_Integer (localAddr) +
                        Integer_Address (i) * Integer_Address (Virtmem.PAGE_SIZE),
                myP4 => addrtab(proctab(owner).pgTable));

            if physAddr = 0 then
                -- Page not mapped in granter's space — roll back
                for j in 0 .. i - 1 loop
                    granteeVirt := GRANT_REGION_BASE +
                        Integer_Address (globalId) * GRANT_SLOT_SIZE +
                        Integer_Address (j) * Integer_Address (Virtmem.PAGE_SIZE);

                    Virtmem.unmapPage (
                        virt => granteeVirt,
                        myP4 => addrtab(proctab(grantee).pgTable),
                        success => ok);
                end loop;
                return;
            end if;

            -- Calculate target virtual address in grantee's space
            granteeVirt := GRANT_REGION_BASE +
                Integer_Address (globalId) * GRANT_SLOT_SIZE +
                Integer_Address (i) * Integer_Address (Virtmem.PAGE_SIZE);

            -- Map the physical page into grantee's address space
            mapPageInst (
                phys    => physAddr,
                virt    => granteeVirt,
                flags   => flags,
                myP4    => addrtab(proctab(grantee).pgTable),
                success => ok);

            if not ok then
                -- Roll back previously mapped pages
                for j in 0 .. i - 1 loop
                    granteeVirt := GRANT_REGION_BASE +
                        Integer_Address (globalId) * GRANT_SLOT_SIZE +
                        Integer_Address (j) * Integer_Address (Virtmem.PAGE_SIZE);

                    Virtmem.unmapPage (
                        virt => granteeVirt,
                        myP4 => addrtab(proctab(grantee).pgTable),
                        success => ok);
                end loop;
                return;
            end if;
        end loop;

        -- Record grant metadata in owner's grant table
        proctab(owner).grants(granterSlot) := (
            active      => True,
            granterPID  => owner,
            granteePID  => grantee,
            granterAddr => localAddr,
            granteeAddr => To_Address (
                GRANT_REGION_BASE +
                Integer_Address (globalId) * GRANT_SLOT_SIZE),
            numPages    => numPages,
            permission  => perm
        );

        --  Return globally unique ID for both address computation
        --  and revocation.
        id      := globalId;
        success := True;
    end createGrant;

    ---------------------------------------------------------------------------
    -- revokeGrant
    -- Unmap granted pages from grantee's address space.
    ---------------------------------------------------------------------------
    procedure revokeGrant (id : GrantID)
        with SPARK_Mode => On
    is
        pid   : constant ProcessID := PerCPUData.getCurrentPID;
        -- Threads share parent's grant table
        owner : constant ProcessID :=
            (if proctab(pid).isThread then proctab(pid).ppid else pid);
        granteeVirt : Integer_Address;
        ok    : Boolean;
        g     : Grant renames proctab(owner).grants(id);
    begin
        if not g.active then
            return;
        end if;

        --  Don't unmap if grantee is already dead (page tables freed)
        if proctab(g.granteePID).state = INVALID then
            g := (active => False, others => <>);
            return;
        end if;

        -- Unmap each page from grantee's address space
        for i in 0 .. g.numPages - 1 loop
            granteeVirt := To_Integer (g.granteeAddr) +
                Integer_Address (i) * Integer_Address (Virtmem.PAGE_SIZE);

            Virtmem.unmapPage (
                virt    => granteeVirt,
                myP4    => addrtab(proctab(g.granteePID).pgTable),
                success => ok);
        end loop;

        -- Invalidate TLB for grantee. If grantee is on this CPU,
        -- flush locally. If on another CPU, set the global TLB flush
        -- flag and send reschedule IPI to trigger remote flush.
        tlbShootdown : declare
            granteeCPU : constant Natural := proctab(g.granteePID).cpu;
        begin
            if granteeCPU = PerCPUData.getCPUNumber then
                Virtmem.flushTLB;
            else
                tlbFlushPending(granteeCPU) := True;
                IPI.sendReschedule (granteeCPU);
            end if;
        end tlbShootdown;

        -- Mark grant slot as inactive
        g := (active => False, others => <>);
    end revokeGrant;

    ---------------------------------------------------------------------------
    -- revokeAllGrants
    -- Revoke all active grants owned by the specified process.
    -- Called during process kill().
    ---------------------------------------------------------------------------
    procedure revokeAllGrants (pid : ProcessID)
        with SPARK_Mode => On
    is
        g : Grant;
        granteeVirt : Integer_Address;
        ok : Boolean;
    begin
        for i in GrantID loop
            g := proctab(pid).grants(i);

            if g.active then
                -- Only unmap if grantee is still valid
                if proctab(g.granteePID).state /= INVALID then
                    for j in 0 .. g.numPages - 1 loop
                        granteeVirt := To_Integer (g.granteeAddr) +
                            Integer_Address (j) * Integer_Address (Virtmem.PAGE_SIZE);

                        Virtmem.unmapPage (
                            virt    => granteeVirt,
                            myP4    => addrtab(proctab(g.granteePID).pgTable),
                            success => ok);
                    end loop;

                end if;

                proctab(pid).grants(i) := (active => False, others => <>);
            end if;
        end loop;
    end revokeAllGrants;

    ---------------------------------------------------------------------------
    -- Capability-Aware IPC
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- capSend
    ---------------------------------------------------------------------------
    function capSend (capSlot : Capabilities.CapabilitySlot;
                      msg     : Message) return MessageTag
        with SPARK_Mode => On
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
        return send (dest => candidatePID, msg => stamped);
    end capSend;

    ---------------------------------------------------------------------------
    -- capCall
    ---------------------------------------------------------------------------
    function capCall (capSlot : Capabilities.CapabilitySlot;
                      msg     : Message) return MessageTag
        with SPARK_Mode => On
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
        return send (dest => candidatePID, msg => stamped);
    end capCall;

    ---------------------------------------------------------------------------
    -- capSubmit
    ---------------------------------------------------------------------------
    function capSubmit (capSlot : Capabilities.CapabilitySlot;
                        msg     : Message;
                        token   : Unsigned_64) return Boolean
        with SPARK_Mode => On
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
                       token => token);
    end capSubmit;

    ---------------------------------------------------------------------------
    -- Notification Operations
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- capNotify
    ---------------------------------------------------------------------------
    function capNotify (capSlot : Capabilities.CapabilitySlot) return Boolean
        with SPARK_Mode => On
    is
        pid     : constant ProcessID := PerCPUData.getCurrentPID;
        cap     : Capabilities.Capability;
        opStatus : Capabilities.Operations.OperationStatus;
        destPID : Unsigned_64;
    begin
        Capabilities.Operations.lookupCap (
            table  => proctab(pid).caps,
            slot   => capSlot,
            cap    => cap,
            status => opStatus);

        if opStatus /= Capabilities.Operations.OP_OK then
            return False;
        end if;

        if cap.capType /= Capabilities.CAP_NOTIFICATION then
            return False;
        end if;

        if not cap.rights(Capabilities.RIGHT_WRITE) then
            return False;
        end if;

        destPID := cap.object.ref;

        if destPID = 0 or else destPID > Unsigned_64(ProcessID'Last) then
            return False;
        end if;

        -- Generation check
        if cap.gen /= proctab(ProcessID(destPID)).capGeneration then
            return False;
        end if;

        deliverBound : declare
            notifPID : constant ProcessID := ProcessID(destPID);
            recvPID  : ProcessID := NO_PROCESS;
            recv     : ProcessID := NO_PROCESS;
            shouldWakeReceive : Boolean := False;
        begin
            Spinlocks.enterCriticalSection (mailtab(notifPID).lock);

            -- OR badge into notification word
            mailtab(notifPID).notifyWord :=
                mailtab(notifPID).notifyWord or cap.capBadge;

            -- Wake if blocked in notifyWait
            if mailtab(notifPID).notifyWaiter then
                mailtab(notifPID).notifyWaiter := False;
                notify (notifPID);
            elsif mailtab(notifPID).boundReceiver /= NO_PROCESS then
                recvPID := mailtab(notifPID).boundReceiver;
                recv    := getReceiver (recvPID);
                if proctab(recvPID).state = RECEIVING then
                    shouldWakeReceive := True;
                end if;
            end if;

            Spinlocks.exitCriticalSection (mailtab(notifPID).lock);

            if shouldWakeReceive then
                wakeRecv : declare
                    ignore : ProcessID;
                begin
                    Spinlocks.enterCriticalSection (mailtab(recv).lock);
                    if proctab(recvPID).state = RECEIVING then
                        Queues.popItem (
                            mailtab(recv).recvQueue, recvPID, ignore);
                        notify (recvPID);
                    end if;
                    Spinlocks.exitCriticalSection (mailtab(recv).lock);
                end wakeRecv;
            end if;
        end deliverBound;

        return True;
    end capNotify;

    ---------------------------------------------------------------------------
    -- notifyWait
    ---------------------------------------------------------------------------
    function notifyWait return Unsigned_64
        with SPARK_Mode => On
    is
        mypid    : constant ProcessID := PerCPUData.getCurrentPID;
        receiver : constant ProcessID := getReceiver (mypid);
        result   : Unsigned_64;
    begin
        loop
            Spinlocks.enterCriticalSection (mailtab(receiver).lock);

            if mailtab(receiver).notifyWord /= 0 then
                result := mailtab(receiver).notifyWord;
                mailtab(receiver).notifyWord := 0;
                mailtab(receiver).notifyWaiter := False;
                Spinlocks.exitCriticalSection (mailtab(receiver).lock);
                return result;
            end if;

            -- Block until notified
            mailtab(receiver).notifyWaiter := True;
            proctab(mypid).state := WAITINGFORNOTIFY;
            Spinlocks.exitCriticalSection (mailtab(receiver).lock);

            yield;
        end loop;
    end notifyWait;

    ---------------------------------------------------------------------------
    -- notifyPoll
    ---------------------------------------------------------------------------
    function notifyPoll return Unsigned_64
        with SPARK_Mode => On
    is
        mypid    : constant ProcessID := PerCPUData.getCurrentPID;
        receiver : constant ProcessID := getReceiver (mypid);
        result   : Unsigned_64;
    begin
        Spinlocks.enterCriticalSection (mailtab(receiver).lock);

        result := mailtab(receiver).notifyWord;
        mailtab(receiver).notifyWord := 0;

        Spinlocks.exitCriticalSection (mailtab(receiver).lock);

        return result;
    end notifyPoll;

    ---------------------------------------------------------------------------
    -- bindNotification
    ---------------------------------------------------------------------------
    function bindNotification
        (capSlot : Capabilities.CapabilitySlot) return Boolean
        with SPARK_Mode => On
    is
        mypid : constant ProcessID := PerCPUData.getCurrentPID;
        cap   : Capabilities.Capability;
        opStatus : Capabilities.Operations.OperationStatus;
        notifPIDRaw : Unsigned_64;
        notifPID : ProcessID;
    begin
        if mypid = NO_PROCESS then
            return False;
        end if;

        Capabilities.Operations.lookupCap (
            table  => proctab(mypid).caps,
            slot   => capSlot,
            cap    => cap,
            status => opStatus);

        if opStatus /= Capabilities.Operations.OP_OK then
            return False;
        end if;

        if cap.capType /= Capabilities.CAP_NOTIFICATION then
            return False;
        end if;

        if not cap.rights(Capabilities.RIGHT_READ) then
            return False;
        end if;

        notifPIDRaw := cap.object.ref;
        if notifPIDRaw = 0 or else
           notifPIDRaw > Unsigned_64(ProcessID'Last)
        then
            return False;
        end if;

        notifPID := ProcessID(notifPIDRaw);

        if cap.gen /= proctab(notifPID).capGeneration then
            return False;
        end if;

        proctab(mypid).boundNotification := notifPID;
        Spinlocks.enterCriticalSection (mailtab(notifPID).lock);
        mailtab(notifPID).boundReceiver  := mypid;
        Spinlocks.exitCriticalSection (mailtab(notifPID).lock);
        return True;
    end bindNotification;

    ---------------------------------------------------------------------------
    -- unbindNotification
    ---------------------------------------------------------------------------
    procedure unbindNotification
        with SPARK_Mode => On
    is
        mypid : constant ProcessID := PerCPUData.getCurrentPID;
        old   : ProcessID;
    begin
        if mypid = NO_PROCESS then
            return;
        end if;

        old := proctab(mypid).boundNotification;
        proctab(mypid).boundNotification := NO_PROCESS;

        if old /= NO_PROCESS then
            Spinlocks.enterCriticalSection (mailtab(old).lock);
            mailtab(old).boundReceiver := NO_PROCESS;
            Spinlocks.exitCriticalSection (mailtab(old).lock);
        end if;
    end unbindNotification;

end Process.IPC;
