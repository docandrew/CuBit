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
    -- Scan the sender's pending requests for one targeting the given replier.
    -- Returns the token and removes the entry (swap-remove).
    ---------------------------------------------------------------------------
    procedure findAndRemovePending (sender  : in  ProcessID;
                                    replier : in  ProcessID;
                                    token   : out Unsigned_64;
                                    found   : out Boolean)
        with SPARK_Mode => On
    is
    begin
        found := False;
        token := 0;

        for i in 0 .. proctab(sender).numPending - 1 loop
            if proctab(sender).pendingRequests(i).dest = replier then
                token := proctab(sender).pendingRequests(i).token;

                -- Swap-remove: replace with last entry
                proctab(sender).numPending := proctab(sender).numPending - 1;

                if i < proctab(sender).numPending then
                    proctab(sender).pendingRequests(i) :=
                        proctab(sender).pendingRequests(proctab(sender).numPending);
                end if;

                proctab(sender).pendingRequests(proctab(sender).numPending) :=
                    (NO_PROCESS, 0);

                found := True;
                return;
            end if;
        end loop;
    end findAndRemovePending;

    ---------------------------------------------------------------------------
    -- Event Ring Buffer Helpers
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- enqueueEvent
    -- Push an event into a mailbox's event ring buffer.
    -- Caller must hold mailtab(owner).lock.
    -- @return True if enqueued, False if queue is full (event dropped).
    ---------------------------------------------------------------------------
    procedure enqueueEvent (owner   : in  ProcessID;
                            item    : in  Message;
                            success : out Boolean)
        with SPARK_Mode => On
    is
        eq : EventQueue renames mailtab(owner).events;
    begin
        if eq.count >= EVENT_QUEUE_SIZE then
            success := False;
            return;
        end if;

        eq.events(eq.head) := item;
        eq.head  := (eq.head + 1) mod EVENT_QUEUE_SIZE;
        eq.count := eq.count + 1;
        success  := True;
    end enqueueEvent;

    ---------------------------------------------------------------------------
    -- dequeueEvent
    -- Pop an event from a mailbox's event ring buffer.
    -- Caller must hold mailtab(owner).lock.
    ---------------------------------------------------------------------------
    procedure dequeueEvent (owner   : in  ProcessID;
                            item    : out Message;
                            success : out Boolean)
        with SPARK_Mode => On
    is
        eq : EventQueue renames mailtab(owner).events;
    begin
        if eq.count = 0 then
            item    := NULL_MESSAGE;
            success := False;
            return;
        end if;

        item     := eq.events(eq.tail);
        eq.events(eq.tail) := NULL_MESSAGE;
        eq.tail  := (eq.tail + 1) mod EVENT_QUEUE_SIZE;
        eq.count := eq.count - 1;
        success  := True;
    end dequeueEvent;

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
    begin
        -- Validate our own state
        if mypid = NO_PROCESS then
            from := NO_PROCESS;
            msg  := NULL_MESSAGE;
            return;
        end if;

        Spinlocks.enterCriticalSection (mailtab(receiver).lock);

        -- Check sendQueue FIRST. Path 2 senders store their message
        -- in proctab(sender).sendMsg, not in the shared mailbox.
        if not Queues.isEmpty (mailtab(receiver).sendQueue) then
            Queues.dequeue (mailtab(receiver).sendQueue, sender);

            -- Read from the sender's per-process storage
            msg  := proctab(sender).sendMsg;
            from := sender;

            proctab(sender).state := WAITINGFORREPLY;

            -- Mint one-use reply cap for this sender
            proctab(mypid).caps(Capabilities.REPLY_CAP_SLOT) :=
                (capType  => Capabilities.CAP_REPLY,
                 rights   => Capabilities.ALL_RIGHTS,
                 capBadge => Capabilities.NO_BADGE,
                 object   => (ref   => Unsigned_64(from),
                              param => 0),
                 gen      => proctab(from).capGeneration);

            Spinlocks.exitCriticalSection (mailtab(receiver).lock);
            return;
        end if;

        if mailtab(receiver).hasMsg then
            -- Message deposited by send() Path 1 or submit().
            from := mailtab(receiver).sender;
            mailtab(receiver).hasMsg := False;
            -- Path 1: sender in WAITINGFORREPLY, msg in sendMsg.
            -- submit(): sender running, msg in mailtab.msg.
            if from /= NO_PROCESS
               and then proctab(from).state = WAITINGFORREPLY
            then
                msg := proctab(from).sendMsg;
            else
                msg := mailtab(receiver).msg;
            end if;

            -- Mint one-use reply cap for this sender
            proctab(mypid).caps(Capabilities.REPLY_CAP_SLOT) :=
                (capType  => Capabilities.CAP_REPLY,
                 rights   => Capabilities.ALL_RIGHTS,
                 capBadge => Capabilities.NO_BADGE,
                 object   => (ref   => Unsigned_64(from),
                              param => 0),
                 gen      => proctab(from).capGeneration);

            Spinlocks.exitCriticalSection (mailtab(receiver).lock);
            return;
        end if;

        -- Check bound notification before blocking (seL4-style).
        -- If this process has a bound notification with pending bits,
        -- return it as a synthetic message instead of blocking.
        if proctab(mypid).boundNotification /= NO_PROCESS then
            checkBound : declare
                bn : constant ProcessID := proctab(mypid).boundNotification;
            begin
                if mailtab(bn).notifyWord /= 0 then
                    from := NO_PROCESS;
                    msg  := (tag      => (label  => 0,
                                          length => 1,
                                          flags  => 0,
                                          badge  => 0),
                             capBadge => 0,
                             words    => (0 => mailtab(bn).notifyWord,
                                          others => 0));
                    mailtab(bn).notifyWord := 0;

                    -- No real sender — clear any stale reply cap
                    proctab(mypid).caps(Capabilities.REPLY_CAP_SLOT) :=
                        Capabilities.NULL_CAPABILITY;

                    Spinlocks.exitCriticalSection (mailtab(receiver).lock);
                    return;
                end if;
            end checkBound;
        end if;

        -- No message and no sender waiting. Block as a receiver.
        proctab(mypid).queueKey := receiver;
        Queues.enqueue (mailtab(receiver).recvQueue, mypid, ignore);
        proctab(mypid).state := RECEIVING;

        Spinlocks.exitCriticalSection (mailtab(receiver).lock);

        yield;

        -- Woken by send() Path 1 or submit(). Check sendQueue first
        -- in case Path 2 senders enqueued while we were blocked.
        Spinlocks.enterCriticalSection (mailtab(receiver).lock);

        if not Queues.isEmpty (mailtab(receiver).sendQueue) then
            Queues.dequeue (mailtab(receiver).sendQueue, sender);

            msg  := proctab(sender).sendMsg;
            from := sender;

            proctab(sender).state := WAITINGFORREPLY;
        else
            -- Woken by send() Path 1 or submit().
            from := mailtab(receiver).sender;
            mailtab(receiver).hasMsg := False;
            -- Path 1 sender is in WAITINGFORREPLY; read from sendMsg.
            -- submit() sender is running; read from mailtab.msg.
            if from /= NO_PROCESS
               and then proctab(from).state = WAITINGFORREPLY
            then
                msg := proctab(from).sendMsg;
            else
                msg := mailtab(receiver).msg;
            end if;
        end if;

        -- Mint one-use reply cap for this sender
        proctab(mypid).caps(Capabilities.REPLY_CAP_SLOT) :=
            (capType  => Capabilities.CAP_REPLY,
             rights   => Capabilities.ALL_RIGHTS,
             capBadge => Capabilities.NO_BADGE,
             object   => (ref   => Unsigned_64(from),
                          param => 0),
             gen      => proctab(from).capGeneration);

        Spinlocks.exitCriticalSection (mailtab(receiver).lock);
    end receive;

    ---------------------------------------------------------------------------
    -- receiveEvent
    ---------------------------------------------------------------------------
    function receiveEvent return Message with SPARK_Mode => On is
        mypid    : constant ProcessID := PerCPUData.getCurrentPID;
        receiver : constant ProcessID := getReceiver (mypid);
        event    : Message;
        ok       : Boolean;
    begin
        loop
            Spinlocks.enterCriticalSection (mailtab(receiver).lock);

            dequeueEvent (receiver, event, ok);

            if ok then
                Spinlocks.exitCriticalSection (mailtab(receiver).lock);
                return event;
            end if;

            -- No event available, block
            proctab(mypid).state := WAITINGFOREVENT;
            Spinlocks.exitCriticalSection (mailtab(receiver).lock);

            yield;
        end loop;
    end receiveEvent;

    ---------------------------------------------------------------------------
    -- receiveEventNB
    -- Non-blocking event receive. Pops from the event ring buffer.
    ---------------------------------------------------------------------------
    procedure receiveEventNB (msg : out Message; found : out Boolean) with
        SPARK_Mode => On
    is
        mypid    : constant ProcessID := PerCPUData.getCurrentPID;
        receiver : constant ProcessID := getReceiver (mypid);
    begin
        msg   := NULL_MESSAGE;
        found := False;

        if mypid = NO_PROCESS then
            return;
        end if;

        Spinlocks.enterCriticalSection (mailtab(receiver).lock);

        dequeueEvent (receiver, msg, found);

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
    begin
        -----------------------------------------------------------------------
        -- Phase 1: Reply to previous sender
        -----------------------------------------------------------------------
        if replyTo /= NO_PROCESS then
            -- Read state once to avoid TOCTOU (Fix 1)
            rtState := proctab(replyTo).state;

            if rtState /= INVALID then
                if rtState = WAITINGFORREPLY then
                    -- Validate reply cap and deliver reply.
                    -- Kernel threads are exempt; userspace must
                    -- hold a matching CAP_REPLY. If validation
                    -- fails, silently skip — Phase 2 mints fresh.
                    validateRW : declare
                        doReply   : Boolean := False;
                        foundSlot : Capabilities.CapabilitySlot :=
                            Capabilities.REPLY_CAP_SLOT;
                        cap : Capabilities.Capability;
                        remaining : Unsigned_64;
                        bs  : Natural;
                    begin
                        if proctab(mypid).mode /= USER then
                            doReply := True;
                        else
                            cap := proctab(mypid).caps(
                                Capabilities.REPLY_CAP_SLOT);
                            if cap.capType =
                                   Capabilities.CAP_REPLY
                               and then cap.object.ref =
                                   Unsigned_64(replyTo)
                               and then cap.gen =
                                   proctab(replyTo).capGeneration
                            then
                                doReply := True;
                            else
                                -- Bitmap scan: iterate only
                                -- deferred reply cap slots
                                remaining := proctab(mypid)
                                    .deferredReplyCaps;
                                while remaining /= 0 loop
                                    bs := Util.getFirstSetBit
                                        (remaining);
                                    cap := proctab(mypid).caps(bs);
                                    if cap.capType =
                                       Capabilities.CAP_REPLY
                                       and then cap.object.ref =
                                           Unsigned_64(replyTo)
                                       and then cap.gen =
                                           proctab(replyTo)
                                               .capGeneration
                                    then
                                        doReply := True;
                                        foundSlot := bs;
                                        exit;
                                    end if;
                                    remaining :=
                                        remaining and (remaining - 1);
                                end loop;
                            end if;
                        end if;

                        if doReply then
                            -- Consume one-use reply cap + bitmap
                            if proctab(mypid).mode = USER then
                                proctab(mypid).caps(foundSlot) :=
                                    Capabilities.NULL_CAPABILITY;
                                proctab(mypid).deferredReplyCaps :=
                                    proctab(mypid).deferredReplyCaps
                                    and not Shift_Left
                                        (Unsigned_64'(1),
                                         foundSlot);
                            end if;

                            -- Store reply, make sender READY (no
                            -- directSwitch — check for next msg).
                            proctab(replyTo).replyMsg := replyMsg;
                            notify (replyTo);
                        end if;
                    end validateRW;
                else
                    -- Async path: delegate to full reply()
                    ignore := reply (replyTo, replyMsg);
                end if;
            end if;
        end if;

        -----------------------------------------------------------------------
        -- Phase 2: Receive next message without yielding if possible
        -----------------------------------------------------------------------
        Spinlocks.enterCriticalSection (mailtab(receiver).lock);

        -- Check sendQueue first. Path 2 senders store their message
        -- in proctab(sender).sendMsg, not in the shared mailbox.
        if not Queues.isEmpty (mailtab(receiver).sendQueue) then
            Queues.dequeue (mailtab(receiver).sendQueue, sender);

            -- Read from the sender's per-process storage
            msg  := proctab(sender).sendMsg;
            from := sender;

            proctab(sender).state := WAITINGFORREPLY;

            -- Mint one-use reply cap for this sender
            proctab(mypid).caps(Capabilities.REPLY_CAP_SLOT) :=
                (capType  => Capabilities.CAP_REPLY,
                 rights   => Capabilities.ALL_RIGHTS,
                 capBadge => Capabilities.NO_BADGE,
                 object   => (ref   => Unsigned_64(from),
                              param => 0),
                 gen      => proctab(from).capGeneration);

            Spinlocks.exitCriticalSection (mailtab(receiver).lock);
            return;
        end if;

        -- Check deposited message (send Path 1 or submit)
        if mailtab(receiver).hasMsg then
            from := mailtab(receiver).sender;
            mailtab(receiver).hasMsg := False;
            if from /= NO_PROCESS
               and then proctab(from).state = WAITINGFORREPLY
            then
                msg := proctab(from).sendMsg;
            else
                msg := mailtab(receiver).msg;
            end if;

            -- Mint one-use reply cap for this sender
            proctab(mypid).caps(Capabilities.REPLY_CAP_SLOT) :=
                (capType  => Capabilities.CAP_REPLY,
                 rights   => Capabilities.ALL_RIGHTS,
                 capBadge => Capabilities.NO_BADGE,
                 object   => (ref   => Unsigned_64(from),
                              param => 0),
                 gen      => proctab(from).capGeneration);

            Spinlocks.exitCriticalSection (mailtab(receiver).lock);
            return;
        end if;

        -- No message available — block as receiver
        proctab(mypid).queueKey := receiver;
        Queues.enqueue (mailtab(receiver).recvQueue, mypid, ign);
        proctab(mypid).state := RECEIVING;

        Spinlocks.exitCriticalSection (mailtab(receiver).lock);

        yield;

        -- Woken by send() Path 1 or submit(). Check sendQueue first
        -- in case Path 2 senders enqueued while we were blocked.
        Spinlocks.enterCriticalSection (mailtab(receiver).lock);

        if not Queues.isEmpty (mailtab(receiver).sendQueue) then
            Queues.dequeue (mailtab(receiver).sendQueue, sender);

            msg  := proctab(sender).sendMsg;
            from := sender;

            proctab(sender).state := WAITINGFORREPLY;
        else
            -- Woken by send() Path 1 or submit().
            from := mailtab(receiver).sender;
            mailtab(receiver).hasMsg := False;
            -- Path 1 sender is in WAITINGFORREPLY; read from sendMsg.
            -- submit() sender is running; read from mailtab.msg.
            if from /= NO_PROCESS
               and then proctab(from).state = WAITINGFORREPLY
            then
                msg := proctab(from).sendMsg;
            else
                msg := mailtab(receiver).msg;
            end if;
        end if;

        -- Mint one-use reply cap for this sender
        proctab(mypid).caps(Capabilities.REPLY_CAP_SLOT) :=
            (capType  => Capabilities.CAP_REPLY,
             rights   => Capabilities.ALL_RIGHTS,
             capBadge => Capabilities.NO_BADGE,
             object   => (ref   => Unsigned_64(from),
                          param => 0),
             gen      => proctab(from).capGeneration);

        Spinlocks.exitCriticalSection (mailtab(receiver).lock);
    end replyWait;

    procedure receiveNB (from  : out ProcessID;
                         msg   : out Message;
                         found : out Boolean) with
        SPARK_Mode => On
    is
        mypid    : constant ProcessID := PerCPUData.getCurrentPID;
        receiver : constant ProcessID := getReceiver (mypid);
        sender   : ProcessID;
    begin
        Spinlocks.enterCriticalSection (mailtab(receiver).lock);

        -- Check sendQueue FIRST. Path 2 senders store their message
        -- in proctab(sender).sendMsg, not in the shared mailbox.
        if not Queues.isEmpty (mailtab(receiver).sendQueue) then
            Queues.dequeue (mailtab(receiver).sendQueue, sender);

            -- Read from the sender's per-process storage
            msg   := proctab(sender).sendMsg;
            from  := sender;
            found := True;

            proctab(sender).state := WAITINGFORREPLY;

            -- Mint one-use reply cap for this sender
            proctab(mypid).caps(Capabilities.REPLY_CAP_SLOT) :=
                (capType  => Capabilities.CAP_REPLY,
                 rights   => Capabilities.ALL_RIGHTS,
                 capBadge => Capabilities.NO_BADGE,
                 object   => (ref   => Unsigned_64(from),
                              param => 0),
                 gen      => proctab(from).capGeneration);

        elsif mailtab(receiver).hasMsg then
            -- Message deposited by send() Path 1 or submit()
            from  := mailtab(receiver).sender;
            mailtab(receiver).hasMsg := False;
            found := True;
            if from /= NO_PROCESS
               and then proctab(from).state = WAITINGFORREPLY
            then
                msg := proctab(from).sendMsg;
            else
                msg := mailtab(receiver).msg;
            end if;

            -- Mint one-use reply cap for this sender
            proctab(mypid).caps(Capabilities.REPLY_CAP_SLOT) :=
                (capType  => Capabilities.CAP_REPLY,
                 rights   => Capabilities.ALL_RIGHTS,
                 capBadge => Capabilities.NO_BADGE,
                 object   => (ref   => Unsigned_64(from),
                              param => 0),
                 gen      => proctab(from).capGeneration);
        else
            from  := NO_PROCESS;
            msg   := NULL_MESSAGE;
            found := False;
        end if;

        Spinlocks.exitCriticalSection (mailtab(receiver).lock);
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
            -- Path 1: receiver already waiting. Dequeue them.
            -- Message already in proctab(pid).sendMsg; just record
            -- sender identity. Skip redundant 48-byte mailbox copy.
            mailtab(dest).hasMsg  := True;
            mailtab(dest).sender  := pid;

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
        ok : Boolean;
    begin
        -- Validate destination
        if dest = NO_PROCESS then
            return;
        end if;

        if proctab(dest).state = INVALID then
            return;
        end if;

        Spinlocks.enterCriticalSection (mailtab(dest).lock);

        enqueueEvent (dest, msg, ok);

        if proctab(dest).state = WAITINGFOREVENT then
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
        token : Unsigned_64;
        ok    : Boolean;
    begin
        -- Validate target
        if replyTo = NO_PROCESS then
            return 0;
        end if;

        if proctab(replyTo).state = INVALID then
            return 0;
        end if;

        if proctab(replyTo).state = WAITINGFORREPLY then
            -- Validate reply cap (userspace only; kernel threads are exempt)
            if proctab(mypid).mode = USER then
                validateReply : declare
                    cap        : Capabilities.Capability;
                    authorized : Boolean := False;
                    foundSlot  : Capabilities.CapabilitySlot :=
                        Capabilities.REPLY_CAP_SLOT;
                begin
                    -- Fast path: check well-known slot 63
                    cap := proctab(mypid).caps(
                        Capabilities.REPLY_CAP_SLOT);
                    if cap.capType = Capabilities.CAP_REPLY
                       and then cap.object.ref = Unsigned_64(replyTo)
                       and then cap.gen =
                           proctab(replyTo).capGeneration
                    then
                        authorized := True;
                    else
                        -- Slow path: iterate only deferred reply cap
                        -- slots via bitmap (avoids scanning all 64).
                        bitmapScan : declare
                            remaining : Unsigned_64 :=
                                proctab(mypid).deferredReplyCaps;
                            s : Natural;
                        begin
                            while remaining /= 0 loop
                                s := Util.getFirstSetBit (remaining);
                                cap := proctab(mypid).caps(s);
                                if cap.capType = Capabilities.CAP_REPLY
                                   and then cap.object.ref =
                                       Unsigned_64(replyTo)
                                   and then cap.gen =
                                       proctab(replyTo).capGeneration
                                then
                                    authorized := True;
                                    foundSlot := s;
                                    exit;
                                end if;
                                -- Clear lowest set bit
                                remaining :=
                                    remaining and (remaining - 1);
                            end loop;
                        end bitmapScan;
                    end if;

                    if not authorized then
                        return 0;
                    end if;

                    -- Consume (one-use) and clear bitmap bit
                    proctab(mypid).caps(foundSlot) :=
                        Capabilities.NULL_CAPABILITY;
                    proctab(mypid).deferredReplyCaps :=
                        proctab(mypid).deferredReplyCaps and
                        not Shift_Left (Unsigned_64'(1), foundSlot);
                end validateReply;
            end if;

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
                item    => (token => token,
                            msg   => msg,
                            from  => mypid,
                            valid => True),
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
    -- submit
    -- Non-blocking async send. Delivers message to dest's mailbox and
    -- returns immediately.  When token /= NO_COMPLETION_TOKEN, records
    -- (dest, token) in caller's pending array so that a later reply()
    -- can enqueue a CompletionEntry.  Fire-and-forget senders pass
    -- NO_COMPLETION_TOKEN to avoid leaking pending slots.
    ---------------------------------------------------------------------------
    function submit (dest  : ProcessID;
                     msg   : Message;
                     token : Unsigned_64) return Boolean
        with SPARK_Mode => On
    is
        pid      : constant ProcessID := PerCPUData.getCurrentPID;
        receiver : ProcessID;
        wantCompletion : constant Boolean :=
            (token /= NO_COMPLETION_TOKEN);
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

        Spinlocks.enterCriticalSection (mailtab(dest).lock);

        -- Check mailbox not full (single-slot)
        if mailtab(dest).hasMsg then
            Spinlocks.exitCriticalSection (mailtab(dest).lock);
            return False;
        end if;

        -- Deposit message in mailbox
        mailtab(dest).hasMsg  := True;
        mailtab(dest).msg     := msg;
        mailtab(dest).sender  := pid;

        -- Record pending request only when a completion is expected
        if wantCompletion then
            proctab(pid).pendingRequests(proctab(pid).numPending) :=
                (dest => dest, token => token);
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
        -- Initialize output (entries is user memory, needs STAC/CLAC)
        x86.stac;
        entries     := (others => NULL_COMPLETION);
        x86.clac;
        numReturned := 0;

        if mypid = NO_PROCESS then
            return;
        end if;

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
        pid     : constant ProcessID := PerCPUData.getCurrentPID;
        destPID : Unsigned_64;
        badge   : Capabilities.Badge;
        status  : Capabilities.Operations.OperationStatus;
        stamped : Message := msg;
    begin
        Capabilities.Operations.resolveEndpoint (
            table   => proctab(pid).caps,
            slot    => capSlot,
            rights  => Capabilities.READ_WRITE,
            destPID => destPID,
            capBadge => badge,
            status  => status);

        if status /= Capabilities.Operations.OP_OK then
            return NULL_TAG;
        end if;

        if destPID > Unsigned_64(ProcessID'Last) then
            return NULL_TAG;
        end if;

        -- Generation check: stale cap if gen doesn't match target
        if proctab(pid).caps(capSlot).gen /=
           proctab(ProcessID(destPID)).capGeneration
        then
            return NULL_TAG;
        end if;

        stamped.capBadge := badge;
        return send (dest => ProcessID(destPID), msg => stamped);
    end capSend;

    ---------------------------------------------------------------------------
    -- capCall
    ---------------------------------------------------------------------------
    function capCall (capSlot : Capabilities.CapabilitySlot;
                      msg     : Message) return MessageTag
        with SPARK_Mode => On
    is
        pid     : constant ProcessID := PerCPUData.getCurrentPID;
        destPID : Unsigned_64;
        badge   : Capabilities.Badge;
        status  : Capabilities.Operations.OperationStatus;
        stamped : Message := msg;
    begin
        Capabilities.Operations.resolveEndpoint (
            table   => proctab(pid).caps,
            slot    => capSlot,
            rights  => Capabilities.READ_WRITE,
            destPID => destPID,
            capBadge => badge,
            status  => status);

        if status /= Capabilities.Operations.OP_OK then
            return NULL_TAG;
        end if;

        if destPID > Unsigned_64(ProcessID'Last) then
            return NULL_TAG;
        end if;

        -- Generation check: stale cap if gen doesn't match target
        if proctab(pid).caps(capSlot).gen /=
           proctab(ProcessID(destPID)).capGeneration
        then
            return NULL_TAG;
        end if;

        stamped.capBadge := badge;
        return send (dest => ProcessID(destPID), msg => stamped);
    end capCall;

    ---------------------------------------------------------------------------
    -- capSubmit
    ---------------------------------------------------------------------------
    function capSubmit (capSlot : Capabilities.CapabilitySlot;
                        msg     : Message;
                        token   : Unsigned_64) return Boolean
        with SPARK_Mode => On
    is
        pid     : constant ProcessID := PerCPUData.getCurrentPID;
        destPID : Unsigned_64;
        badge   : Capabilities.Badge;
        status  : Capabilities.Operations.OperationStatus;
        stamped : Message := msg;
    begin
        Capabilities.Operations.resolveEndpoint (
            table   => proctab(pid).caps,
            slot    => capSlot,
            rights  => Capabilities.READ_WRITE,
            destPID => destPID,
            capBadge => badge,
            status  => status);

        if status /= Capabilities.Operations.OP_OK then
            return False;
        end if;

        if destPID > Unsigned_64(ProcessID'Last) then
            return False;
        end if;

        -- Generation check: stale cap if gen doesn't match target
        if proctab(pid).caps(capSlot).gen /=
           proctab(ProcessID(destPID)).capGeneration
        then
            return False;
        end if;

        stamped.capBadge := badge;
        return submit (dest  => ProcessID(destPID),
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

        Spinlocks.enterCriticalSection (
            mailtab(ProcessID(destPID)).lock);

        -- OR badge into notification word
        mailtab(ProcessID(destPID)).notifyWord :=
            mailtab(ProcessID(destPID)).notifyWord or cap.capBadge;

        -- Wake if blocked in notifyWait
        if mailtab(ProcessID(destPID)).notifyWaiter then
            mailtab(ProcessID(destPID)).notifyWaiter := False;
            notify (ProcessID(destPID));
        -- Also wake if a process bound this notification and is blocked
        -- in receive(). boundReceiver tracks which process called
        -- bindNotification on this notification PID.
        elsif mailtab(ProcessID(destPID)).boundReceiver /= NO_PROCESS
        then
            wakeRecv : declare
                recvPID : constant ProcessID :=
                    mailtab(ProcessID(destPID)).boundReceiver;
                ignore  : ProcessID;
                recv    : constant ProcessID := getReceiver (recvPID);
            begin
                if proctab(recvPID).state = RECEIVING then
                    Queues.popItem (
                        mailtab(recv).recvQueue, recvPID, ignore);
                    -- Deliver notification as synthetic message
                    mailtab(recv).hasMsg  := True;
                    mailtab(recv).msg    := (
                        tag      => (label  => 0,
                                     length => 1,
                                     flags  => 0,
                                     badge  => 0),
                        capBadge => 0,
                        words    => (
                            0 => mailtab(ProcessID(destPID)).notifyWord,
                            others => 0));
                    mailtab(recv).sender := NO_PROCESS;
                    mailtab(ProcessID(destPID)).notifyWord := 0;
                    notify (recvPID);
                end if;
            end wakeRecv;
        end if;

        Spinlocks.exitCriticalSection (
            mailtab(ProcessID(destPID)).lock);

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
    procedure bindNotification (notifPID : ProcessID)
        with SPARK_Mode => On
    is
        mypid : constant ProcessID := PerCPUData.getCurrentPID;
    begin
        if mypid = NO_PROCESS then
            return;
        end if;

        if notifPID = NO_PROCESS then
            return;
        end if;

        proctab(mypid).boundNotification := notifPID;
        mailtab(notifPID).boundReceiver  := mypid;
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
            mailtab(old).boundReceiver := NO_PROCESS;
        end if;
    end unbindNotification;

end Process.IPC;
