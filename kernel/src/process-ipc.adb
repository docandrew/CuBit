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
with PerCPUData;
with Process.Queues;
with Virtmem;

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

        -- Check sendQueue FIRST. When send() doesn't find a waiting
        -- receiver, it deposits the message (hasMsg=True) AND enqueues
        -- in sendQueue. We must dequeue and advance sender state.
        if not Queues.isEmpty (mailtab(receiver).sendQueue) then
            Queues.dequeue (mailtab(receiver).sendQueue, sender);

            msg  := mailtab(receiver).msg;
            from := mailtab(receiver).sender;
            mailtab(receiver).hasMsg := False;

            proctab(sender).state := WAITINGFORREPLY;

            Spinlocks.exitCriticalSection (mailtab(receiver).lock);
            return;
        end if;

        if mailtab(receiver).hasMsg then
            -- Message delivered directly (sender already WAITINGFORREPLY).
            msg  := mailtab(receiver).msg;
            from := mailtab(receiver).sender;
            mailtab(receiver).hasMsg := False;

            Spinlocks.exitCriticalSection (mailtab(receiver).lock);
            return;
        end if;

        -- No message and no sender waiting. Block as a receiver.
        proctab(mypid).queueKey := receiver;
        Queues.enqueue (mailtab(receiver).recvQueue, mypid, ignore);
        proctab(mypid).state := RECEIVING;

        Spinlocks.exitCriticalSection (mailtab(receiver).lock);

        yield;

        -- Woken up by a sender's send() call. The sender placed the message
        -- in the mailbox and set us to READY before we got here.
        Spinlocks.enterCriticalSection (mailtab(receiver).lock);

        msg  := mailtab(receiver).msg;
        from := mailtab(receiver).sender;
        mailtab(receiver).hasMsg := False;

        -- The sender is already in WAITINGFORREPLY (set by send)
        Spinlocks.exitCriticalSection (mailtab(receiver).lock);
    end receive;

    ---------------------------------------------------------------------------
    -- receiveEvent
    ---------------------------------------------------------------------------
    function receiveEvent return Message with SPARK_Mode => On is
        mypid    : constant ProcessID := PerCPUData.getCurrentPID;
        receiver : constant ProcessID := getReceiver (mypid);
        event    : Message;
    begin
        Spinlocks.enterCriticalSection (mailtab(receiver).lock);

        if not mailtab(receiver).hasEvent then
            proctab(mypid).state := WAITINGFOREVENT;
            Spinlocks.exitCriticalSection (mailtab(receiver).lock);

            yield;

            Spinlocks.enterCriticalSection (mailtab(receiver).lock);
        end if;

        event := mailtab(receiver).eventMsg;
        mailtab(receiver).hasEvent := False;

        Spinlocks.exitCriticalSection (mailtab(receiver).lock);

        return event;
    end receiveEvent;

    ---------------------------------------------------------------------------
    -- receiveNB
    -- Non-blocking receive. Checks for a pending message or queued sender.
    ---------------------------------------------------------------------------
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

        -- Check sendQueue FIRST. When send() doesn't find a waiting
        -- receiver, it deposits the message (hasMsg=True) AND enqueues
        -- in sendQueue. We must dequeue the sender and advance their
        -- state to WAITINGFORREPLY so that reply() can wake them.
        if not Queues.isEmpty (mailtab(receiver).sendQueue) then
            Queues.dequeue (mailtab(receiver).sendQueue, sender);

            msg   := mailtab(receiver).msg;
            from  := mailtab(receiver).sender;
            mailtab(receiver).hasMsg := False;
            found := True;

            proctab(sender).state := WAITINGFORREPLY;

        elsif mailtab(receiver).hasMsg then
            -- Message delivered directly (sender already WAITINGFORREPLY)
            msg   := mailtab(receiver).msg;
            from  := mailtab(receiver).sender;
            mailtab(receiver).hasMsg := False;
            found := True;
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
    --   state to WAITINGFORREPLY. Then reply() calls inform() which adds
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

        Spinlocks.enterCriticalSection (mailtab(dest).lock);

        -- Deposit our message in the mailbox regardless of path
        mailtab(dest).hasMsg  := True;
        mailtab(dest).msg := msg;
        mailtab(dest).sender  := pid;

        if not Queues.isEmpty (mailtab(dest).recvQueue) then
            -- Path 1: receiver already waiting. Dequeue and wake them.
            Queues.dequeue (mailtab(dest).recvQueue, receiver);

            -- Sender goes to WAITINGFORREPLY
            proctab(pid).state := WAITINGFORREPLY;

            -- Remove receiver from RECEIVING state, make READY
            ready (receiver);

            Spinlocks.exitCriticalSection (mailtab(dest).lock);
        else
            -- Path 2: no receiver yet. Enqueue ourselves as a sender.
            proctab(pid).queueKey := dest;
            Queues.enqueue (mailtab(dest).sendQueue, pid, ignore);
            proctab(pid).state := SENDING;

            Spinlocks.exitCriticalSection (mailtab(dest).lock);
        end if;

        -- Single yield per path:
        -- Path 1: WAITINGFORREPLY — woken by reply() calling inform()
        -- Path 2: SENDING — receiver sets us to WAITINGFORREPLY,
        --         then reply() calls inform() to wake us
        yield;

        -- Reply delivered — replyMsg populated by reply()
        replyTag := proctab(pid).replyMsg.tag;

        return replyTag;
    end send;

    ---------------------------------------------------------------------------
    -- sendEvent
    -- Non-blocking send for interrupt context. Does not block the caller.
    ---------------------------------------------------------------------------
    procedure sendEvent (dest : ProcessID; msg : Message)
        with SPARK_Mode => On is
    begin
        -- Validate destination
        if dest = NO_PROCESS then
            return;
        end if;

        if proctab(dest).state = INVALID then
            return;
        end if;

        Spinlocks.enterCriticalSection (mailtab(dest).lock);

        mailtab(dest).hasEvent := True;
        mailtab(dest).eventMsg    := msg;
        mailtab(dest).eventSender := dest;

        if proctab(dest).state = WAITINGFOREVENT then
            inform (dest);
        end if;

        Spinlocks.exitCriticalSection (mailtab(dest).lock);
    end sendEvent;

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
            -- SYNC PATH (unchanged): store reply in proctab, wake sender
            proctab(replyTo).replyMsg := msg;
            inform (replyTo);
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
                inform (replyTo);
            end if;
        end if;

        return 1;
    end reply;

    ---------------------------------------------------------------------------
    -- submit
    -- Non-blocking async send. Delivers message to dest's mailbox and
    -- returns immediately. Records (dest, token) in caller's pending array.
    ---------------------------------------------------------------------------
    function submit (dest  : ProcessID;
                     msg   : Message;
                     token : Unsigned_64) return Boolean
        with SPARK_Mode => On
    is
        pid      : constant ProcessID := PerCPUData.getCurrentPID;
        receiver : ProcessID;
    begin
        -- Validate destination
        if dest = NO_PROCESS then
            return False;
        end if;

        if proctab(dest).state = INVALID then
            return False;
        end if;

        -- Check we have room for another pending request
        if proctab(pid).numPending >= MAX_PENDING_ASYNC then
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

        -- Record pending request (for reply() to find the token)
        proctab(pid).pendingRequests(proctab(pid).numPending) :=
            (dest => dest, token => token);
        proctab(pid).numPending := proctab(pid).numPending + 1;

        -- Wake receiver if one is waiting
        if not Queues.isEmpty (mailtab(dest).recvQueue) then
            Queues.dequeue (mailtab(dest).recvQueue, receiver);
            ready (receiver);
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
        with SPARK_Mode => On
    is
        mypid    : constant ProcessID := PerCPUData.getCurrentPID;
        receiver : constant ProcessID := getReceiver (mypid);
        drained  : Natural := 0;
        item     : CompletionEntry;
        ok       : Boolean;
        effectiveMax : Natural;
        effectiveMin : Natural;
    begin
        -- Initialize output
        entries     := (others => NULL_COMPLETION);
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
                -- Drain up to effectiveMax entries
                while drained < effectiveMax loop
                    dequeueCompletion (receiver, item, ok);
                    exit when not ok;
                    entries(drained) := item;
                    drained := drained + 1;
                end loop;

                Spinlocks.exitCriticalSection (mailtab(receiver).lock);
                numReturned := drained;
                return;
            end if;

            -- Not enough completions yet — block
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
                           id        : out GrantID;
                           success   : out Boolean)
        with SPARK_Mode => On
    is
        pid      : constant ProcessID := PerCPUData.getCurrentPID;
        physAddr : Virtmem.PhysAddress;
        granteeVirt : Integer_Address;
        flags    : Unsigned_64;
        ok       : Boolean;
        slotFound : Boolean := False;

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

        -- Find a free grant slot
        for i in GrantID loop
            if not proctab(pid).grants(i).active then
                id        := i;
                slotFound := True;
                exit;
            end if;
        end loop;

        if not slotFound then
            return;
        end if;

        -- Determine permission flags
        if perm = GRANT_READWRITE then
            flags := Virtmem.PG_USERDATA;
        else
            flags := Virtmem.PG_USERDATARO;
        end if;

        -- Map each page from granter's space into grantee's space
        for i in 0 .. numPages - 1 loop
            -- Look up physical address behind granter's virtual address
            physAddr := Virtmem.tableWalk (
                virt => To_Integer (localAddr) +
                        Integer_Address (i) * Integer_Address (Virtmem.PAGE_SIZE),
                myP4 => addrtab(proctab(pid).pgTable));

            if physAddr = 0 then
                -- Page not mapped in granter's space — roll back
                for j in 0 .. i - 1 loop
                    granteeVirt := GRANT_REGION_BASE +
                        Integer_Address (id) * GRANT_SLOT_SIZE +
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
                Integer_Address (id) * GRANT_SLOT_SIZE +
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
                        Integer_Address (id) * GRANT_SLOT_SIZE +
                        Integer_Address (j) * Integer_Address (Virtmem.PAGE_SIZE);

                    Virtmem.unmapPage (
                        virt => granteeVirt,
                        myP4 => addrtab(proctab(grantee).pgTable),
                        success => ok);
                end loop;
                return;
            end if;
        end loop;

        -- Record grant metadata
        proctab(pid).grants(id) := (
            active      => True,
            granterPID  => pid,
            granteePID  => grantee,
            granterAddr => localAddr,
            granteeAddr => To_Address (
                GRANT_REGION_BASE +
                Integer_Address (id) * GRANT_SLOT_SIZE),
            numPages    => numPages,
            permission  => perm
        );

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
        granteeVirt : Integer_Address;
        ok    : Boolean;
        g     : Grant renames proctab(pid).grants(id);
    begin
        if not g.active then
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

        -- Invalidate TLB for grantee if they have a valid page table
        -- (Full TLB shootdown via IPI is future work — for now we flush
        -- the local TLB if grantee happens to be current)
        if PerCPUData.getCurrentPID = g.granteePID then
            Virtmem.setActiveP4 (
                Virtmem.K2P (addrtab(proctab(g.granteePID).pgTable)'Address));
        end if;

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

end Process.IPC;
