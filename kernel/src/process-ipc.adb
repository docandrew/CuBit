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
with Call_Sequences;
with Process_Identities;
with Capabilities.Operations;
with Config;
with Grant_Page_Installation;
with IPC_Labels;
with IPI;
with Kernel_Reports;
with Memory_Grants;
with Memory_Grants.Loans;
with Retained_Record_Blocks;
with System.Storage_Elements;
with System.Address_To_Access_Conversions;
with PerCPUData;
with Process.Queues;
with Process.User_Memory;
with Process.DMA;
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

    -- Empty every entry in place (a whole-ring aggregate would be a 5.8 KiB
    -- temporary on the kernel stack).
    procedure clearCompletionSlots (slots : in out CompletionSlots) is
    begin
        for i in slots.ring'Range loop
            slots.ring (i) := NULL_COMPLETION;
            slots.owners (i) := NO_THREAD;
        end loop;
    end clearCompletionSlots;

    -- pid's completion entries exist (allocating them on its first async
    -- submit). Caller holds mailtab(pid).lock. False: out of memory, and
    -- the submit is refused.
    package Completion_Conversion is
      new System.Address_To_Access_Conversions (CompletionSlots);

    function ensureCompletionSlots (pid : ProcessID) return Boolean is
        cq : CompletionQueue renames completionTab(pid);
        bytes : constant Storage_Count :=
          CompletionSlots'Object_Size / System.Storage_Unit;
        block : System.Address;
    begin
        if cq.slots /= null then
            return True;
        end if;
        BuddyAllocator.alloc (BuddyAllocator.getOrder (bytes), block);
        if block = System.Null_Address then
            return False;
        end if;
        cq.slots := CompletionSlotsAccess (Completion_Conversion.To_Pointer (block));
        clearCompletionSlots (cq.slots.all);
        return True;
    end ensureCompletionSlots;

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

        cq.slots.ring(cq.tail) := item;
        cq.slots.owners(cq.tail) := thread;
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
            if cq.slots.owners(idx) = thread or else
               (not strict and then cq.slots.owners(idx) = NO_THREAD)
            then
                item := cq.slots.ring(idx);
                for j in i .. cq.count - 2 loop
                    idx := (cq.head + j) mod COMPLETION_QUEUE_SIZE;
                    following := (idx + 1) mod COMPLETION_QUEUE_SIZE;
                    cq.slots.ring(idx) := cq.slots.ring(following);
                    cq.slots.owners(idx) := cq.slots.owners(following);
                end loop;
                cq.tail := (cq.tail + COMPLETION_QUEUE_SIZE - 1) mod COMPLETION_QUEUE_SIZE;
                cq.slots.ring(cq.tail) := NULL_COMPLETION;
                cq.slots.owners(cq.tail) := NO_THREAD;
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
            if cq.slots.owners(idx) = thread or else cq.slots.owners(idx) = NO_THREAD then
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
         callSeq      : out Unsigned_64;
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
        callSeq     := 0;
        ok          := False;

        if threadtab (callerThread).mode = KERNEL then
            replyThread := mainThreadOf (replyTo);
            callSeq := threadtab (replyThread).callSequence;
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
                callSeq := cap.authorityTag;
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
            callSeq := cap.authorityTag;
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
           senderThread => NO_THREAD, publisher => NO_PROCESS, callSequence => 0);
        success := True;
    end takeIRQDoorbell;

    ---------------------------------------------------------------------------
    -- A receiver's queue (IPC-002 step 3, docs/ipc-delivery.md): two
    -- classes (requests; published events), each a ring of QUEUE_CREDIT
    -- entries per sending process (Kernel_Credits), allocated when first
    -- needed. A flooding sender fills only its own ring and is told so; it
    -- never takes another sender's room. Callers hold mailtab(owner).lock.
    ---------------------------------------------------------------------------

    use type System.Address;
    use type BuddyAllocator.Order;
    package Credit_States is new System.Address_To_Access_Conversions
      (Kernel_Credits.Receiver_State);
    subtype Credit_State is Credit_States.Object_Pointer;
    use type Credit_State;

    type Entry_Chunk is array (0 .. SENDERS_PER_CHUNK * QUEUE_CREDIT - 1) of RingEntry;
    package Entry_Chunks is new System.Address_To_Access_Conversions (Entry_Chunk);

    Page_Bytes : constant := 4096;

    -- The smallest buddy order holding bytes.
    function orderFor (bytes : Natural) return BuddyAllocator.Order is
        ord : BuddyAllocator.Order := 0;
    begin
        while Page_Bytes * 2 ** Natural (ord) < bytes loop
            ord := ord + 1;
        end loop;
        return ord;
    end orderFor;

    State_Order : constant BuddyAllocator.Order :=
      orderFor (Kernel_Credits.Receiver_State'Max_Size_In_Storage_Elements);
    Chunk_Order : constant BuddyAllocator.Order :=
      orderFor (Entry_Chunk'Max_Size_In_Storage_Elements);

    function classOf (kind : RingEntryKind) return Queue_Class is
      (if kind = RING_EVENT then Event_Class else Request_Class);

    function stateOf (owner : ProcessID; class : Queue_Class) return Credit_State is
      (if mailtab(owner).queues (class).state = System.Null_Address then null
       else Credit_States.To_Pointer (mailtab(owner).queues (class).state));

    -- The entry for sender's ring at position.
    function entryAt
      (owner : ProcessID; class : Queue_Class; sender : Kernel_Credits.Sender;
       position : Kernel_Credits.Position) return Entry_Chunks.Object_Pointer
    is
      (Entry_Chunks.To_Pointer
         (mailtab(owner).queues (class).chunks ((sender - 1) / SENDERS_PER_CHUNK)));

    function entryIndex
      (sender : Kernel_Credits.Sender; position : Kernel_Credits.Position) return Natural is
      (((sender - 1) mod SENDERS_PER_CHUNK) * QUEUE_CREDIT + position);

    -- The class's state and sender's chunk, allocated if absent.
    procedure ensureStorage
      (owner : ProcessID; class : Queue_Class; sender : Kernel_Credits.Sender;
       ok : out Boolean)
    is
        q : Class_Queue renames mailtab(owner).queues (class);
        chunk : constant Natural := (sender - 1) / SENDERS_PER_CHUNK;
        block : System.Address;
    begin
        ok := False;
        if q.state = System.Null_Address then
            BuddyAllocator.alloc (State_Order, block);
            if block = BuddyAllocator.NO_BLOCK_AVAILABLE then
                return;
            end if;
            Kernel_Credits.Initialize (Credit_States.To_Pointer (block).all, QUEUE_CREDIT);
            q.state := block;
        end if;
        if q.chunks (chunk) = System.Null_Address then
            BuddyAllocator.alloc (Chunk_Order, block);
            if block = BuddyAllocator.NO_BLOCK_AVAILABLE then
                return;
            end if;
            q.chunks (chunk) := block;
        end if;
        ok := True;
    end ensureStorage;

    -- Anything queued for owner, in either class.
    function queued (owner : ProcessID) return Boolean is
      ((for some c in Queue_Class =>
          stateOf (owner, c) /= null and then not Kernel_Credits.Empty (stateOf (owner, c).all)));

    -- Nothing pending for owner's receive but what a sender brings: no
    -- queued messages, kernel notices, IRQ doorbell or blocked senders.
    -- Then its receive would take a new message first, so a call may be
    -- handed to a waiting receiver directly (IPC-003, docs/ipc-fastpath.md).
    -- Caller holds mailtab(owner).lock.
    function mailboxIdle (owner : ProcessID) return Boolean is
      (not queued (owner) and then
       not proctab(owner).kernelNoticePending and then
       not proctab(owner).irqNotificationPending and then
       Queues.isEmpty (mailtab(owner).sendQueue));

    -- Free owner's queue storage (its mailbox is closed and drained).
    procedure freeQueues (owner : ProcessID) is
    begin
        for c in Queue_Class loop
            declare
                q : Class_Queue renames mailtab(owner).queues (c);
            begin
                for chunk of q.chunks loop
                    if chunk /= System.Null_Address then
                        BuddyAllocator.free (Chunk_Order, chunk);
                        chunk := System.Null_Address;
                    end if;
                end loop;
                if q.state /= System.Null_Address then
                    BuddyAllocator.free (State_Order, q.state);
                    q.state := System.Null_Address;
                end if;
            end;
        end loop;
        mailtab(owner).nextClass := Request_Class;
    end freeQueues;

    -- sender ended: its rings at owner go.
    procedure forgetSender (owner : ProcessID; sender : ProcessID) is
    begin
        if sender = NO_PROCESS then
            return;
        end if;
        for c in Queue_Class loop
            if stateOf (owner, c) /= null then
                Kernel_Credits.Forget (stateOf (owner, c).all, Kernel_Credits.Sender (sender));
            end if;
        end loop;
    end forgetSender;

    ---------------------------------------------------------------------------
    -- enqueueRing
    -- Admit an entry into owner's queue, in its sender's ring.
    -- @return True if kept, False if refused: the mailbox is closed, the
    -- sender's ring is full, or no memory (the sender keeps its message).
    ---------------------------------------------------------------------------
    procedure enqueueRing (owner   : in  ProcessID;
                           item    : in  RingEntry;
                           success : out Boolean)
    is
        use type Kernel_Credits.Admit_Result;
        class : constant Queue_Class := classOf (item.kind);
        account : constant ProcessID :=
          (if item.kind = RING_EVENT then item.publisher else item.sender);
        position : Kernel_Credits.Position;
        result : Kernel_Credits.Admit_Result;
        ok : Boolean;
    begin
        success := False;
        -- Kernel events never use the queue (kernel notices).
        if mailtab(owner).closed or else account = NO_PROCESS then
            return;
        end if;
        ensureStorage (owner, class, Kernel_Credits.Sender (account), ok);
        if not ok then
            return;
        end if;
        Kernel_Credits.Admit
          (stateOf (owner, class).all, Kernel_Credits.Sender (account),
           Unsigned_64 (generationOf (account)), position, result);
        if result /= Kernel_Credits.Admitted then
            return;
        end if;
        entryAt (owner, class, Kernel_Credits.Sender (account), position).all
          (entryIndex (Kernel_Credits.Sender (account), position)) := item;
        success := True;
    end enqueueRing;

    -- The next entry of one class, round-robin across its senders.
    procedure takeClass (owner   : in  ProcessID;
                         class   : in  Queue_Class;
                         item    : out RingEntry;
                         success : out Boolean)
    is
        state : constant Credit_State := stateOf (owner, class);
        sender : Kernel_Credits.Sender;
        position : Kernel_Credits.Position;
    begin
        item := NULL_RING_ENTRY;
        success := False;
        if state = null then
            return;
        end if;
        Kernel_Credits.Take (state.all, sender, position, success);
        if success then
            declare
                chunk : constant Entry_Chunks.Object_Pointer :=
                  entryAt (owner, class, sender, position);
                index : constant Natural := entryIndex (sender, position);
            begin
                item := chunk (index);
                chunk (index) := NULL_RING_ENTRY;
            end;
        end if;
    end takeClass;

    ---------------------------------------------------------------------------
    -- dequeueRing
    -- The next entry of either class; mixed receive alternates classes.
    ---------------------------------------------------------------------------
    procedure dequeueRing (owner   : in  ProcessID;
                           item    : out RingEntry;
                           success : out Boolean)
    is
        first : constant Queue_Class := mailtab(owner).nextClass;
        other : constant Queue_Class :=
          (if first = Request_Class then Event_Class else Request_Class);
    begin
        takeClass (owner, first, item, success);
        if not success then
            takeClass (owner, other, item, success);
        end if;
        if success then
            mailtab(owner).nextClass := other;
        end if;
    end dequeueRing;

    ---------------------------------------------------------------------------
    -- dequeueRingKind
    -- The next entry of kind's class (only published events are taken this
    -- way; a request kind takes the request class).
    ---------------------------------------------------------------------------
    procedure dequeueRingKind (owner   : in  ProcessID;
                               kind    : in  RingEntryKind;
                               item    : out RingEntry;
                               success : out Boolean)
    is
    begin
        takeClass (owner, classOf (kind), item, success);
    end dequeueRingKind;

    ---------------------------------------------------------------------------
    -- dequeueRingServiceRequest
    -- The next request (RING_SYNC, RING_ASYNC_REQUEST, RING_ONEWAY): service
    -- code does not consume published events while polling for client work.
    ---------------------------------------------------------------------------
    procedure dequeueRingServiceRequest (owner   : in  ProcessID;
                                         item    : out RingEntry;
                                         success : out Boolean)
    is
    begin
        takeClass (owner, Request_Class, item, success);
    end dequeueRingServiceRequest;

    -- One of receiver's kernel notices, if its doorbell rang (grant ends,
    -- exit and fault reports). Called with the receiver mailbox locked
    -- (mailbox, then grant and report locks: docs/kernel-locking.md).
    procedure takeKernelNotice
      (receiver : ProcessID; item : out RingEntry; found : out Boolean);

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
                                 senderThread => sender, publisher => NO_PROCESS,
                                 callSequence => threadtab (sender).callSequence);
                        threadtab (sender).state := WAITINGFORREPLY;
                        found := True;
                    end if;
                when IRQ_Doorbell =>
                    if not serviceOnly then
                        takeIRQDoorbell (receiver, item, found);
                    end if;
                when Kernel_Notices =>
                    if not serviceOnly then
                        takeKernelNotice (receiver, item, found);
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
            -- A synchronous reply capability names the call it answers in
            -- authorityTag (unused for replies): its sender's call sequence.
            threadtab (me).replyCap :=
                (capType => Capabilities.CAP_REPLY,
                 rights => Capabilities.ALL_RIGHTS,
                 authorityTag => item.callSequence,
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
            if queued (receiver) or else
               not Queues.isEmpty (mailtab(receiver).sendQueue) or else
               proctab(receiver).irqNotificationPending or else
               proctab(receiver).kernelNoticePending or else
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

    -- After a wake: a call handed over directly comes first (it was the
    -- only thing pending when it arrived), then the general selection.
    -- Caller holds mailtab(receiver).lock.
    procedure takeHandoffOrWork
      (me : ThreadID; receiver : ProcessID; item : out RingEntry; found : out Boolean) is
    begin
        if threadtab (me).handoffValid then
            item := threadtab (me).handoffItem;
            threadtab (me).handoffValid := False;
            threadtab (me).handoffItem := NULL_RING_ENTRY;
            found := True;
        else
            takeMailboxWork (receiver, False, item, found);
        end if;
    end takeHandoffOrWork;

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
        takeHandoffOrWork (me, receiver, re, received);
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
    -- A synchronous call whose deadline passed (docs/ipc-fastpath.md, "Call
    -- deadlines"): its caller wakes with REPLY_TIMEOUT. Taken under the lock
    -- its other outcome uses, so a reply and a timeout cannot both win:
    -- still queued as a sender, the server's mailbox (the server never sees
    -- the request); waiting for the reply, the caller's own (as reply
    -- delivery locks it).
    procedure expireCall (tid : ThreadID; nowMs : Unsigned_64) is
        caller  : constant ProcessID := processOf (tid);
        server  : ProcessID;
        removed : ThreadID;

        function due return Boolean is
          (threadtab (tid).callDeadlineActive and then
           threadtab (tid).callDeadlineMs <= nowMs);

        procedure timeOut is
        begin
            threadtab (tid).replyMsg :=
              (tag => (label => IPC_Labels.REPLY_TIMEOUT, length => 0, flags => 0, reserved => 0),
               authorityTag => 0, words => (others => 0));
            threadtab (tid).callDeadlineActive := False;
            ready (tid);
        end timeOut;
    begin
        if threadtab (tid).state = SENDING then
            -- queueKey names the server while SENDING (other states use it
            -- for scheduler keys): check its range before using it.
            if threadtab (tid).queueKey not in 1 .. Integer (ProcessID'Last) then
                return;
            end if;
            server := ProcessID (threadtab (tid).queueKey);
            Spinlocks.enterCriticalSection (mailtab(server).lock);
            Spinlocks.enterCriticalSection (lock);
            if threadtab (tid).state = SENDING and then due and then
               ProcessID (threadtab (tid).queueKey) = server
            then
                Queues.popItem (mailtab(server).sendQueue, tid, removed);
                if removed = tid then
                    timeOut;
                end if;
            end if;
            Spinlocks.exitCriticalSection (lock);
            Spinlocks.exitCriticalSection (mailtab(server).lock);
        elsif threadtab (tid).state = WAITINGFORREPLY and then caller /= NO_PROCESS then
            Spinlocks.enterCriticalSection (mailtab(caller).lock);
            Spinlocks.enterCriticalSection (lock);
            if threadtab (tid).state = WAITINGFORREPLY and then due then
                timeOut;
            end if;
            Spinlocks.exitCriticalSection (lock);
            Spinlocks.exitCriticalSection (mailtab(caller).lock);
        end if;
    end expireCall;

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
            if threadtab (tid).callDeadlineActive and then
               threadtab (tid).callDeadlineMs <= nowMs
            then
                expireCall (tid, nowMs);
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

            takeKernelNotice (receiver, re, ok);
            if not ok then
                takeIRQDoorbell (receiver, re, ok);
            end if;
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

        takeKernelNotice (receiver, re, found);
        if not found then
            takeIRQDoorbell (receiver, re, found);
        end if;
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
    -- Defined with reply, below.
    function completeReplyLocked
      (replyTo : ProcessID; replyThread : ThreadID;
       requestId : Unsigned_64; callSeq : Unsigned_64; msg : Message)
      return Unsigned_64;

    procedure replyWait (replyTo : in ProcessID; replyMsg : in Message;
                         from : out ProcessID; msg : out Message) is
        mypid    : constant ProcessID := PerCPUData.getCurrentPID;
        me       : constant ThreadID := PerCPUData.getCurrentThread;
        receiver : constant ProcessID := getReceiver (mypid);
        requestId : Unsigned_64;
        callSeq : Unsigned_64;
        replyThread : ThreadID;
        ok : Boolean;
        ignored : Unsigned_64;
        ignoreT : ThreadID;
        re : RingEntry;
        received : Boolean;
    begin
        -- Labels only the kernel gives are refused before anything is
        -- consumed: the server keeps its authority and can reply properly.
        if IPC_Labels.Is_Kernel_Reply (replyMsg.tag.label) then
            receive (from, msg);
            return;
        end if;
        if replyTo = NO_PROCESS or else mypid = NO_PROCESS or else
           receiver /= mypid or else replyTo = mypid
        then
            if replyTo /= NO_PROCESS then
                ignored := reply (replyTo, replyMsg);
            end if;
            receive (from, msg);
            return;
        end if;

        -- The same generation-checked, mailbox-locked reply as reply().
        lockMailboxes (mypid, replyTo);
        if mailtab(replyTo).closed then
            unlockMailboxes (mypid, replyTo);
            receive (from, msg);
            return;
        end if;
        consumeReplyAuthority (mypid, me, replyTo, replyThread, requestId, callSeq, ok);
        if not ok then
            unlockMailboxes (mypid, replyTo);
            receive (from, msg);
            return;
        end if;

        -- Fast path (IPC-003, docs/ipc-fastpath.md): a synchronous reply to
        -- a caller waiting on this CPU, and nothing pending for this server,
        -- so its receive would block. Wait in receive and switch to the
        -- caller in one step, instead of making this thread runnable only
        -- for it to block again.
        if requestId = NO_REQUEST_ID and then replyThread /= NO_THREAD and then
           processOf (replyThread) = replyTo and then
           Call_Sequences.Accepts
             (threadtab (replyThread).state = WAITINGFORREPLY,
              threadtab (replyThread).callSequence, callSeq) and then
           threadtab (replyThread).cpu = PerCPUData.getCPUNumber and then
           not mailtab(mypid).closed and then mailboxIdle (mypid)
        then
            -- Register the wait, as receiveInternal does before blocking.
            threadtab (me).queueKey := receiver;
            threadtab (me).receiveDeadlineMs := 0;
            threadtab (me).receiveDeadlineReceiver := receiver;
            threadtab (me).receiveDeadlineActive := False;
            Queues.enqueue (mailtab(receiver).recvQueue, me, ignoreT);
            threadtab (me).state := RECEIVING;

            Spinlocks.enterCriticalSection (lock);
            threadtab (replyThread).replyMsg := replyMsg;
            unlockMailboxes (mypid, replyTo);
            directSwitch (me, replyThread);
            Spinlocks.exitCriticalSection (lock);

            -- Woken with work, as receiveInternal after its yield.
            Spinlocks.enterCriticalSection (mailtab(receiver).lock);
            threadtab (me).receiveDeadlineReceiver := NO_PROCESS;
            takeHandoffOrWork (me, receiver, re, received);
            installReceivedWork (me, re, received, from, msg);
            Spinlocks.exitCriticalSection (mailtab(receiver).lock);
            return;
        end if;

        -- General path: complete the reply, then receive.
        Spinlocks.exitCriticalSection (mailtab(mypid).lock);
        ignored := completeReplyLocked (replyTo, replyThread, requestId, callSeq, replyMsg);
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
                   expectedGeneration : Capabilities.Generation;
                   deadlineMs : Unsigned_64) return MessageTag

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

        -- Authority was checked by capSend, the only caller: the slot's
        -- read-write rights and current generation, which is re-checked
        -- above under this mailbox lock.

        -- Store our message in per-sender storage so it cannot be
        -- overwritten by another sender racing to the same destination.
        -- A new call: only an answer to this one is delivered
        -- (Call_Sequences). At the last sequence the thread makes no more
        -- calls: retired, never wrapped.
        if not Call_Sequences.Can_Begin (threadtab (me).callSequence) then
            Spinlocks.exitCriticalSection (mailtab(dest).lock);
            return NULL_TAG;
        end if;
        threadtab (me).sendMsg := msg;
        threadtab (me).callSequence := Call_Sequences.Next (threadtab (me).callSequence);
        -- How long the caller waits (docs/ipc-fastpath.md, "Call
        -- deadlines"); armed under this lock, before the caller blocks.
        if deadlineMs /= Unsigned_64'Last then
            threadtab (me).callDeadlineMs := deadlineMs;
            threadtab (me).callDeadlineActive := True;
        end if;

        if not Queues.isEmpty (mailtab(dest).recvQueue) then
            -- Path 1: receiver already waiting.
            enqueueP1 : declare
                item : constant RingEntry :=
                  (msg       => msg,
                   sender    => pid,
                   kind      => RING_SYNC,
                   requestId => NO_REQUEST_ID,
                   senderThread => me, publisher => NO_PROCESS,
                   callSequence => threadtab (me).callSequence);
                -- Fast path (IPC-003): nothing else is pending for dest, so
                -- its receive would take this message first anyway; hand it
                -- to the receiver directly instead of queueing it.
                idle : constant Boolean := mailboxIdle (dest);
                ok : Boolean;
            begin
                if not idle then
                    enqueueRing (dest, item, ok);
                    if not ok then
                        threadtab (me).callDeadlineActive := False;
                        Spinlocks.exitCriticalSection (mailtab(dest).lock);
                        return NULL_TAG;
                    end if;
                end if;

                -- A receiving thread on this CPU, if the destination has
                -- one, so the call can be a direct switch (else the longest
                -- waiter).
                Queues.dequeuePreferring
                  (mailtab(dest).recvQueue, PerCPUData.getCPUNumber, receiver);

                if idle then
                    if threadtab (receiver).waitsForIPCActivity then
                        -- An activity waiter takes nothing itself: queue the
                        -- message for the receive that follows its wake.
                        enqueueRing (dest, item, ok);
                        if not ok then
                            Queues.enqueue (mailtab(dest).recvQueue, receiver, ignore);
                            threadtab (me).callDeadlineActive := False;
                            Spinlocks.exitCriticalSection (mailtab(dest).lock);
                            return NULL_TAG;
                        end if;
                    else
                        threadtab (receiver).handoffItem := item;
                        threadtab (receiver).handoffValid := True;
                    end if;
                end if;
            end enqueueP1;

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

                -- Resumed: a reply delivered by reply(), or the deadline.
                threadtab (me).callDeadlineActive := False;
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

        -- A reply delivered by reply(), or the deadline (REPLY_TIMEOUT).
        threadtab (me).callDeadlineActive := False;
        replyTag := threadtab (me).replyMsg.tag;

        return replyTag;
    end send;

    ---------------------------------------------------------------------------
    -- trySendEvent
    -- Publish an event for dest without blocking. Kept in publisher's
    -- event ring at dest, or refused (accepted False) when that ring is
    -- full: the publisher is told and keeps it.
    ---------------------------------------------------------------------------
    procedure trySendEvent (dest      : ProcessID;
                            msg       : Message;
                            publisher : ProcessID;
                            accepted  : out Boolean;
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
                      senderThread => NO_THREAD,
                      publisher => publisher, callSequence => 0),
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

    function putReport
      (subject : ProcessID; kind : Kernel_Reports.Report_Kind;
       recipient : ProcessID; generation : Capabilities.Generation;
       msg : Message) return Boolean;

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
        faultMsg.words (0) := Process_Identities.To_Word (identityOf (pid));
        faultMsg.words (1) := detail0;
        faultMsg.words (2) := detail1;
        faultMsg.words (3) := detail2;

        if putReport (pid, Kernel_Reports.Fault_To_Supervisor, svpid,
                      generationOf (svpid), faultMsg)
        then
            ringReport (svpid);
        end if;
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
       requestId : Unsigned_64; callSeq : Unsigned_64; msg : Message)
      return Unsigned_64
    is
        me : constant ThreadID := PerCPUData.getCurrentThread;
        mypid : constant ProcessID := processOf (me);
        token : Unsigned_64;
        submitter : ThreadID;
        ok : Boolean;
    begin
        if requestId = NO_REQUEST_ID then
            -- Delivered only to the call it answers: a caller that timed
            -- out (or moved on to another call) refuses it.
            if replyThread = NO_THREAD or else
               processOf (replyThread) /= replyTo or else
               not Call_Sequences.Accepts
                     (threadtab (replyThread).state = WAITINGFORREPLY,
                      threadtab (replyThread).callSequence, callSeq)
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
                        from => Process_Identities.To_Word (identityOf (mypid)),
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
        callSeq : Unsigned_64;
        replyThread : ThreadID;
        ok : Boolean;
    begin
        if replyTo = NO_PROCESS or else IPC_Labels.Is_Kernel_Reply (msg.tag.label) then
            return 0;
        end if;
        lockMailboxes (mypid, replyTo);
        if mailtab(replyTo).closed then
            unlockMailboxes (mypid, replyTo);
            return 0;
        end if;
        consumeReplyAuthority (mypid, me, replyTo, replyThread, requestId, callSeq, ok);
        if not ok then
            unlockMailboxes (mypid, replyTo);
            return 0;
        end if;
        if mypid /= replyTo then
            Spinlocks.exitCriticalSection (mailtab(mypid).lock);
        end if;
        return completeReplyLocked (replyTo, replyThread, requestId, callSeq, msg);
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
        if mypid = NO_PROCESS or else IPC_Labels.Is_Kernel_Reply (msg.tag.label) then
            return 0;
        end if;
        -- A label only the kernel gives is refused above, before the reply
        -- is consumed. Otherwise consume the one-use reply even when delivery
        -- will fail, or a departed caller would strand the server's slot.
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
        return completeReplyLocked
          (replyTo, replyThread, cap.object.param, cap.authorityTag, msg);
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
              COMPLETION_QUEUE_SIZE or else
            not ensureCompletionSlots (pid))
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
                      senderThread => NO_THREAD, publisher => NO_PROCESS, callSequence => 0),
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
    procedure waitCompletion (destination : in  Unsigned_64;
                              maxEntries  : in  Natural;
                              minWait     : in  Natural;
                              numReturned : out Natural;
                              ok          : out Boolean)
    is
        mypid    : constant ProcessID := PerCPUData.getCurrentPID;
        me       : constant ThreadID := PerCPUData.getCurrentThread;
        receiver : constant ProcessID := getReceiver (mypid);
        Entry_Bytes : constant Storage_Count :=
          CompletionEntry'Max_Size_In_Storage_Elements;
        drained  : Natural := 0;
        item     : aliased CompletionEntry;
        taken    : Boolean;
        copied   : Boolean;
        effectiveMax : Natural;
        effectiveMin : Natural;
        ignore       : ThreadID;
    begin
        numReturned := 0;
        ok := False;
        effectiveMax := Natural'Min (maxEntries, COMPLETION_QUEUE_SIZE);
        effectiveMin := Natural'Min (minWait, effectiveMax);
        if mypid = NO_PROCESS or else effectiveMax = 0 or else
           not User_Memory.Writable_Range
             (mypid, destination, Entry_Bytes * Storage_Count (effectiveMax))
        then
            return;
        end if;
        ok := True;

        loop
            Spinlocks.enterCriticalSection (mailtab(receiver).lock);

            if completionsFor (receiver, me) >= effectiveMin then
                -- Each entry through the checked copier; the range was
                -- writable when checked (a thread unmapping its own buffer
                -- meanwhile loses what it asked for).
                while drained < effectiveMax loop
                    dequeueCompletion (receiver, me, item, taken);
                    exit when not taken;
                    User_Memory.Copy_To_User
                      (mypid, destination + Unsigned_64 (Entry_Bytes) * Unsigned_64 (drained),
                       item'Address, Entry_Bytes, copied);
                    exit when not copied;
                    drained := drained + 1;
                end loop;

                Spinlocks.exitCriticalSection (mailtab(receiver).lock);
                numReturned := drained;
                return;
            end if;

            -- Not enough completions yet: block.
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

    -- Kernel notices (docs/ipc-delivery.md, "Events are state on kernel
    -- objects"). A grant's end is kept on its record for each party until
    -- that party reads it; exit and fault reports are kept in
    -- Kernel_Reports. Nothing is queued, so nothing is dropped: the
    -- process is only told to look (kernelNoticePending).
    Grant_Event_Words : constant := 3;

    -- Per-process grant state is Process.Grant_State (grantsOf).
    -- Processes to ring once grantLock is free, linked through
    -- Grant_State.wakeNext (each at most once: wakeQueued). Under grantLock.
    noticeWakeHead      : ProcessID := NO_PROCESS;
    -- Whether that list has any member: most grant operations leave no
    -- notice, so ringGrantNotices skips grantLock when this is False.
    noticeWakesPending  : Boolean := False with Volatile;

    -- Exit and fault reports. reportLock is a leaf.
    reportLock : Spinlocks.spinlock;
    reports    : Kernel_Reports.Table;

    -- Every process's control messages (proctab(pid).controls). A leaf.
    controlLock : Spinlocks.spinlock;
    -- EVENT_CONTROL: the kind, the sender's identity.
    Control_Words : constant := 2;

    -- Tell dest to look for notices. Caller holds no grant or report lock
    -- (the mailbox lock is taken here) and not Process.lock.
    procedure ringNoticeDoorbell (dest : ProcessID) is
    begin
        if dest = NO_PROCESS then
            return;
        end if;
        Spinlocks.enterCriticalSection (mailtab(dest).lock);
        if not mailtab(dest).closed then
            proctab(dest).kernelNoticePending := True;
            wakeForUnsolicitedWork (dest);
        end if;
        Spinlocks.exitCriticalSection (mailtab(dest).lock);
    end ringNoticeDoorbell;

    -- Put pid on the list to ring once grantLock is free. Caller holds
    -- grantLock. open/closeNotices leave a queued entry in place: a doorbell
    -- on a closed mailbox does nothing, and one on a new life makes it look
    -- and find nothing (takeKernelNotice).
    procedure queueNoticeWake (pid : ProcessID) is
    begin
        if not grantsOf (pid).wakeQueued then
            grantsOf (pid).wakeQueued := True;
            grantsOf (pid).wakeNext := noticeWakeHead;
            noticeWakeHead := pid;
        end if;
        noticeWakesPending := True;
    end queueNoticeWake;

    -- Ring every process grant work left a notice for. Caller does not
    -- hold grantLock.
    procedure ringGrantNotices is
        dest : ProcessID := NO_PROCESS;
    begin
        -- Set under grantLock before it is released; this thread released
        -- it after its own grant work, so its notices are visible here.
        if not noticeWakesPending then
            return;
        end if;
        loop
            Spinlocks.enterCriticalSection (grantLock);
            dest := noticeWakeHead;
            if dest /= NO_PROCESS then
                noticeWakeHead := grantsOf (dest).wakeNext;
                grantsOf (dest).wakeNext := NO_PROCESS;
                grantsOf (dest).wakeQueued := False;
            end if;
            if noticeWakeHead = NO_PROCESS then
                noticeWakesPending := False;
            end if;
            Spinlocks.exitCriticalSection (grantLock);
            exit when dest = NO_PROCESS;
            ringNoticeDoorbell (dest);
        end loop;
    end ringGrantNotices;

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

    -- As many records as fit the one page a block gets.
    Grants_Per_Block : constant Positive :=
      4096 / (Grant'Object_Size / System.Storage_Unit);
    package Grant_Storage is new Retained_Record_Blocks
      (Grant, Memory_Grants.Global_Slot'Last, Grants_Per_Block, allocateGrantMetadataBlock);
    subtype Grant_Pointer is Grant_Storage.Element_Access;
    use type Grant_Pointer;
    use type Memory_Grants.Global_Slot;

    -- One table of grants for the whole system (KERN-003 step 2,
    -- docs/process-objects.md), indexed by global slot; slot 0 names none.
    -- Records outlive process-table pages: a grant awaiting its final
    -- reader survives its owner's record. All under grantLock.
    Grants : Grant_Storage.Store;
    -- Free slots (retired, reusable), linked through ownerNext; slots
    -- never used yet start at Next_Fresh (No_Slot once all were used).
    Free_Head  : Memory_Grants.Global_Slot := Memory_Grants.No_Slot;
    Next_Fresh : Memory_Grants.Global_Slot := Memory_Grants.Grant_Slot'First;
    -- Each process's owned grants (live, or retired with an unread notice)
    -- and received grants (live), and how many.
    -- Take a window of grantee's for a grant about to be mapped there.
    procedure takeWindow (grantee : ProcessID; g : in out Grant; taken : out Boolean) is
        window : Grant_Windows.Window;
    begin
        Grant_Windows.Allocate (grantsOf (grantee).windows, window, taken);
        if taken then
            g.granteeWindow := window;
            g.windowHeld := True;
            g.granteeAddr := To_Address (Integer_Address
              (Memory_Grants.Window_Address (window)));
        end if;
    end takeWindow;

    -- Give back g's window. Only once its pages are unmapped and every CPU
    -- has flushed them (unmapGrantPages): the next grant mapped there must
    -- not be reachable through a stale translation.
    procedure releaseWindow (g : in out Grant) is
    begin
        if g.windowHeld then
            if not Grant_Windows.Contains (grantsOf (g.granteePID).windows, g.granteeWindow) then
                raise ProcessException with "Grant window lost";
            end if;
            Grant_Windows.Release (grantsOf (g.granteePID).windows, g.granteeWindow);
            g.windowHeld := False;
        end if;
    end releaseWindow;

    function grantFor (slot : Memory_Grants.Global_Slot) return Grant_Pointer is
      (if slot = Memory_Grants.No_Slot then null else Grant_Storage.Find (Grants, slot));

    -- A free slot owner may fill, without taking it yet (a failed create
    -- leaves the table unchanged): null at the owner's quota or when the
    -- table is full.
    procedure reserveGrantRecord
      (owner : ProcessID; slot : out Memory_Grants.Global_Slot; value : out Grant_Pointer)
    is
        result : Grant_Storage.Allocation_Result;
    begin
        slot := Memory_Grants.No_Slot;
        value := null;
        if owner = NO_PROCESS or else grantsOf (owner).ownedCount >= OWNER_GRANT_QUOTA then
            return;
        end if;
        if Free_Head /= Memory_Grants.No_Slot then
            slot := Free_Head;
            value := grantFor (slot);
            return;
        end if;
        if Next_Fresh = Memory_Grants.No_Slot then
            return;
        end if;
        Grant_Storage.Ensure
          (Grants, Next_Fresh, (generation => Memory_Grants.Initial_Generation, others => <>),
           Grant_Storage.Maximum_Blocks, value, result);
        if value /= null then
            slot := Next_Fresh;
        end if;
    end reserveGrantRecord;

    -- Take the slot reserveGrantRecord gave, before its record is written.
    procedure takeReservedSlot (slot : Memory_Grants.Global_Slot) is
    begin
        if slot = Free_Head then
            Free_Head := grantFor (slot).ownerNext;
        elsif slot = Next_Fresh then
            Next_Fresh := (if Next_Fresh = Memory_Grants.Global_Slot'Last
                           then Memory_Grants.No_Slot else Next_Fresh + 1);
        else
            raise ProcessException with "Grant slot taken without a reservation";
        end if;
    end takeReservedSlot;

    -- Link a just-written live grant into its owner's and grantee's lists.
    procedure linkGrant (slot : Memory_Grants.Global_Slot) is
        value : constant Grant_Pointer := grantFor (slot);
        owner : constant ProcessID := value.granterPID;
        grantee : constant ProcessID := value.granteePID;
    begin
        value.ownerPrev := Memory_Grants.No_Slot;
        value.ownerNext := grantsOf (owner).ownedHead;
        if grantsOf (owner).ownedHead /= Memory_Grants.No_Slot then
            grantFor (grantsOf (owner).ownedHead).ownerPrev := slot;
        end if;
        grantsOf (owner).ownedHead := slot;
        grantsOf (owner).ownedCount := grantsOf (owner).ownedCount + 1;
        value.granteePrev := Memory_Grants.No_Slot;
        value.granteeNext := grantsOf (grantee).receivedHead;
        if grantsOf (grantee).receivedHead /= Memory_Grants.No_Slot then
            grantFor (grantsOf (grantee).receivedHead).granteePrev := slot;
        end if;
        grantsOf (grantee).receivedHead := slot;
        grantsOf (grantee).receivedCount := grantsOf (grantee).receivedCount + 1;
    end linkGrant;

    -- Nothing to do for a grant already off the list (its grantee died
    -- while its revocation was pending).
    procedure unlinkReceived (grantee : ProcessID; slot : Memory_Grants.Global_Slot) is
        value : constant Grant_Pointer := grantFor (slot);
    begin
        if grantee = NO_PROCESS or else
           (value.granteePrev = Memory_Grants.No_Slot and then grantsOf (grantee).receivedHead /= slot)
        then
            return;
        end if;
        if value.granteePrev /= Memory_Grants.No_Slot then
            grantFor (value.granteePrev).granteeNext := value.granteeNext;
        else
            grantsOf (grantee).receivedHead := value.granteeNext;
        end if;
        if value.granteeNext /= Memory_Grants.No_Slot then
            grantFor (value.granteeNext).granteePrev := value.granteePrev;
        end if;
        value.granteePrev := Memory_Grants.No_Slot;
        value.granteeNext := Memory_Grants.No_Slot;
        grantsOf (grantee).receivedCount := grantsOf (grantee).receivedCount - 1;
    end unlinkReceived;

    -- A retired grant leaves its owner's list: its slot is free again, or
    -- retired for good at its last generation.
    procedure releaseOwnedSlot (owner : ProcessID; slot : Memory_Grants.Global_Slot) is
        value : constant Grant_Pointer := grantFor (slot);
    begin
        if value.ownerPrev /= Memory_Grants.No_Slot then
            grantFor (value.ownerPrev).ownerNext := value.ownerNext;
        else
            grantsOf (owner).ownedHead := value.ownerNext;
        end if;
        if value.ownerNext /= Memory_Grants.No_Slot then
            grantFor (value.ownerNext).ownerPrev := value.ownerPrev;
        end if;
        grantsOf (owner).ownedCount := grantsOf (owner).ownedCount - 1;
        value.ownerPrev := Memory_Grants.No_Slot;
        value.ownerNext := Memory_Grants.No_Slot;
        if value.reusable then
            value.ownerNext := Free_Head;
            Free_Head := slot;
        end if;
    end releaseOwnedSlot;

    -- Visit every grant on one of pid's lists. Visit may retire grants,
    -- including others on the same list, so the walk is over a snapshot
    -- taken first; Visit checks each slot is still what it expects.
    generic
        with procedure Visit (slot : Memory_Grants.Global_Slot);
    procedure Sweep_Grants (pid : ProcessID; owned : Boolean);

    procedure Sweep_Grants (pid : ProcessID; owned : Boolean) is
        count : constant Natural :=
          (if owned then grantsOf (pid).ownedCount else grantsOf (pid).receivedCount);
        Slot_Bytes : constant := 4;
        order : BuddyAllocator.Order := 0;
        buffer : System.Address;
        slot : Memory_Grants.Global_Slot;
    begin
        if count = 0 then
            return;
        end if;
        while Natural (2 ** Natural (order)) * Natural (Virtmem.PAGE_SIZE) < count * Slot_Bytes loop
            order := order + 1;
        end loop;
        BuddyAllocator.alloc (order, buffer);
        if buffer = System.Null_Address then
            raise ProcessException with "No memory to walk a grant list";
        end if;
        declare
            type Slots is array (1 .. count) of Unsigned_32;
            snapshot : Slots with Import, Address => buffer;
            taken : Natural := 0;
        begin
            slot := (if owned then grantsOf (pid).ownedHead else grantsOf (pid).receivedHead);
            while slot /= Memory_Grants.No_Slot and then taken < count loop
                taken := taken + 1;
                snapshot (taken) := Unsigned_32 (slot);
                slot := (if owned then grantFor (slot).ownerNext else grantFor (slot).granteeNext);
            end loop;
            for k in 1 .. taken loop
                Visit (Memory_Grants.Global_Slot (snapshot (k)));
            end loop;
        end;
        BuddyAllocator.free (order, buffer);
    end Sweep_Grants;

    ---------------------------------------------------------------------------
    -- Grant notices (docs/ipc-delivery.md). Callers hold grantLock.
    ---------------------------------------------------------------------------

    -- The grant on value just retired (its record was reset): keep the
    -- owner's notice on it, and its slot, until the owner reads it.
    procedure setOwnerNoticeLocked
      (owner : ProcessID; value : Grant_Pointer;
       ended : Memory_Grants.Grant_Generation; peer : Process_Identities.Identity) is
    begin
        if owner = NO_PROCESS or else not grantsOf (owner).noticesOpen or else
           value.ownerNotice
        then
            return;
        end if;
        value.ownerNotice := True;
        value.noticeGeneration := ended;
        value.noticePeer := peer;
        grantsOf (owner).ownerNotices := grantsOf (owner).ownerNotices + 1;
        queueNoticeWake (owner);
    end setOwnerNoticeLocked;

    -- The owner revoked value's grant: its grantee, if it holds the grant,
    -- is told. A grantee holding nothing has nothing to give back; its
    -- acquire will fail.
    procedure setGranteeNoticeLocked (value : Grant_Pointer) is
        grantee : constant ProcessID := value.granteePID;
    begin
        if grantee = NO_PROCESS or else value.granteeNotice or else
           not grantsOf (grantee).noticesOpen or else
           Memory_Grants.Acquisition_Total (value.lifecycle) = 0
        then
            return;
        end if;
        value.granteeNotice := True;
        grantsOf (grantee).granteeNotices := grantsOf (grantee).granteeNotices + 1;
        queueNoticeWake (grantee);
    end setGranteeNoticeLocked;

    -- The grantee read the notice, or gave the grant back (the notice is
    -- answered), or the grant retired.
    procedure clearGranteeNoticeLocked (value : Grant_Pointer) is
    begin
        if value.granteeNotice then
            value.granteeNotice := False;
            if grantsOf (value.granteePID).granteeNotices > 0 then
                grantsOf (value.granteePID).granteeNotices :=
                  grantsOf (value.granteePID).granteeNotices - 1;
            end if;
        end if;
    end clearGranteeNoticeLocked;

    -- One of receiver's grant notices, as the event message it was before
    -- (CuBit.Control_Events decodes it): words are the global slot, the
    -- generation, the peer's identity. Owner notices are on its owned list
    -- (retired grants it has not read about), grantee notices on its
    -- received list.
    procedure takeGrantNoticeLocked
      (receiver : ProcessID; msg : out Message; found : out Boolean)
    is
        slot : Memory_Grants.Global_Slot;
        value : Grant_Pointer;
    begin
        msg := NULL_MESSAGE;
        found := False;
        if grantsOf (receiver).ownerNotices > 0 then
            slot := grantsOf (receiver).ownedHead;
            while slot /= Memory_Grants.No_Slot loop
                value := grantFor (slot);
                if value.ownerNotice then
                    msg.tag := (label => IPC_Labels.EVENT_GRANT_RETURNED,
                                length => Grant_Event_Words, flags => 0, reserved => 0);
                    msg.words (0) := Unsigned_64 (slot);
                    msg.words (1) := Unsigned_64 (value.noticeGeneration);
                    msg.words (2) := Process_Identities.To_Word (value.noticePeer);
                    value.ownerNotice := False;
                    grantsOf (receiver).ownerNotices := grantsOf (receiver).ownerNotices - 1;
                    -- Read: the retired grant leaves the owner's list.
                    releaseOwnedSlot (receiver, slot);
                    found := True;
                    return;
                end if;
                slot := value.ownerNext;
            end loop;
            raise ProcessException with "Owner notice count without a notice";
        end if;
        if grantsOf (receiver).granteeNotices > 0 then
            slot := grantsOf (receiver).receivedHead;
            while slot /= Memory_Grants.No_Slot loop
                value := grantFor (slot);
                if value.granteeNotice then
                    msg.tag := (label => IPC_Labels.EVENT_GRANT_REVOKED,
                                length => Grant_Event_Words, flags => 0, reserved => 0);
                    msg.words (0) := Unsigned_64 (slot);
                    msg.words (1) := Unsigned_64 (value.generation);
                    msg.words (2) := Process_Identities.To_Word (value.granterIdentity);
                    clearGranteeNoticeLocked (value);
                    found := True;
                    return;
                end if;
                slot := value.granteeNext;
            end loop;
            raise ProcessException with "Grantee notice count without a notice";
        end if;
    end takeGrantNoticeLocked;

    ---------------------------------------------------------------------------
    -- Exit and fault reports, and taking notices (docs/ipc-delivery.md)
    ---------------------------------------------------------------------------

    function toReport (msg : Message) return Kernel_Reports.Report is
      (Label   => msg.tag.label,
       Length  => Kernel_Reports.Word_Count'Min
                    (Natural (msg.tag.length), Kernel_Reports.Report_Words),
       Words   => (msg.words (0), msg.words (1), msg.words (2), msg.words (3)),
       Further => 0);

    function toMessage (value : Kernel_Reports.Report) return Message is
      (tag => (label => value.Label, length => Unsigned_8 (value.Length),
               flags => 0, reserved => value.Further),
       authorityTag => 0,
       words => (value.Words (0), value.Words (1), value.Words (2), value.Words (3)));

    -- A retired process whose last report was just read or dropped.
    procedure freeReleasedPID (pid : Kernel_Reports.Process) is
    begin
        if pid /= Kernel_Reports.No_Process then
            PIDTracker.freePID (ProcessID (pid), invalidated => True);
        end if;
    end freeReleasedPID;

    procedure releaseRetiredPID (pid : ProcessID) is
        freeNow : Boolean;
    begin
        Spinlocks.enterCriticalSection (reportLock);
        Kernel_Reports.Request_Free (reports, Kernel_Reports.Process (pid), freeNow);
        Spinlocks.exitCriticalSection (reportLock);
        if freeNow then
            PIDTracker.freePID (pid, invalidated => True);
        end if;
    end releaseRetiredPID;

    -- Keep a report about subject for recipient (this life); True when
    -- kept, so the caller rings the recipient once its locks are released.
    function putReport
      (subject : ProcessID; kind : Kernel_Reports.Report_Kind;
       recipient : ProcessID; generation : Capabilities.Generation;
       msg : Message) return Boolean
    is
        kept : Boolean := False;
    begin
        if subject = NO_PROCESS or else recipient = NO_PROCESS then
            return False;
        end if;
        Spinlocks.enterCriticalSection (reportLock);
        -- An exit is reported once per life: its PID is not reused while
        -- the report is unread, so the slot is free.
        if kind not in Kernel_Reports.Exit_Kind or else
           not reports.Subjects (Kernel_Reports.Process (subject)).Reports (kind).Unread
        then
            Kernel_Reports.Put
              (reports, Kernel_Reports.Process (subject), kind,
               (Id => Kernel_Reports.Process (recipient),
                Generation => Unsigned_64 (generation)),
               toReport (msg), kept);
        end if;
        Spinlocks.exitCriticalSection (reportLock);
        return kept;
    end putReport;

    procedure reportExit
      (pid : ProcessID; msg : Message;
       parent : ProcessID; parentGeneration : Capabilities.Generation;
       manager : ProcessID; managerGeneration : Capabilities.Generation;
       ringParent, ringManager : out Boolean) is
    begin
        ringParent := parentGeneration /= 0 and then
          putReport (pid, Kernel_Reports.Exit_To_Parent, parent, parentGeneration, msg);
        ringManager := manager /= parent and then manager /= pid and then
          putReport (pid, Kernel_Reports.Exit_To_Manager, manager, managerGeneration, msg);
    end reportExit;

    procedure ringReport (recipient : ProcessID) is
    begin
        ringNoticeDoorbell (recipient);
    end ringReport;

    procedure sendControl
      (target : ProcessID; targetGeneration : Capabilities.Generation;
       sender : ProcessID; kind : IPC_Labels.Control_Kind;
       result : out Kernel_Controls.Send_Result)
    is
        use type Kernel_Controls.Send_Result;
    begin
        result := Kernel_Controls.Not_Open;
        if target = NO_PROCESS or else sender = NO_PROCESS then
            return;
        end if;
        Spinlocks.enterCriticalSection (controlLock);
        Kernel_Controls.Send
          (proctab(target).controls, Unsigned_64 (targetGeneration),
           Kernel_Controls.Sender_Id (sender), Unsigned_64 (generationOf (sender)),
           kind, result);
        Spinlocks.exitCriticalSection (controlLock);
        if result = Kernel_Controls.Accepted then
            ringNoticeDoorbell (target);
        end if;
    end sendControl;

    procedure takeKernelNotice
      (receiver : ProcessID; item : out RingEntry; found : out Boolean)
    is
        msg : Message := NULL_MESSAGE;
        value : Kernel_Reports.Report;
        released : Kernel_Reports.Process;
    begin
        item := NULL_RING_ENTRY;
        found := False;
        if not proctab(receiver).kernelNoticePending then
            return;
        end if;
        Spinlocks.enterCriticalSection (grantLock);
        takeGrantNoticeLocked (receiver, msg, found);
        Spinlocks.exitCriticalSection (grantLock);
        if not found then
            Spinlocks.enterCriticalSection (reportLock);
            Kernel_Reports.Take
              (reports, Kernel_Reports.Process (receiver), value, found, released);
            Spinlocks.exitCriticalSection (reportLock);
            freeReleasedPID (released);
            if found then
                msg := toMessage (value);
            end if;
        end if;
        if not found then
            declare
                sender : Kernel_Controls.Sender_Id;
                senderGeneration : Unsigned_64;
                kind : IPC_Labels.Control_Kind;
            begin
                Spinlocks.enterCriticalSection (controlLock);
                Kernel_Controls.Take
                  (proctab(receiver).controls, sender, senderGeneration, kind, found);
                Spinlocks.exitCriticalSection (controlLock);
                if found then
                    -- Words: the kind, the sender's identity (its life when
                    -- it sent, kept with the message).
                    msg.tag := (label => IPC_Labels.EVENT_CONTROL,
                                length => Control_Words, flags => 0, reserved => 0);
                    msg.words (0) := IPC_Labels.Control_Kind'Enum_Rep (kind);
                    msg.words (1) := Process_Identities.To_Word (Process_Identities.Encode
                      (Process_Identities.Slot (sender),
                       Process_Identities.Generation (senderGeneration)));
                end if;
            end;
        end if;
        if found then
            item := (msg => msg, sender => NO_PROCESS, kind => RING_EVENT,
                     requestId => NO_REQUEST_ID, senderThread => NO_THREAD, publisher => NO_PROCESS, callSequence => 0);
        else
            -- The doorbell outlived its notices; a new one rings it again.
            proctab(receiver).kernelNoticePending := False;
        end if;
    end takeKernelNotice;

    procedure openNotices (pid : ProcessID) is
    begin
        Spinlocks.enterCriticalSection (grantLock);
        grantsOf (pid).noticesOpen := True;
        grantsOf (pid).ownerNotices := 0;
        grantsOf (pid).granteeNotices := 0;
        Spinlocks.exitCriticalSection (grantLock);
        Spinlocks.enterCriticalSection (reportLock);
        Kernel_Reports.Open
          (reports, Kernel_Reports.Process (pid), Unsigned_64 (generationOf (pid)));
        Spinlocks.exitCriticalSection (reportLock);
        Spinlocks.enterCriticalSection (controlLock);
        Kernel_Controls.Open (proctab(pid).controls, Unsigned_64 (generationOf (pid)));
        Spinlocks.exitCriticalSection (controlLock);
    end openNotices;

    procedure closeNotices (pid : ProcessID) is
        slot, next : Memory_Grants.Global_Slot;
        value : Grant_Pointer;
        released : Kernel_Reports.Process;
    begin
        Spinlocks.enterCriticalSection (grantLock);
        grantsOf (pid).noticesOpen := False;
        -- Its own retired grants' notices: no one will read them, so those
        -- grants leave its list now.
        slot := grantsOf (pid).ownedHead;
        while slot /= Memory_Grants.No_Slot loop
            value := grantFor (slot);
            next := value.ownerNext;
            if value.ownerNotice then
                value.ownerNotice := False;
                releaseOwnedSlot (pid, slot);
            end if;
            slot := next;
        end loop;
        slot := grantsOf (pid).receivedHead;
        while slot /= Memory_Grants.No_Slot loop
            value := grantFor (slot);
            value.granteeNotice := False;
            slot := value.granteeNext;
        end loop;
        grantsOf (pid).ownerNotices := 0;
        grantsOf (pid).granteeNotices := 0;
        Spinlocks.exitCriticalSection (grantLock);
        Spinlocks.enterCriticalSection (controlLock);
        Kernel_Controls.Close (proctab(pid).controls);
        Spinlocks.exitCriticalSection (controlLock);
        loop
            Spinlocks.enterCriticalSection (reportLock);
            Kernel_Reports.Close (reports, Kernel_Reports.Process (pid), released);
            Spinlocks.exitCriticalSection (reportLock);
            exit when released = Kernel_Reports.No_Process;
            freeReleasedPID (released);
        end loop;
    end closeNotices;

    -- A new life of pid owns and receives nothing yet: every grant of an
    -- earlier life left both lists before its slot could be reused.
    function grantListsEmpty (pid : ProcessID) return Boolean is
        empty : Boolean;
    begin
        if pid = NO_PROCESS then return False; end if;
        Spinlocks.enterCriticalSection (grantLock);
        empty := grantsOf (pid).ownedHead = Memory_Grants.No_Slot and then
                 grantsOf (pid).receivedHead = Memory_Grants.No_Slot and then
                 Grant_Windows.Is_Empty (grantsOf (pid).windows);
        Spinlocks.exitCriticalSection (grantLock);
        return empty;
    end grantListsEmpty;

    function hasActiveGrants (owner : ProcessID) return Boolean is
        slot : Memory_Grants.Global_Slot := grantsOf (owner).ownedHead;
    begin
        while slot /= Memory_Grants.No_Slot loop
            if Memory_Grants.Is_Active (grantFor (slot).lifecycle) then
                return True;
            end if;
            slot := grantFor (slot).ownerNext;
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
        -- The slot's next generation; at the last one the slot retires.
        Memory_Grants.Advance_Generation (nextGeneration, mayReuse);
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
           forwardable => False,
           notify      => False,
           others      => <>);
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
                           forwardable : Boolean := False;
                           notify : Boolean := False)
    is
        pid : constant ProcessID := PerCPUData.getCurrentPID;
        owner : constant ProcessID :=
          pid;
        receiver : ProcessID;
        flags : Unsigned_64;
        ok : Boolean;
        slot : Memory_Grants.Global_Slot := Memory_Grants.No_Slot;
        target : Grant_Pointer;
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
                  (physical, BuddyAllocator.Frame_Owner (owner), success);
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
        flags := (if perm = GRANT_READWRITE then Virtmem.PG_USERDATA
                  else Virtmem.PG_USERDATARO);
        staging := (globalSlot => slot,
                    granterPID => owner, granteePID => receiver,
                    granterIdentity => identityOf (owner),
                    granteeIdentity => identityOf (receiver),
                    granterAddr => localAddr,
                    permission => perm, forwardable => forwardable, notify => notify,
                    others => <>);
        takeWindow (receiver, staging, ok);
        if not ok then
            Spinlocks.exitCriticalSection (grantLock);
            return;
        end if;

        installPages (numPages, installed, ok);
        if not ok then
            -- Partial mappings are already removed and flushed.
            releaseWindow (staging);
            Spinlocks.exitCriticalSection (grantLock);
            return;
        end if;
        staging.numPages := installed;

        staging.lifecycle := Memory_Grants.Available_Lifecycle;
        staging.generation := target.generation;
        takeReservedSlot (slot);
        target.all := staging;
        linkGrant (slot);
        id := slot;
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
        -- The parent's owner (from its record, once found).
        rootOwner : ProcessID := NO_PROCESS;
        source : Grant_Pointer;
        scope : Scope_Pointer;
        requested : constant Grant_Loans.Terms :=
          (pageOffset, numPages, (if perm = GRANT_READWRITE then
           Memory_Grants.Borrowed_Read_Write else Memory_Grants.Borrowed_Read_Only));
        loan : Grant_Loans.Loan_Reference;
        reservation : Grant_Loans.Reservation_Result;
        applied, mapped : Boolean;
        target : Grant_Pointer;
        childSlot : Memory_Grants.Global_Slot;
        staging : Grant;
        childLink : Link_Storage.Element_Access;
        installed : Natural;
        window : Grant_Windows.Window;
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
                  (physical, BuddyAllocator.Frame_Owner (rootOwner), success);
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
        if grantee = NO_PROCESS or else expectedGeneration = 0 then
            return;
        end if;
        Spinlocks.enterCriticalSection (grantLock);
        source := grantFor (parent.slot);
        if source /= null then
            rootOwner := source.granterPID;
        end if;
        if source = null or else rootOwner = NO_PROCESS or else not proctab(caller).admitted or else not proctab(rootOwner).admitted or else
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
        reserveGrantRecord (caller, childSlot, target);
        if target = null then
            Spinlocks.exitCriticalSection (grantLock);
            return;
        end if;
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
        staging := (granteePID => grantee, others => <>);
        takeWindow (grantee, staging, mapped);
        if not mapped then
            Spinlocks.exitCriticalSection (grantLock);
            return;
        end if;
        window := staging.granteeWindow;
        Grant_Loans.Reserve (scope.all, requested, loan, reservation);
        if reservation /= Grant_Loans.Reserved then
            releaseWindow (staging);
            Spinlocks.exitCriticalSection (grantLock);
            return;
        end if;
        staging := (globalSlot => childSlot, granterPID => caller,
          granteePID => grantee,
          granterIdentity => identityOf (caller), granteeIdentity => identityOf (grantee),
          permission => perm, forwardable => False,
          notify => source.notify,
          granterAddr => To_Address (To_Integer (source.granteeAddr) +
            Integer_Address (pageOffset) * Virtmem.PAGE_SIZE),
          granteeAddr => To_Address (Integer_Address (Memory_Grants.Window_Address (window))),
          granteeWindow => window, windowHeld => True, others => <>);
        installPages (numPages, installed, mapped);
        if not mapped then
            -- Transaction has already removed its partial mappings and waited
            -- for shootdown. Only now can the reserved loan and the window
            -- retire.
            releaseWindow (staging);
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
        takeReservedSlot (childSlot);
        target.all := staging;
        linkGrant (childSlot);
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
        value : constant Grant_Pointer := grantFor (slot);
        link : constant Parent_Link := linkFor(slot);
        applied : Boolean;
        notified : Boolean;
        owner : ProcessID;
        grantee : Process_Identities.Identity;
        ended : Memory_Grants.Grant_Generation;
        ownerPrev, ownerNext : Memory_Grants.Global_Slot;
    begin
        if value = null or else value.granterPID = NO_PROCESS then return; end if;
        notified := value.notify;
        owner := value.granterPID;
        grantee := value.granteeIdentity;
        ended := value.generation;
        clearGranteeNoticeLocked (value);
        unlinkReceived (value.granteePID, slot);
        ownerPrev := value.ownerPrev;
        ownerNext := value.ownerNext;
        unmapGrantPages (value.all);
        releaseWindow (value.all);
        if link.active then
            Grant_Loans.Finish_Retirement
              (scopeFor (link.parent.slot).all, link.loan, applied);
            if not applied then
                raise ProcessException with "Child retired in invalid loan phase";
            end if;
            Link_Storage.Find (Links, slot).all := Empty_Parent_Link;
        end if;
        invalidateGrant (value.all);
        -- Still on its owner's list until any notice is read.
        value.ownerPrev := ownerPrev;
        value.ownerNext := ownerNext;
        if notified then
            setOwnerNoticeLocked (owner, value, ended, grantee);
        end if;
        if not value.ownerNotice then
            releaseOwnedSlot (owner, slot);
        end if;
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
        scope : constant Scope_Pointer := scopeFor (slot);

        -- A child of this grant: revoke it.
        procedure revokeChild (childSlot : Memory_Grants.Global_Slot) is
        begin
            if linkFor (childSlot).active and then
               linkFor (childSlot).parent = reference
            then
                revokeGrantLocked (childSlot);
            end if;
        end revokeChild;
        procedure revokeChildren is new Sweep_Grants (revokeChild);
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
        revokeChildren (receiver, owned => True);
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
        if g.notify then
            setGranteeNoticeLocked (g);
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
    procedure revokeGrant (id : Memory_Grants.Global_Slot; success : out Boolean)

    is
        owner : constant ProcessID := PerCPUData.getCurrentPID;
        value : Grant_Pointer;
    begin
        success := False;
        Spinlocks.enterCriticalSection (grantLock);
        value := grantFor (id);
        if owner = NO_PROCESS or else value = null or else value.granterPID /= owner or else
          not Memory_Grants.Is_Active (value.lifecycle)
        then
            Spinlocks.exitCriticalSection (grantLock);
            return;
        end if;
        revokeGrantLocked (id);
        success := True;
        Spinlocks.exitCriticalSection (grantLock);
        ringGrantNotices;
        notifyDMAWork;
    end revokeGrant;

    ---------------------------------------------------------------------------
    -- revokeAllGrants
    -- Revoke all active grants owned by the specified process.
    -- Called during process kill().
    ---------------------------------------------------------------------------
    procedure revokeAllGrants (pid : ProcessID)

    is
        procedure revokeOwned (slot : Memory_Grants.Global_Slot) is
        begin
            if grantFor (slot).granterPID = pid then
                revokeGrantLocked (slot);
            end if;
        end revokeOwned;
        procedure revokeEach is new Sweep_Grants (revokeOwned);
    begin
        Spinlocks.enterCriticalSection (grantLock);
        revokeEach (pid, owned => True);
        Spinlocks.exitCriticalSection (grantLock);
        ringGrantNotices;
        notifyDMAWork;
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
        -- One grant pid received, if still live: close it for the dead
        -- receiver, and retire it (or leave its revocation pending).
        procedure closeReceived (slot : Memory_Grants.Global_Slot) is
            owner : constant ProcessID := grantFor (slot).granterPID;
        begin
            if grantFor (slot).granteePID /= pid or else
               not Memory_Grants.Is_Active (grantFor (slot).lifecycle)
            then
                return;
            end if;
            declare
                hadAcquisitions : Boolean;
                value : constant Grant_Pointer := grantFor (slot);
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
                        releaseWindow (value.all);
                        value.numPages := 0;
                        -- Its receiver is gone: off its list now, so the
                        -- slot's next life starts with an empty list.
                        unlinkReceived (pid, reference.slot);
                    else
                        retireGrantLocked (reference.slot);
                    end if;
                end if;
            end;
            completeOwnerPIDIfReady (owner);
        end closeReceived;
        procedure closeEach is new Sweep_Grants (closeReceived);
    begin
        Spinlocks.enterCriticalSection (grantLock);
        closeEach (pid, owned => False);
        Spinlocks.exitCriticalSection (grantLock);
        ringGrantNotices;
        notifyDMAWork;
    end revokeAllGrantsTo;

    procedure getOwnedGrantGeneration
      (slot       : Memory_Grants.Grant_Slot;
       generation : out Memory_Grants.Grant_Generation)

    is
        pid : constant ProcessID := PerCPUData.getCurrentPID;
        owner : constant ProcessID :=
          pid;
        value : Grant_Pointer;
    begin
        generation := 0;

        Spinlocks.enterCriticalSection (grantLock);

        value := grantFor (slot);
        --  Zero unless the caller owns an active grant here. All
        --  invalidation paths retire mappings/TLBs before marking inactive.
        if value /= null and then Memory_Grants.Is_Active (value.lifecycle)
           and then value.granterPID = owner
        then
            generation := value.generation;
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
        value : Grant_Pointer;
        mappedBytes : Unsigned_64;
        applied : Boolean;
        link : Parent_Link;
    begin
        mappedAddress := System.Null_Address;
        success := False;

        Spinlocks.enterCriticalSection (grantLock);
        link := linkFor (reference.slot);
        value := grantFor (reference.slot);

        if value = null or else expectedOwner = NO_PROCESS or else
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

    procedure releaseDMAAllocations (pid : ProcessID) is
        complete : Boolean;
        reusable : Boolean;
    begin
        -- Only the retirement worker calls this, after Take_Ready. No grant
        -- lock is held across the bounded ownership-tag cleanup.
        Spinlocks.enterCriticalSection (grantLock);
        if not proctab(pid).grantTeardownPending or else
           not proctab(pid).grantTeardownReady or else hasActiveGrants (pid)
        then
            DMA.Finish_Step (pid, True);
            Spinlocks.exitCriticalSection (grantLock);
            return;
        end if;
        Spinlocks.exitCriticalSection (grantLock);
        DMA.Retire_Step (pid, True, complete);
        Spinlocks.enterCriticalSection (grantLock);
        DMA.Finish_Step (pid, complete);
        if complete then
            reusable := proctab(pid).pidReusableAfterGrants;
            proctab(pid).grantTeardownPending := False;
            proctab(pid).grantTeardownReady := False;
            proctab(pid).pidReusableAfterGrants := False;
            if reusable then
                releaseRetiredPID (pid);
            end if;
        end if;
        Spinlocks.exitCriticalSection (grantLock);
    end releaseDMAAllocations;

    procedure completeOwnerPIDIfReady (owner : ProcessID) is
    begin
        if proctab(owner).grantTeardownPending and then
           proctab(owner).grantTeardownReady and then
           not hasActiveGrants (owner)
        then
            -- Enqueue only: never walk allocation pages while holding
            -- grantLock. Repeated notifications cannot duplicate a worker job.
            DMA.Enqueue (owner);
        end if;
    end completeOwnerPIDIfReady;

    procedure returnGrant
      (reference : Memory_Grants.Reference;
       success   : out Boolean)

    is
        pid : constant ProcessID := PerCPUData.getCurrentPID;
        receiver : constant ProcessID :=
          pid;
        value : Grant_Pointer;
        result : Memory_Grants.Return_Result;
        revoked : Memory_Grants.Revocation_Result;
        applied : Boolean;
        link : Parent_Link;
    begin
        success := False;
        Spinlocks.enterCriticalSection (grantLock);

        link := linkFor (reference.slot);
        value := grantFor (reference.slot);
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
            -- Giving the grant back answers a revoke notice.
            clearGranteeNoticeLocked (value);
            if result = Memory_Grants.Revocation_Completed_On_Return then
                retireGrantLocked (reference.slot);
            elsif value.notify and then not link.active and then
                  not Memory_Grants.Has_Forwarding_Hold (value.lifecycle)
            then
                -- A notified grant is one channel's: its grantee letting go
                -- ends it, and the owner hears so from the kernel. A close
                -- request a flooded mailbox refused cannot leave the owner
                -- holding a reader that is gone (docs/data-plane.md,
                -- "Security review").
                Memory_Grants.Request_Revocation (value.lifecycle, revoked);
                if revoked = Memory_Grants.Revocation_Completed then
                    retireGrantLocked (reference.slot);
                end if;
            end if;
        end if;

        success := True;
        Spinlocks.exitCriticalSection (grantLock);
        ringGrantNotices;
        notifyDMAWork;
    end returnGrant;

    procedure revokeGrantReference
      (reference : Memory_Grants.Reference;
       success   : out Boolean)

    is
        pid : constant ProcessID := PerCPUData.getCurrentPID;
        owner : constant ProcessID :=
          pid;
        value : Grant_Pointer;
    begin
        success := False;
        Spinlocks.enterCriticalSection (grantLock);
        value := grantFor (reference.slot);
        if value = null or else
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
        ringGrantNotices;
        notifyDMAWork;
    end revokeGrantReference;

    procedure prepareGrantProtectedTeardown
      (pid         : ProcessID;
       pidReusable : Boolean;
       deferred    : out Boolean)

    is
    begin
        deferred := False;
        Spinlocks.enterCriticalSection (grantLock);
        deferred := hasActiveGrants (pid) or else DMA.Has_Records (pid);

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
        -- CPU execution and address-space destruction are complete before
        -- this call. Require synchronous all-CPU translation retirement too.
        TLB_Shootdown.Invalidate_All;
        Spinlocks.enterCriticalSection (grantLock);
        if proctab(pid).grantTeardownPending then
            proctab(pid).grantTeardownReady := True;
            completeOwnerPIDIfReady (pid);
        end if;
        Spinlocks.exitCriticalSection (grantLock);
        ringGrantNotices;
        notifyDMAWork;
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
                      msg     : Message;
                      deadlineMs : Unsigned_64) return MessageTag
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
                     expectedGeneration => generation, deadlineMs => deadlineMs);
    end capSend;

    ---------------------------------------------------------------------------
    -- capCall
    ---------------------------------------------------------------------------
    function capCall (capSlot : Capabilities.CapabilitySlot;
                      msg     : Message;
                      deadlineMs : Unsigned_64) return MessageTag
    is
    begin
        return capSend (capSlot, msg, deadlineMs);
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


    -- Caller holds Process.lock. A thread that ends with a call handed to
    -- it (IPC-003) and never taken fails that caller's wait, as a queued
    -- synchronous send does when its receiver dies.
    procedure failHandoff (tid : ThreadID) is
        s : ThreadID;
    begin
        if threadtab (tid).handoffValid then
            s := threadtab (tid).handoffItem.senderThread;
            if threadtab (tid).handoffItem.kind = RING_SYNC and then s /= NO_THREAD
               and then Call_Sequences.Accepts
                          (threadtab (s).state = WAITINGFORREPLY,
                           threadtab (s).callSequence,
                           threadtab (tid).handoffItem.callSequence)
            then
                threadtab (s).replyMsg := NULL_MESSAGE;
                ready (s);
            end if;
            threadtab (tid).handoffValid := False;
            threadtab (tid).handoffItem := NULL_RING_ENTRY;
        end if;
    end failHandoff;

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
            -- Only the call this capability answers (the caller may have
            -- timed out and be waiting on another).
            if waiter /= NO_THREAD and then
               Call_Sequences.Accepts
                 (threadtab (waiter).state = WAITINGFORREPLY,
                  threadtab (waiter).callSequence, cap.authorityTag)
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
        failHandoff (tid);
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
                    -- reused and before a receiver can mint a reply cap:
                    -- the dead sender's rings at p go.
                    forgetSender (p, pid);
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
                        state : constant Credit_State := stateOf (pid, Request_Class);
                        s     : ThreadID;
                        e     : RingEntry;
                    begin
                        if state /= null then
                            for sender in Kernel_Credits.Sender loop
                                for k in 0 .. state.Rings (sender).Count - 1 loop
                                    e := entryAt (pid, Request_Class, sender, 0).all
                                      (entryIndex (sender, (state.Rings (sender).Head + k) mod QUEUE_CREDIT));
                                    s := e.senderThread;
                                    if e.kind = RING_SYNC and then s /= NO_THREAD
                                       and then Call_Sequences.Accepts
                                                  (threadtab (s).state = WAITINGFORREPLY,
                                                   threadtab (s).callSequence, e.callSequence)
                                    then
                                        threadtab (s).replyMsg := NULL_MESSAGE;
                                        ready (s);
                                    end if;
                                end loop;
                            end loop;
                        end if;
                    end drainRingSenders;

                    --  Calls handed to its threads and never taken.
                    drainHandoffs : declare
                        t : ThreadID := mainThreadOf (pid);
                    begin
                        while t /= NO_THREAD loop
                            failHandoff (t);
                            t := threadtab (t).nextSibling;
                        end loop;
                    end drainHandoffs;
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
                freeQueues (pid);
                mailtab(pid).nextReceiveLane := Queued_Messages;

                --  Clear completion queue and pending requests
                --  In place: a whole-queue aggregate is a 5.8 KiB temporary.
                declare
                    queue : CompletionQueue renames completionTab(pid).C.all;
                begin
                    if queue.slots /= null then
                        clearCompletionSlots (queue.slots.all);
                    end if;
                    queue.head := 0;
                    queue.tail := 0;
                    queue.count := 0;
                end;
                proctab(pid).pendingRequests :=
                    (others => NO_PENDING);
                proctab(pid).numPending := 0;
                proctab(pid).requestSequence := IPC_Request_Ids.Initial_Sequence;
                proctab(pid).irqNotificationPending := False;
                proctab(pid).kernelNoticePending := False;
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
                                cq.slots.ring(cq.tail) :=
                                    (requestId =>
                                        proctab(p).pendingRequests(r)
                                            .requestId,
                                     token =>
                                        proctab(p).pendingRequests(r)
                                            .token,
                                     msg       => NULL_MESSAGE,
                                     from      => Process_Identities.To_Word (identityOf (pid)),
                                     status    => COMPLETION_TARGET_DIED,
                                     valid     => True,
                                     reserved  => (others => 0));
                                cq.slots.owners(cq.tail) :=
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


end Process.IPC;
