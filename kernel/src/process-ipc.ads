-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- @summary
-- CuBitOS IPC
--
-- Multi-word register-based IPC (L4/seL4 style). Messages carry a tag
-- (opcode, length, flags, badge) plus up to 4 64-bit data words, for a
-- total of 40 bytes passed entirely in registers.
--
-- Lock ordering (acquire in this order, never reverse):
--   1. mailtab(pid).lock    (per-mailbox, also protects completionTab(pid))
--   2. Process.lock         (global process table)
--   3. readyList.lock       (scheduler)
--   4. sleepList.lock       (sleep queue)
-------------------------------------------------------------------------------
with Capabilities;

package Process.IPC with
    SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- send
    -- Synchronous send: deliver a message to a process' mailbox. If a
    -- receiver is already waiting, hand the message off immediately.
    -- Otherwise enqueue as a sender and block.
    --
    -- The caller blocks in WAITINGFORREPLY until the receiver calls reply().
    -- @return the reply message tag.
    ---------------------------------------------------------------------------
    function send (dest : ProcessID; msg : Message) return MessageTag
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- sendEvent
    -- Non-blocking version of send, intended for interrupts. Sends a message
    -- to the destination process but does not block whatever process was
    -- active when the interrupt occurred.
    ---------------------------------------------------------------------------
    procedure sendEvent (dest : ProcessID; msg : Message)
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- receive
    -- Receive a message from one's mailbox. Block if no message available.
    -- On return, from contains the sender PID and msg contains the message.
    ---------------------------------------------------------------------------
    procedure receive (from : out ProcessID; msg : out Message)
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- receiveEvent
    -- Block until an event is received, or return the event if one is already
    -- waiting. If multiple events are sent to this process between
    -- receiveEvent calls, only the latest one will be delivered.
    ---------------------------------------------------------------------------
    function receiveEvent return Message
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- reply
    -- Reply to a process who called send() or submit() and unblock it.
    -- Dual-path: if sender is in WAITINGFORREPLY (sync send), stores reply
    -- and wakes them directly. If sender used submit() (async), enqueues a
    -- CompletionEntry and wakes them if in WAITINGFORCOMPLETION.
    -- @return 1 on success.
    ---------------------------------------------------------------------------
    function reply (replyTo : ProcessID; msg : Message) return Unsigned_64
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- receiveNB
    -- Non-blocking version of receive.
    -- If a message is available, sets found to True and fills from/msg.
    -- Otherwise sets found to False.
    ---------------------------------------------------------------------------
    procedure receiveNB (from  : out ProcessID;
                         msg   : out Message;
                         found : out Boolean)
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- Async I/O Primitives
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- submit
    -- Non-blocking async send. Delivers message to dest's mailbox and
    -- returns immediately. The caller records a (dest, token) pending
    -- request so that reply() can enqueue a completion.
    -- @return True on success, False if mailbox full or invalid dest.
    ---------------------------------------------------------------------------
    function submit (dest  : ProcessID;
                     msg   : Message;
                     token : Unsigned_64) return Boolean
        with SPARK_Mode => On;

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
                              numReturned : out Natural)
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- pollCompletion
    -- Non-blocking single completion check.
    -- @param result - the completion entry if found
    -- @param found  - True if a completion was available
    ---------------------------------------------------------------------------
    procedure pollCompletion (result : out CompletionEntry;
                              found  : out Boolean)
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- Shared Memory Grant Operations
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- createGrant
    -- Map pages from the caller's address space into grantee's address space.
    -- The caller (granter) owns the grant and can revoke it later.
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
                           id        : out GrantID;
                           success   : out Boolean)
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- revokeGrant
    -- Unmap granted pages from the grantee's address space.
    -- Only the granter (caller) can revoke.
    -- @param id - ID of the grant to revoke
    ---------------------------------------------------------------------------
    procedure revokeGrant (id : GrantID)
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- revokeAllGrants
    -- Revoke all active grants owned by the specified process.
    -- Called during process kill() to prevent dangling mappings.
    -- @param pid - PID of the process whose grants should be revoked
    ---------------------------------------------------------------------------
    procedure revokeAllGrants (pid : ProcessID)
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- Capability-Aware IPC
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- capSend
    -- Resolve the endpoint capability at capSlot in the caller's cap table,
    -- stamp the message badge from the capability, and perform a synchronous
    -- send to the resolved destination.
    -- @return the reply message tag (NULL_TAG on capability error).
    ---------------------------------------------------------------------------
    function capSend (capSlot : Capabilities.CapabilitySlot;
                      msg     : Message) return MessageTag
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- capCall
    -- Like capSend but writes the full reply message back via pointer.
    -- Resolves endpoint cap, stamps badge, sends, returns reply tag.
    -- The caller should read the full reply from proctab(pid).replyMsg.
    -- @return the reply message tag (NULL_TAG on capability error).
    ---------------------------------------------------------------------------
    function capCall (capSlot : Capabilities.CapabilitySlot;
                      msg     : Message) return MessageTag
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- capSubmit
    -- Resolve endpoint capability, stamp badge, perform async submit.
    -- @return True on success, False on capability error or mailbox full.
    ---------------------------------------------------------------------------
    function capSubmit (capSlot : Capabilities.CapabilitySlot;
                        msg     : Message;
                        token   : Unsigned_64) return Boolean
        with SPARK_Mode => On;

end Process.IPC;
