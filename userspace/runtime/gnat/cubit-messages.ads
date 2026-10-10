------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2021 Jon Andrew
--
--  @summary
--  IPC Messages / Syscalls
--
--  Full multi-word message support matching kernel Process.Message types.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;
with CuBit.Process_IDs;

pragma Warnings (Off, "internal GNAT unit");
with System.Secondary_Stack;
pragma Warnings (On, "internal GNAT unit");

package CuBit.Messages is

   --  Syscall Numbers

   SYSCALL_EXIT            : constant Unsigned_64 := 0;
   SYSCALL_GETPID          : constant Unsigned_64 := 6;
   SYSCALL_KILL            : constant Unsigned_64 := 7;
   SYSCALL_SBRK            : constant Unsigned_64 := 8;
   SYSCALL_WRITE           : constant Unsigned_64 := 12;
   SYSCALL_INFO            : constant Unsigned_64 := 15;
   SYSCALL_RECEIVE         : constant Unsigned_64 := 17;
   SYSCALL_REPLY           : constant Unsigned_64 := 18;
   SYSCALL_SEND_EVENT      : constant Unsigned_64 := 19;
   SYSCALL_RECEIVE_EVENT   : constant Unsigned_64 := 20;
   SYSCALL_RECEIVE_UNTIL_MONOTONIC_MILLISECOND :
      constant Unsigned_64 := 21;
   SYSCALL_POLL_ANY_IPC    : constant Unsigned_64 := 22;
   SYSCALL_WAIT_COMPLETION : constant Unsigned_64 := 24;
   SYSCALL_POLL_COMPLETION : constant Unsigned_64 := 25;
   SYSCALL_RECEIVE_EVENT_NB : constant Unsigned_64 := 26;
   SYSCALL_POLL_EVENT      : constant Unsigned_64 := 26;
   SYSCALL_GETTIME         : constant Unsigned_64 := 27;
   --  GETTIME's epoch when the kernel publishes the clock page (then read
   --  CuBit.Monotonic instead, docs/fast-clock.md), else the HPET's own;
   --  Last indicates unavailable. Never UTC.
   SYSCALL_READ_MONOTONIC_MICROSECONDS : constant Unsigned_64 := 114;
   --  Allocate zero-filled RW/NX private RAM: arg0 bytes (1..16MiB), rounded
   --  to 4KiB. Returns a page-aligned base, or zero on failure.
   --  No fixed address selection.
   SYSCALL_ALLOCATE_OWNED_MEMORY : constant Unsigned_64 := 115;
   --  Release own exact allocation: arg0 base, arg1 bytes (same rounded size).
   --  Returns zero on success, Last on rejection.
   --  Partial unmapping unsupported.
   --  Existing grant acquisitions retain their backing until returned.
   SYSCALL_RELEASE_OWNED_MEMORY : constant Unsigned_64 := 116;
   --  Page-aligned subrange of one owned allocation; bytes round upward.
   --  Mode 0 inaccessible, 1 read-only, 3 RW; no executable mode.
   SYSCALL_PROTECT_OWNED_MEMORY : constant Unsigned_64 := 117;
   --  Reserve virtual capacity only: arg0 page-aligned bytes (up to 2GiB).
   --  Returns base or zero. No RAM is committed by reservation alone.
   SYSCALL_RESERVE_OWNED_MEMORY : constant Unsigned_64 := 123;
   --  arg0 reservation base, arg1 exact current prefix, arg2 page-aligned
   --  additional bytes (up to 16MiB). Returns zero or Last; old pointers stay.
   SYSCALL_COMMIT_OWNED_MEMORY_PREFIX : constant Unsigned_64 := 124;
   --  arg0 base, arg1 exact reserved capacity. Retires all committed chunks;
   --  returns zero only after address-space reservation is released.
   SYSCALL_RELEASE_OWNED_RESERVATION : constant Unsigned_64 := 125;
   SYSCALL_SLEEP           : constant Unsigned_64 := 28;
   --  Give the CPU to any other ready thread and run again when next picked.
   SYSCALL_YIELD           : constant Unsigned_64 := 118;
   --  Sleep until an absolute time on the READ_MONOTONIC_MICROSECONDS clock
   --  (word 0); a time already passed returns at once.
   SYSCALL_SLEEP_UNTIL_MONOTONIC_MICROSECOND :
     constant Unsigned_64 := 119;
   SYSCALL_POLL_SERVICE_REQUEST : constant Unsigned_64 := 80;
   SYSCALL_CREATE_SHARED_MEMORY_GRANT_FOR_PROCESS_ID :
      constant Unsigned_64 := 102;
   SYSCALL_REVOKE_SHARED_MEMORY_GRANT : constant Unsigned_64 := 103;
   SYSCALL_INP8            : constant Unsigned_64 := 30;
   SYSCALL_OUTP8           : constant Unsigned_64 := 31;
   SYSCALL_INP16           : constant Unsigned_64 := 32;
   SYSCALL_OUTP16          : constant Unsigned_64 := 33;
   SYSCALL_INP32           : constant Unsigned_64 := 36;
   SYSCALL_OUTP32          : constant Unsigned_64 := 37;

   SYSCALL_MAPFB           : constant Unsigned_64 := 29;

   SYSCALL_VIRT_TO_PHYS    : constant Unsigned_64 := 50;

   SYSCALL_SPAWN           : constant Unsigned_64 := 60;

   SYSCALL_MAP_DEVICE      : constant Unsigned_64 := 70;
   SYSCALL_PROCLIST        : constant Unsigned_64 := 71;
   SYSCALL_POLICY_MINT_CAPABILITY : constant Unsigned_64 := 72;
   SYSCALL_POLICY_MINT_CAPABILITY_FOR_INCARNATION :
     constant Unsigned_64 := 120;
   SYSCALL_POLICY_DELEGATE_ENDPOINT : constant Unsigned_64 := 121;
   SYSCALL_RESUME          : constant Unsigned_64 := 73;

   --  Device manager syscalls
   SYSCALL_ALLOC_DMA       : constant Unsigned_64 := 74;
   --  ALLOC_DMA arg3: 0 ordinary, 1 retained with 4 KiB CPU leaves,
   --  3 retained driver-private 2 MiB CPU leaf (order9/aligned VA only).
   --  Subpage grants pin constituent frames and map 4 KiB recipient leaves.
   SYSCALL_ENABLE_IRQ      : constant Unsigned_64 := 75;
   SYSCALL_MAP_INTO        : constant Unsigned_64 := 76;
   SYSCALL_SET_SYSINFO     : constant Unsigned_64 := 77;
   SYSCALL_SET_CPU         : constant Unsigned_64 := 78;
   SYSCALL_SET_LATENCY_CONTRACT : constant Unsigned_64 := 81;
   SYSCALL_TRACE_RESET     : constant Unsigned_64 := 82;
   SYSCALL_TRACE_SUMMARY   : constant Unsigned_64 := 83;
   SYSCALL_INSPECT_CAPABILITY : constant Unsigned_64 := 84;

   SYSCALL_REGISTER_DRIVER : constant Unsigned_64 := 2000;

   --  Capability-aware IPC syscalls
   --  Synchronous calls take a deadline (docs/ipc-fastpath.md, "Call
   --  deadlines").
   SYSCALL_SEND_VIA_ENDPOINT_CAPABILITY        : constant Unsigned_64 := 129;
   SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY        : constant Unsigned_64 := 128;
   SYSCALL_SUBMIT_VIA_ENDPOINT_CAPABILITY      : constant Unsigned_64 := 42;

   --  Atomic reply+receive
   SYSCALL_REPLY_WAIT      : constant Unsigned_64 := 48;

   --  Move reply cap from slot 63 to another slot (deferred replies)
   SYSCALL_MOVE_REPLY_CAPABILITY : constant Unsigned_64 := 51;
   SYSCALL_REPLY_AND_CONSUME_REPLY_CAPABILITY :
      constant Unsigned_64 := 52;

   --  Capability-directed shared-memory grant
   SYSCALL_CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY :
      constant Unsigned_64 := 106;

   --  Transitional service discovery syscall
   SYSCALL_SET_WELL_KNOWN  : constant Unsigned_64 := 107;
   --  Returns zero for an owned inactive slot; Last for invalid/foreign slots.
   --  Nonzero generation includes pending revocation, not just available use.
   SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION :
      constant Unsigned_64 := 108;
   SYSCALL_ACQUIRE_SHARED_MEMORY_GRANT : constant Unsigned_64 := 109;
   SYSCALL_REVOKE_SHARED_MEMORY_GRANT_REFERENCE :
      constant Unsigned_64 := 110;
   SYSCALL_RETURN_SHARED_MEMORY_GRANT_ACQUISITION :
      constant Unsigned_64 := 111;
   SYSCALL_ACQUIRE_SHARED_MEMORY_GRANT_VIA_CAPABILITY :
      constant Unsigned_64 := 112;
   SYSCALL_DERIVE_SHARED_MEMORY_GRANT_VIA_CAPABILITY :
      constant Unsigned_64 := 122;
   SYSCALL_WAIT_FOR_IPC_OR_COMPLETION_UNTIL_MONOTONIC_MILLISECOND :
      constant Unsigned_64 := 113;

   type Activity_Result is (Work_Available, Deadline_Reached, Unavailable);
   --  TRACE_SUMMARY controls beyond Summary require a LATENCY_TRACE kernel.
   type Trace_Control_Operation is
     (Summary, Start_Local, Freeze_Local, Dump_Local);
   for Trace_Control_Operation use
     (Summary => 0, Start_Local => 1, Freeze_Local => 2, Dump_Local => 3);
   --  Readiness hint only: drain typed queues separately.
   --  Last means no deadline.
   --  Available work wins over an expired deadline. Another receiver may
   --  drain it before the caller polls; this does not reserve queue entries.
   function Wait_For_Activity_Until (Deadline : Unsigned_64)
      return Activity_Result;

   --  Well-known service roles (must match kernel Config.ServiceRole)
   ROLE_FILESYSTEM : constant Unsigned_64 := 1;
   ROLE_PROCMGR    : constant Unsigned_64 := 2;

   --  Well-known capability slots
   CAP_SLOT_SELF      : constant Unsigned_64 := 0;
   CAP_SLOT_FS        : constant Unsigned_64 := 1;
   CAP_SLOT_KEYBOARD  : constant Unsigned_64 := 2;
   CAP_SLOT_SELF_PROC : constant Unsigned_64 := 3;
   CAP_SLOT_ATA       : constant Unsigned_64 := 10;
   CAP_SLOT_RAMDISK   : constant Unsigned_64 := 13;
   CAP_SLOT_NVME      : constant Unsigned_64 := 11;
   CAP_SLOT_NET       : constant Unsigned_64 := 11;
   CAP_SLOT_PROCMGR   : constant Unsigned_64 := 12;
   CAP_SLOT_MIXER     : constant Unsigned_64 := 14;
   CAP_SLOT_MIXER_NTF : constant Unsigned_64 := 15;
   CAP_SLOT_CONFIG    : constant Unsigned_64 := 20;
   CAP_SLOT_DESKTOP   : constant Unsigned_64 := 21;
   CAP_SLOT_DISPLAY   : constant Unsigned_64 := 22;
   CAP_SLOT_CLOCK     : constant Unsigned_64 := 25;

   subtype CapabilitySlot is Unsigned_64 range 0 .. 63;

   --  Maximum work accepted by one privileged MAP_INTO syscall.  Callers
   --  map larger regions in bounded chunks so the kernel remains responsive.
   MAX_MAP_INTO_PAGES_PER_CALL : constant Unsigned_64 := 1024;

   STDOUT : constant Unsigned_64 := 1;

   --  Sysinfo query IDs (must match kernel/src/sysinfo.ads)
   SYSINFO_RAMDISK_ADDRESS    : constant Unsigned_64 := 1000;
   SYSINFO_SECONDARY_STACK    : constant Unsigned_64 := 1001;
   SYSINFO_RAMDISK_SIZE       : constant Unsigned_64 := 1002;
   SYSINFO_NET_IOBASE         : constant Unsigned_64 := 1200;
   SYSINFO_NVME_BAR0          : constant Unsigned_64 := 1300;
   SYSINFO_NVME_DMA_PHYS      : constant Unsigned_64 := 1301;
   SYSINFO_HDA_BAR0           : constant Unsigned_64 := 1500;
   SYSINFO_HDA_DMA_PHYS       : constant Unsigned_64 := 1501;
   SYSINFO_GPU_BAR0           : constant Unsigned_64 := 1700;
   SYSINFO_GPU_DMA_PHYS       : constant Unsigned_64 := 1701;
   SYSINFO_GPU_COMMON_OFF     : constant Unsigned_64 := 1702;
   SYSINFO_GPU_NOTIFY_OFF     : constant Unsigned_64 := 1703;
   SYSINFO_GPU_ISR_OFF        : constant Unsigned_64 := 1704;
   SYSINFO_GPU_DEVICE_OFF     : constant Unsigned_64 := 1705;
   SYSINFO_GPU_NOTIFY_MULT    : constant Unsigned_64 := 1706;
   SYSINFO_GPU_IS_PRIMARY     : constant Unsigned_64 := 1707;
   SYSINFO_GPU_SECOND_DMA_PHYS : constant Unsigned_64 := 1708;
   SYSINFO_NUM_CPUS           : constant Unsigned_64 := 1400;
   SYSINFO_MONOTONIC_DIAGNOSTIC : constant Unsigned_64 := 1402;
   --  UTC milliseconds since the Unix epoch at monotonic time zero (UTC now
   --  = this + SYSCALL_GETTIME), or 0 while unknown. Set only by the
   --  registered clock service.
   SYSINFO_WALL_CLOCK_OFFSET  : constant Unsigned_64 := 1403;
   SYSINFO_MEM_OWNED_SELF     : constant Unsigned_64 := 1602;
   SYSINFO_EVENT_DROPS_SELF   : constant Unsigned_64 := 1401;
   SYSINFO_REGISTERED_DRIVER  : constant Unsigned_64 := 2000;

   --  Driver IDs for SYSINFO_REGISTERED_DRIVER queries
   DRIVER_KEYBOARD : constant Unsigned_64 := 1;
   DRIVER_ATA      : constant Unsigned_64 := 2;
   DRIVER_NETSTACK : constant Unsigned_64 := 3;
   DRIVER_PROCMGR  : constant Unsigned_64 := 4;
   DRIVER_NVME     : constant Unsigned_64 := 5;
   DRIVER_FS       : constant Unsigned_64 := 6;
   DRIVER_DEVMGR   : constant Unsigned_64 := 7;
   DRIVER_HDA      : constant Unsigned_64 := 8;
   DRIVER_MIXER    : constant Unsigned_64 := 9;
   DRIVER_MOUSE    : constant Unsigned_64 := 10;
   DRIVER_CONFIG   : constant Unsigned_64 := 11;
   DRIVER_NETMGR   : constant Unsigned_64 := 12;
   DRIVER_LOGSTORE : constant Unsigned_64 := 13;
   DRIVER_IPCTEST  : constant Unsigned_64 := 14;
   DRIVER_DESKTOP  : constant Unsigned_64 := 15;
   DRIVER_DISPLAY  : constant Unsigned_64 := 16;
   DRIVER_GPU      : constant Unsigned_64 := 17;
   DRIVER_CCL_TEST : constant Unsigned_64 := 18;
   DRIVER_CLOCK    : constant Unsigned_64 := 19;

   --  Scheduler latency classes. Values match kernel Process.LatencyClass.
   LATENCY_BACKGROUND  : constant Unsigned_64 := 0;
   LATENCY_NORMAL      : constant Unsigned_64 := 1;
   LATENCY_INTERACTIVE : constant Unsigned_64 := 2;
   LATENCY_REALTIME    : constant Unsigned_64 := 3;

   --  IPC Message Types (matching kernel Process.Message)

   type MessageTag is record
      label  : Unsigned_32;
      length : Unsigned_8;
      flags  : Unsigned_8;
      reserved : Unsigned_16; -- Not authenticated; never an authority tag
   end record with Size => 64;

   for MessageTag use record
      label  at 0 range 0 .. 31;
      length at 4 range 0 .. 7;
      flags  at 5 range 0 .. 7;
      reserved  at 6 range 0 .. 15;
   end record;

   NULL_TAG : constant MessageTag :=
     (label => 0, length => 0, flags => 0, reserved => 0);

   type MessageWords is array (0 .. 3) of Unsigned_64;

   type Message is record
      tag      : MessageTag;
      --  Kernel-stamped from the capability authorizing this message.
      authorityTag : Unsigned_64 := 0;
      words    : MessageWords;
   end record;

   pragma Compile_Time_Error (Message'Size /= 48 * 8,
                              "IPC message ABI must remain 48 bytes");

   NULL_MESSAGE : constant Message :=
     (tag => NULL_TAG, authorityTag => 0, words => (others => 0));

   --  A process (CuBit.Process_IDs, KERN-003), with what clients of this
   --  package use with it.
   subtype Process_ID is CuBit.Process_IDs.Process_ID;
   No_Process : Process_ID renames CuBit.Process_IDs.No_Process;
   function "=" (Left, Right : Process_ID) return Boolean
     renames CuBit.Process_IDs."=";
   function To_Word (Process : Process_ID) return Unsigned_64
     renames CuBit.Process_IDs.To_Word;
   function From_Word (Word : Unsigned_64) return Process_ID
     renames CuBit.Process_IDs.From_Word;
   function Is_Process (Process : Process_ID) return Boolean
     renames CuBit.Process_IDs.Is_Process;

   --  Async completion queue types (matching kernel process.ads)

   NO_COMPLETION_TOKEN : constant Unsigned_64 := Unsigned_64'Last;
   COMPLETION_OK              : constant Unsigned_64 := 0;
   COMPLETION_TARGET_DIED     : constant Unsigned_64 := 1;
   COMPLETION_CANCELLED       : constant Unsigned_64 := 2;
   COMPLETION_QUEUE_OVERFLOW  : constant Unsigned_64 := 3;

   type CompletionEntry is record
      requestId : Unsigned_64;
      token : Unsigned_64;
      msg   : Message;
      from  : Unsigned_64;
      status : Unsigned_64 := COMPLETION_OK;
      valid : Boolean := False;
   end record;
   --  Wire layout shared with kernel Process.CompletionEntry. The kernel
   --  explicitly zeroes reserved tail bytes 81..87 before exporting them.
   for CompletionEntry use record
      requestId at 0 range 0 .. 63;
      token at 8 range 0 .. 63;
      msg at 16 range 0 .. 383;
      from at 64 range 0 .. 63;
      status at 72 range 0 .. 63;
      valid at 80 range 0 .. 7;
   end record;
   for CompletionEntry'Size use 88 * 8;
   for CompletionEntry'Alignment use 8;

   NULL_COMPLETION : constant CompletionEntry :=
     (requestId => 0,
      token     => 0,
      msg       => NULL_MESSAGE,
      from      => 0,
      status    => COMPLETION_OK,
      valid     => False);

   COMPLETION_QUEUE_SIZE : constant := 64;
   subtype CompletionIndex is Natural range 0 .. COMPLETION_QUEUE_SIZE - 1;
   type CompletionRing is array (CompletionIndex) of CompletionEntry;

   --  Raw syscall wrapper

   function syscall
     (call : Unsigned_64; arg0 : Unsigned_64 := 0; arg1 : Unsigned_64 := 0;
      arg2 : Unsigned_64 := 0; arg3 : Unsigned_64 := 0;
      arg4 : Unsigned_64 := 0; arg5 : Unsigned_64 := 0)
      return Unsigned_64;

   --  Multi-word IPC Wrappers

   --  Blocking receive: returns sender PID in from, message in msg.
   procedure receive (from : out Process_ID; msg : out Message);

   --  Wait for any IPC until an absolute monotonic-millisecond deadline.
   --  received is False on deadline expiry; input publication wakes this
   --  immediately and does not wait for a polling interval.
   procedure receiveUntil
     (deadlineMs : Unsigned_64;
      from       : out Process_ID;
      msg        : out Message;
      received   : out Boolean);

   --  Reply to a sender (unblocks them).
   function reply
     (replyTo : Process_ID; msg : Message) return Unsigned_64;

   --  Reply using a specific saved CAP_REPLY slot.
   --  Consumes a selected reply even if delivery fails (including caller
   --  death). Other capability types are rejected without modification.
   function replyCap
     (slot : CapabilitySlot; msg : Message) return Unsigned_64;

   --  Atomic reply+receive (seL4 ReplyRecv pattern).
   --  Replies to replyTo with replyMsg, then blocks receiving next message.
   procedure replyWait
     (replyTo  : Process_ID;
      replyMsg : Message;
      from     : out Process_ID;
      msg      : in out Message);

   --  Poll_Service_Request
   --
   --  Non-blocking receive for incoming service/client work only.
   --
   --  This is the default receive primitive for servers. It may return:
   --    * a synchronous send/call request, with a reply cap minted;
   --    * an async request submitted with a completion token, with a reply cap
   --      minted for the request ID;
   --    * a one-way service message, with no reply cap minted.
   --
   --  It never consumes keyboard/mouse input, device events, or lifecycle
   --  events. Those belong to Poll_Event/Wait_Event. If found is False, from
   --  is No_Process and msg is NULL_MESSAGE.
   procedure Poll_Service_Request
     (from  : out Process_ID;
      msg   : out Message;
      found : out Boolean);

   --  Poll_Any_Ipc
   --
   --  Non-blocking mixed receive. This is intentionally loud because it can
   --  consume any pending IPC class from the unified mailbox ring: service
   --  requests, one-way messages, and events.
   --
   --  Use this only when implementing a central dispatcher that deliberately
   --  handles all IPC classes itself. Most services should call
   --  Poll_Service_Request, Poll_Event, and Poll_Completion separately.
   procedure Poll_Any_Ipc
     (from  : out Process_ID;
      msg   : out Message;
      found : out Boolean);

   --  Resolve an already-held endpoint, never acquire authority from a PID.
   --  capSubmit revalidates the selected capability's rights and generation.
   procedure Find_Endpoint_Capability
     (Target : Process_ID; Slot : out CapabilitySlot; Found : out Boolean);

   --  Async completion queue wrappers

   --  Block until at least minWait completions available, return up to max.
   --  entries must point to a CompletionRing (or large enough buffer).
   --  Returns the number of completions actually drained.
   function waitCompletion
     (entries : System.Address;
      max     : Unsigned_64;
      min     : Unsigned_64) return Unsigned_64;

   --  Poll_Completion
   --
   --  Non-blocking completion receive for work this process initiated with
   --  capSubmit. Completions are not incoming client requests; they are
   --  the answers to this process' own async operations.
   --
   --  result must point to a CompletionEntry. Returns 1 if found, 0 if empty.
   function Poll_Completion
     (result : System.Address) return Unsigned_64;

   --  Capability-aware IPC wrappers

   --  Synchronous calls wait until Deadline at most: an absolute monotonic
   --  millisecond (Deadline_After), or Wait_Forever, spelled out. There is
   --  no default: the caller says how long it will wait for the server
   --  (docs/ipc-fastpath.md, "Call deadlines"). On expiry the reply tag is
   --  REPLY_TIMEOUT, and the outcome is unknown (the server may still act).
   Wait_Forever : constant Unsigned_64 := Unsigned_64'Last;
   --  Only the kernel gives labels from Kernel_Reply_First on (a server's
   --  reply with one is refused), so they never mean a protocol's status.
   Kernel_Reply_First : constant Unsigned_32 := 16#FFFF_0000#;
   REPLY_TIMEOUT      : constant Unsigned_32 := 16#FFFF_0001#;
   function Deadline_After (Milliseconds : Unsigned_64) return Unsigned_64;

   --  Synchronous send: resolve endpoint cap, stamp authority tag, send.
   function capSend
     (slot : CapabilitySlot; msg : Message; Deadline : Unsigned_64) return MessageTag;

   --  Cap-aware call: resolve cap, send, return full reply via msg pointer.
   function capCall
     (slot : CapabilitySlot; msg : in out Message; Deadline : Unsigned_64) return MessageTag;

   --  Cap-aware async submit: resolve cap, stamp authority tag, submit.
   function capSubmit
     (slot  : CapabilitySlot;
      msg   : Message;
      token : Unsigned_64) return Boolean;

   --  Send async event (non-blocking, intended for interrupt contexts).
   procedure sendEvent (dest : Process_ID; msg : Message);

   --  Send an async event and report bounded-queue backpressure. The kernel
   --  replaces msg.authorityTag with the tag of the authorizing capability;
   --  publication; caller-supplied authority tags are never trusted.
   function trySendEvent (dest : Process_ID; msg : Message) return Boolean;

   --  Blocking receive for unsolicited events. The current ABI returns only
   --  the event tag; migrate this to the unified wait primitive.
   function Wait_Event return Message;

   --  Poll_Event
   --
   --  Non-blocking event receive. Returns True if an event was available and
   --  fills msg with the event payload. It never consumes service requests or
   --  completions.
   function Poll_Event (msg : out Message) return Boolean;

   --  Kill a process by PID. Returns 0 on success, -1 on error.
   function killProcess (pid : Process_ID) return Unsigned_64;

   --  Register a well-known service role in the kernel registry.
   function setWellKnown
     (role : Unsigned_64;
      pid  : Process_ID) return Unsigned_64;

   --  Revoke a shared memory grant.
   procedure revokeGrant (id : Unsigned_64);

   --  Save the reply cap from slot 63 to destSlot (for deferred replies).
   --  Returns 1 on success, 0 on failure.
   function saveReplyCap (destSlot : Unsigned_64) return Unsigned_64;

   --  Port I/O wrappers for userspace drivers.
   function portInp8 (port : Unsigned_16) return Unsigned_64;
   function portOutp8
     (port : Unsigned_16; val : Unsigned_8) return Unsigned_64;
   function portInp16 (port : Unsigned_16) return Unsigned_64;
   function portOutp16
     (port : Unsigned_16; val : Unsigned_16) return Unsigned_64;
   function portInp32 (port : Unsigned_16) return Unsigned_64;
   function portOutp32
     (port : Unsigned_16; val : Unsigned_32) return Unsigned_64;

   --  Translate a virtual address to its physical address
   function virtToPhys (addr : System.Address) return Unsigned_64;

   --  Device manager wrappers

   --  Allocate DMA: contiguous physical pages mapped into target process.
   --  Returns physical address, or -1 on error.
   function allocDma
     (targetPID : Process_ID;
      order     : Unsigned_64;
      virtBase  : Unsigned_64) return Unsigned_64;

   --  Enable IOAPIC routing and register an IRQ subscriber. PCI INTx callers
   --  must request level-triggered, active-low routing; ISA-style sources use
   --  the edge-triggered, active-high defaults.
   function enableIrq
     (vector    : Unsigned_64;
      ownerPID  : Process_ID;
      targetCPU : Unsigned_64;
      levelTriggered : Boolean := False;
      activeLow      : Boolean := False;
      messageSignaled : Boolean := False) return Unsigned_64;

   --  Map physical pages into a target process's address space.
   --  flags: 0=RW, 1=RO, 2=IO (uncacheable)
   function mapInto
     (targetPID : Process_ID;
      physAddr  : Unsigned_64;
      virtAddr  : Unsigned_64;
      numPages  : Unsigned_64;
      flags     : Unsigned_64) return Unsigned_64;

   --  Set a sysinfo query value from userspace.
   function setSysinfo
     (queryID : Unsigned_64;
      value   : Unsigned_64) return Unsigned_64;

   --  Set CPU affinity for a process.
   function setCpu
     (targetPID : Process_ID;
      cpu       : Unsigned_64) return Unsigned_64;

   --  Declare this process' scheduler latency contract. The kernel records
   --  the class, period, and budget now; later scheduler work will use the
   --  same ABI for admission control, deadline ordering, and telemetry.
   function setLatencyContract
     (latencyClass : Unsigned_64;
      periodUs     : Unsigned_64;
      budgetUs     : Unsigned_64;
      flags        : Unsigned_64 := 0) return Unsigned_64;

   --  Legacy/convenience wrappers

   function recvMsg (from : out Unsigned_64) return Unsigned_64;

   function getInfo
     (query : Unsigned_64; detail : Unsigned_64 := 0) return Unsigned_64;

   --  The process registered for a driver or service role (DRIVER_*), or
   --  No_Process (none registered, or the query refused).
   function Registered_Driver (Driver : Unsigned_64) return Process_ID;

   --  This process.
   function Own_Process return Process_ID;

   function registerDriver (driver : Unsigned_64) return Unsigned_64;

   procedure debugPrint (str : String);

   function getSecondaryStack return System.Secondary_Stack.SS_Stack_Ptr
      with Export, Convention => C,
            External_Name => "__gnat_get_secondary_stack";

end CuBit.Messages;
