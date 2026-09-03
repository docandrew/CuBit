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
   SYSCALL_POLL_ANY_IPC    : constant Unsigned_64 := 22;
   SYSCALL_SUBMIT          : constant Unsigned_64 := 23;
   SYSCALL_WAIT_COMPLETION : constant Unsigned_64 := 24;
   SYSCALL_POLL_COMPLETION : constant Unsigned_64 := 25;
   SYSCALL_RECEIVE_EVENT_NB : constant Unsigned_64 := 26;
   SYSCALL_POLL_EVENT      : constant Unsigned_64 := 26;
   SYSCALL_GETTIME         : constant Unsigned_64 := 27;
   SYSCALL_SLEEP           : constant Unsigned_64 := 28;
   SYSCALL_POLL_SERVICE_REQUEST : constant Unsigned_64 := 80;
   SYSCALL_GRANT           : constant Unsigned_64 := 102;
   SYSCALL_REVOKE          : constant Unsigned_64 := 103;
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
   SYSCALL_MINT_CAP        : constant Unsigned_64 := 72;
   SYSCALL_RESUME          : constant Unsigned_64 := 73;

   --  Device manager syscalls
   SYSCALL_ALLOC_DMA       : constant Unsigned_64 := 74;
   SYSCALL_ENABLE_IRQ      : constant Unsigned_64 := 75;
   SYSCALL_MAP_INTO        : constant Unsigned_64 := 76;
   SYSCALL_SET_SYSINFO     : constant Unsigned_64 := 77;
   SYSCALL_SET_CPU         : constant Unsigned_64 := 78;
   SYSCALL_SET_LATENCY_CONTRACT : constant Unsigned_64 := 81;
   SYSCALL_TRACE_RESET     : constant Unsigned_64 := 82;
   SYSCALL_TRACE_SUMMARY   : constant Unsigned_64 := 83;
   SYSCALL_INSPECT_CAP     : constant Unsigned_64 := 84;

   SYSCALL_REGISTER_DRIVER : constant Unsigned_64 := 2000;

   --  Capability-aware IPC syscalls
   SYSCALL_CAP_SEND        : constant Unsigned_64 := 40;
   SYSCALL_CAP_CALL        : constant Unsigned_64 := 41;
   SYSCALL_CAP_SUBMIT      : constant Unsigned_64 := 42;

   --  Notification IPC syscalls
   SYSCALL_NOTIFY          : constant Unsigned_64 := 43;

   --  Atomic reply+receive
   SYSCALL_REPLY_WAIT      : constant Unsigned_64 := 48;

   --  Move reply cap from slot 63 to another slot (deferred replies)
   SYSCALL_SAVE_REPLY_CAP  : constant Unsigned_64 := 51;
   SYSCALL_REPLY_CAP       : constant Unsigned_64 := 52;

   --  Service discovery syscalls
   SYSCALL_GRANT_VIA_CAP   : constant Unsigned_64 := 106;
   SYSCALL_SET_WELL_KNOWN  : constant Unsigned_64 := 107;

   --  Well-known service roles (must match kernel Config.ServiceRole)
   ROLE_FILESYSTEM : constant Unsigned_64 := 1;
   ROLE_PROCMGR    : constant Unsigned_64 := 2;

   --  Well-known capability slots
   CAP_SLOT_SELF      : constant Unsigned_64 := 0;
   CAP_SLOT_FS        : constant Unsigned_64 := 1;
   CAP_SLOT_KEYBOARD  : constant Unsigned_64 := 2;
   CAP_SLOT_SELF_PROC : constant Unsigned_64 := 3;
   CAP_SLOT_ATA       : constant Unsigned_64 := 10;
   CAP_SLOT_NVME      : constant Unsigned_64 := 11;
   CAP_SLOT_NET       : constant Unsigned_64 := 11;
   CAP_SLOT_PROCMGR   : constant Unsigned_64 := 12;
   CAP_SLOT_MIXER     : constant Unsigned_64 := 14;
   CAP_SLOT_MIXER_NTF : constant Unsigned_64 := 15;
   CAP_SLOT_CONFIG    : constant Unsigned_64 := 20;
   CAP_SLOT_DESKTOP   : constant Unsigned_64 := 21;
   CAP_SLOT_DISPLAY   : constant Unsigned_64 := 22;

   subtype CapabilitySlot is Unsigned_64 range 0 .. 63;

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
   SYSINFO_NUM_CPUS           : constant Unsigned_64 := 1400;
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
      badge  : Unsigned_16;
   end record with Size => 64;

   for MessageTag use record
      label  at 0 range 0 .. 31;
      length at 4 range 0 .. 7;
      flags  at 5 range 0 .. 7;
      badge  at 6 range 0 .. 15;
   end record;

   NULL_TAG : constant MessageTag :=
     (label => 0, length => 0, flags => 0, badge => 0);

   type MessageWords is array (0 .. 3) of Unsigned_64;

   type Message is record
      tag      : MessageTag;
      capBadge : Unsigned_64 := 0;
      words    : MessageWords;
   end record;

   NULL_MESSAGE : constant Message :=
     (tag => NULL_TAG, capBadge => 0, words => (others => 0));

   subtype ProcessID is Unsigned_64;
   NO_PROCESS : constant ProcessID := 0;

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
   procedure receive (from : out ProcessID; msg : out Message);

   --  Reply to a sender (unblocks them).
   function reply
     (replyTo : ProcessID; msg : Message) return Unsigned_64;

   --  Reply using a specific saved CAP_REPLY slot.
   function replyCap
     (slot : CapabilitySlot; msg : Message) return Unsigned_64;

   --  Atomic reply+receive (seL4 ReplyRecv pattern).
   --  Replies to replyTo with replyMsg, then blocks receiving next message.
   procedure replyWait
     (replyTo  : ProcessID;
      replyMsg : Message;
      from     : out ProcessID;
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
   --  It never consumes keyboard/mouse input, device events, lifecycle events,
   --  or notifications. Those belong to Poll_Event/Wait_Event or notification
   --  APIs. If found is False, from is NO_PROCESS and msg is NULL_MESSAGE.
   procedure Poll_Service_Request
     (from  : out ProcessID;
      msg   : out Message;
      found : out Boolean);

   --  Poll_Any_Ipc
   --
   --  Non-blocking mixed receive. This is intentionally loud because it can
   --  consume any pending IPC class from the unified mailbox ring: service
   --  requests, one-way messages, events, and notifications.
   --
   --  Use this only when implementing a central dispatcher that deliberately
   --  handles all IPC classes itself. Most services should call
   --  Poll_Service_Request, Poll_Event, and Poll_Completion separately.
   procedure Poll_Any_Ipc
     (from  : out ProcessID;
      msg   : out Message;
      found : out Boolean);

   --  Async non-blocking send (submit + token for completion tracking).
   function submit
     (dest  : ProcessID;
      msg   : Message;
      token : Unsigned_64) return Boolean;

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
   --  submit/capSubmit. Completions are not incoming client requests; they are
   --  the answers to this process' own async operations.
   --
   --  result must point to a CompletionEntry. Returns 1 if found, 0 if empty.
   function Poll_Completion
     (result : System.Address) return Unsigned_64;

   --  pollCompletion
   --
   --  Compatibility spelling for Poll_Completion.
   function pollCompletion
     (result : System.Address) return Unsigned_64;

   --  Capability-aware IPC wrappers

   --  Cap-aware synchronous send: resolve endpoint cap, stamp badge, send.
   function capSend
     (slot : CapabilitySlot; msg : Message) return MessageTag;

   --  Cap-aware call: resolve cap, send, return full reply via msg pointer.
   function capCall
     (slot : CapabilitySlot; msg : in out Message) return MessageTag;

   --  Cap-aware async submit: resolve cap, stamp badge, submit.
   function capSubmit
     (slot  : CapabilitySlot;
      msg   : Message;
      token : Unsigned_64) return Boolean;

   --  Notification IPC wrappers

   --  Signal a notification cap: OR badge into dest's notifyWord.
   procedure capNotify (slot : CapabilitySlot);

   --  Send async event (non-blocking, intended for interrupt contexts).
   procedure sendEvent (dest : ProcessID; msg : Message);

   --  Blocking receive for unsolicited events. The current ABI returns only
   --  the event tag; migrate this to the unified wait primitive.
   function Wait_Event return Message;

   --  Compatibility spelling for Wait_Event.
   function receiveEvent return Message;

   --  Poll_Event
   --
   --  Non-blocking event receive. Returns True if an event was available and
   --  fills msg with the event payload. It never consumes service requests or
   --  completions.
   function Poll_Event (msg : out Message) return Boolean;

   --  Non-blocking receive event. Returns True if an event was available.
   --  Compatibility spelling for Poll_Event.
   function receiveEventNB (msg : out Message) return Boolean;

   --  Create a shared memory grant.
   procedure createGrant
     (grantee   : ProcessID;
      localAddr : System.Address;
      numPages  : Natural;
      readWrite : Boolean;
      grantId   : out Unsigned_64;
      success   : out Boolean);

   --  Kill a process by PID. Returns 0 on success, -1 on error.
   function killProcess (pid : ProcessID) return Unsigned_64;

   --  Create a shared memory grant via capability slot (no raw PID needed).
   --  slot = CAP_ENDPOINT slot identifying grantee.
   procedure createGrantViaCap
     (slot      : CapabilitySlot;
      localAddr : System.Address;
      numPages  : Natural;
      readWrite : Boolean;
      grantId   : out Unsigned_64;
      success   : out Boolean);

   --  Register a well-known service role in the kernel registry.
   function setWellKnown
     (role : Unsigned_64;
      pid  : Unsigned_64) return Unsigned_64;

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
     (targetPID : Unsigned_64;
      order     : Unsigned_64;
      virtBase  : Unsigned_64) return Unsigned_64;

   --  Enable IOAPIC routing and register an IRQ subscriber. PCI INTx callers
   --  must request level-triggered, active-low routing; ISA-style sources use
   --  the edge-triggered, active-high defaults.
   function enableIrq
     (vector    : Unsigned_64;
      ownerPID  : Unsigned_64;
      targetCPU : Unsigned_64;
      levelTriggered : Boolean := False;
      activeLow      : Boolean := False;
      messageSignaled : Boolean := False) return Unsigned_64;

   --  Map physical pages into a target process's address space.
   --  flags: 0=RW, 1=RO, 2=IO (uncacheable)
   function mapInto
     (targetPID : Unsigned_64;
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
     (targetPID : Unsigned_64;
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

   function registerDriver (driver : Unsigned_64) return Unsigned_64;

   procedure debugPrint (str : String);

   function getSecondaryStack return System.Secondary_Stack.SS_Stack_Ptr
      with Export, Convention => C,
            External_Name => "__gnat_get_secondary_stack";

end CuBit.Messages;
