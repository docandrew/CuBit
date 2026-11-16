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
with CuBit.Handles;
with Capability_ABI;

package CuBit.Messages is

   --  Syscall Numbers

   SYSCALL_EXIT            : constant Unsigned_64 := 0;
   SYSCALL_GETPID          : constant Unsigned_64 := 6;
   SYSCALL_SBRK            : constant Unsigned_64 := 8;
   SYSCALL_WRITE           : constant Unsigned_64 := 12;
   SYSCALL_RECEIVE         : constant Unsigned_64 := 17;
   SYSCALL_REPLY           : constant Unsigned_64 := 18;
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
   --  Owned memory, one region API (kernel Process.Owned_Memory, ABI-002);
   --  CuBit.Owned_Reservations wraps these. Reserve: arg0 page-aligned
   --  bytes (up to 2 GiB), arg1 alignment (0: a page); the base, or zero.
   --  No RAM. Change: arg0 address, arg1 bytes (a page range of one
   --  region), arg2 operation (CuBit.Kernel_ABI.Owned_Range_Operation);
   --  zero, or Last. Release: arg0 base, arg1 exact capacity; zero, or
   --  Last. A release also gives back a read-only boot-image view range.
   --  Existing grant acquisitions retain their backing until returned.
   SYSCALL_RESERVE_OWNED_REGION : constant Unsigned_64 := 146;
   SYSCALL_CHANGE_OWNED_RANGE   : constant Unsigned_64 := 147;
   SYSCALL_RELEASE_OWNED_REGION : constant Unsigned_64 := 148;
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

   --  Every system call that takes or returns a capability handle is
   --  reached through CuBit.Handles and its children (CAP-002): their
   --  numbers are not here, so no program can pass a number for a handle.

   SYSCALL_SET_LATENCY_CONTRACT : constant Unsigned_64 := 81;
   SYSCALL_TRACE_RESET     : constant Unsigned_64 := 82;
   SYSCALL_TRACE_SUMMARY   : constant Unsigned_64 := 83;

   --  Atomic reply+receive
   SYSCALL_REPLY_WAIT      : constant Unsigned_64 := 48;

   --  Returns zero for an owned inactive slot; Last for invalid/foreign slots.
   --  Nonzero generation includes pending revocation, not just available use.
   SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION :
      constant Unsigned_64 := 108;
   SYSCALL_ACQUIRE_SHARED_MEMORY_GRANT : constant Unsigned_64 := 109;
   SYSCALL_REVOKE_SHARED_MEMORY_GRANT_REFERENCE :
      constant Unsigned_64 := 110;
   SYSCALL_RETURN_SHARED_MEMORY_GRANT_ACQUISITION :
      constant Unsigned_64 := 111;
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

   --  Maximum work accepted by one privileged MAP_INTO syscall.  Callers
   --  map larger regions in bounded chunks so the kernel remains responsive.
   MAX_MAP_INTO_PAGES_PER_CALL : constant Unsigned_64 := 1024;

   STDOUT : constant Unsigned_64 := 1;

   --  Service roles of the kernel's service registry (kernel
   --  Service_Registry; read with Registered_Driver).
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
   --  lexicon.svc (docs/lexicon.md); 20..27 are taken by roles declared
   --  with their protocols (mixer control, TLS, metrics, ...).
   DRIVER_LEXICON  : constant Unsigned_64 := 28;
   --  acpi.svc: the ACPI namespace service; its clients' endpoints carry
   --  ACPI_Protocol.Observer_Tag (shared/acpi).
   DRIVER_ACPI     : constant Unsigned_64 := 29;
   --  wifi.svc (docs/wifi.md): Wi-Fi drivers attach to it
   --  (CuBit.Wifi_Driver); its controllers call it (CuBit.Wifi_Control).
   --  30 .. 32 are taken by roles declared with their protocols.
   DRIVER_WIFI     : constant Unsigned_64 := 33;
   --  acpi-backend.svc's named ACPI registers (34 is gpio.svc's): acpi.svc's
   --  endpoint carries ACPI_Register_Roles.Frontend_Tag (shared/acpi),
   --  minted only for a startup entry approving (acpi-registers
   --  approve-declared).
   DRIVER_ACPI_REGISTERS : constant Unsigned_64 := 35;

   --  Scheduler latency classes. Values match kernel Process.LatencyClass.
   LATENCY_BACKGROUND  : constant Unsigned_64 := 0;
   LATENCY_NORMAL      : constant Unsigned_64 := 1;
   LATENCY_INTERACTIVE : constant Unsigned_64 := 2;
   LATENCY_REALTIME    : constant Unsigned_64 := 3;

   --  IPC Message Types (matching kernel Process.Message)

   --  The tag's flag byte is two typed components (Capability_ABI): the
   --  protocol's low nibble, and the IPC layer's flags, set only by name.
   --  Declared here (derived) so a "use CuBit.Messages" sees their
   --  operations.
   type Protocol_Flags is new Capability_ABI.Protocol_Flags;
   type IPC_Flags is new Capability_ABI.IPC_Flags;
   No_IPC_Flags : constant IPC_Flags := (others => False);

   type MessageTag is record
      label    : Unsigned_32;
      length   : Unsigned_8;
      protocol : Protocol_Flags := 0;
      ipc      : IPC_Flags := No_IPC_Flags;
      reserved : Unsigned_16; -- Not authenticated; never an authority tag
   end record with Size => 64;

   for MessageTag use record
      label    at 0 range 0 .. 31;
      length   at 4 range 0 .. 7;
      protocol at 5 range 0 .. 3;
      ipc      at 5 range 4 .. 7;
      reserved at 6 range 0 .. 15;
   end record;

   NULL_TAG : constant MessageTag :=
     (label => 0, length => 0, protocol => 0, ipc => No_IPC_Flags, reserved => 0);

   --  The tag's whole flag byte, for decoders that validate it exactly.
   function Flag_Byte (Tag : MessageTag) return Unsigned_8 is
     (Capability_ABI.Flag_Byte (Capability_ABI.Protocol_Flags (Tag.protocol),
                                Capability_ABI.IPC_Flags (Tag.ipc)));

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

   --  The same wait, but a pending kernel notice (a channel wake, grant
   --  lifecycle, child exit) comes back in Event (Taken) in the same call,
   --  so a woken thread pays one system call. Only kernel notices are
   --  taken; other work is still Work_Available.
   type Wake_Result is (Event_Taken, Work_Available, Deadline_Reached, Unavailable);
   procedure Wait_For_Activity_Or_Event
     (Deadline : Unsigned_64; Event : aliased out Message; Result : out Wake_Result);

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

   --  Stop (docs/data-plane.md, "Control messages"; docs/power-states.md,
   --  3.2): how this program answers the kernel's Stop control message,
   --  which its supervisor (procmgr, devmgr) sends to end it cleanly.
   --    Program_Reads_Stop: the program reads Stop itself (with
   --      CuBit.Process_Events or its own event decoding); the runtime does
   --      nothing with it. Every program starts here, so one that already
   --      handles Stop is unchanged.
   --    Exit_At_Once: the program has nothing to finish. When a receive
   --      here (receive, receiveUntil, replyWait, Poll_Any_Ipc, Poll_Event,
   --      Wait_For_Activity_Or_Event) or CuBit.Process_Events returns the
   --      Stop, the runtime ends the process with Stopped_Exit_Code.
   --    Quiesce_Then_Exit: the same, after the program's Quiesce step
   --      (stop its device's engines, commit its state). It runs where the
   --      receive saw the Stop, so it must not rely on that receive's
   --      caller finishing first.
   --  A program declares one of the last two once, before its first
   --  receive. Wait_For_Activity_Until takes no event: a loop that waits
   --  with it alone reads its kernel notices with
   --  CuBit.Process_Events.Serve_Defaults, so that a Stop reaches here.
   type Stop_Disposition is (Program_Reads_Stop, Exit_At_Once, Quiesce_Then_Exit);
   type Quiesce_Procedure is access procedure;
   Stopped_Exit_Code : constant Unsigned_64 := 0;

   procedure Exit_On_Stop;
   procedure On_Stop (Quiesce : not null Quiesce_Procedure);
   function Stop_Answer return Stop_Disposition;

   --  The kernel's control message (kernel ipc_labels.ads EVENT_CONTROL;
   --  CuBit.Control_Events, which CuBit.Process_Events checks these
   --  against at compile time): its label, its length, and Stop's kind.
   Control_Event_Label  : constant := 16#010E#;
   Control_Event_Length : constant := 2;
   Stop_Control_Kind    : constant := 1;

   --  Whether what a receive returned is the kernel's Stop. Only the kernel
   --  posts the label: it refuses a process's events carrying it, and a
   --  process's call carrying it has a sender (From /= No_Process).
   function Is_Kernel_Stop
     (Label : Unsigned_32; Length : Unsigned_8; From : Process_ID; Kind : Unsigned_64)
      return Boolean is
     (Label = Control_Event_Label and then Length = Control_Event_Length
      and then From = No_Process and then Kind = Stop_Control_Kind);

   --  Called with what a receive returned (From: its sender, No_Process
   --  for a kernel notice): the kernel's Stop, under Exit_At_Once or
   --  Quiesce_Then_Exit, ends the process here and does not return.
   --  Anything else returns at once.
   procedure Answer_Stop (Msg : Message; From : Process_ID);

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

   --  Answer a request saved with CuBit.Handles.IPC.Save_Reply. The saved
   --  reply is used up even if delivery fails (including caller death).
   --  Nonzero when delivered.
   function replyCap
     (saved : CuBit.Handles.Reply; msg : Message) return Unsigned_64;

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
   --  events. Those belong to Poll_Event. If found is False, from
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
   --  The request reached its server (or its queue) and will never be
   --  answered: the server received again or closed the reply without
   --  answering, the thread holding it exited, or the server ended
   --  (IPC-000 rule 3). A server's own reply never carries it.
   REPLY_ENDED        : constant Unsigned_32 := 16#FFFF_0002#;

   --  How a synchronous call came back, typed: test this, not labels.
   --  Call_Answered: the server's reply (any label but 0). Call_Ended:
   --  REPLY_ENDED, never a server's own label. Call_Timed_Out:
   --  REPLY_TIMEOUT (the server may still act). Call_Not_Delivered: label
   --  0, what a call refused before delivery returns (no or stale
   --  capability, a server already gone). A protocol whose server itself
   --  answers label 0 reads Call_Not_Delivered as that answer by its own
   --  rule.
   type Call_Ending is (Call_Answered, Call_Ended, Call_Timed_Out, Call_Not_Delivered);
   function Ending_Of (Tag : MessageTag) return Call_Ending is
     (if Tag.label = REPLY_ENDED then Call_Ended
      elsif Tag.label = REPLY_TIMEOUT then Call_Timed_Out
      elsif Tag.label = 0 then Call_Not_Delivered
      else Call_Answered);
   function Deadline_After (Milliseconds : Unsigned_64) return Unsigned_64;

   --  Synchronous send: resolve endpoint cap, stamp authority tag, send.
   function capSend
     (target : CuBit.Handles.Endpoint; msg : Message; Deadline : Unsigned_64)
      return MessageTag;

   --  Cap-aware call: resolve cap, send, return full reply via msg pointer.
   function capCall
     (target : CuBit.Handles.Endpoint; msg : in out Message; Deadline : Unsigned_64)
      return MessageTag;

   --  Cap-aware async submit: resolve cap, stamp authority tag, submit.
   function capSubmit
     (target : CuBit.Handles.Endpoint;
      msg    : Message;
      token  : Unsigned_64) return Boolean;

   --  Send an async event (non-blocking, intended for interrupt contexts)
   --  through the capability that authorizes it: a notification covering a
   --  role dest is registered for, or an endpoint to dest.
   procedure sendEvent
     (dest : Process_ID; msg : Message; authority : CuBit.Handles.Notification);
   procedure sendEvent
     (dest : Process_ID; msg : Message; authority : CuBit.Handles.Endpoint);

   --  Send an async event and report bounded-queue backpressure. The kernel
   --  replaces msg.authorityTag with the tag of the authorizing capability;
   --  caller-supplied authority tags are never trusted.
   function trySendEvent
     (dest : Process_ID; msg : Message; authority : CuBit.Handles.Notification)
      return Boolean;
   function trySendEvent
     (dest : Process_ID; msg : Message; authority : CuBit.Handles.Endpoint)
      return Boolean;

   --  Poll_Event
   --
   --  Non-blocking event receive. Returns True if an event was available and
   --  fills msg with the event payload. It never consumes service requests or
   --  completions.
   function Poll_Event (msg : out Message) return Boolean;
   --  The same, with the event's publisher as the kernel reports it
   --  (No_Process for the kernel's own notices): who sent it, which a
   --  receiver authenticates by; the authority tag is the capability's badge.
   function Poll_Event (msg : out Message; publisher : out Process_ID) return Boolean;

   --  Kill a process, with process authority (Write) over it. Returns 0 on
   --  success, -1 on error.
   function killProcess
     (pid : Process_ID; authority : CuBit.Handles.Process) return Unsigned_64;

   --  Revoke a shared memory grant.
   procedure revokeGrant (id : Unsigned_64);

   --  Port I/O for userspace drivers, through a CAP_IOPORT handle covering
   --  the port. Reads return the value, writes 0; -1 when refused.
   function portInp8 (ports : CuBit.Handles.IO_Ports; port : Unsigned_16) return Unsigned_64;
   function portOutp8
     (ports : CuBit.Handles.IO_Ports; port : Unsigned_16; val : Unsigned_8) return Unsigned_64;
   function portInp16 (ports : CuBit.Handles.IO_Ports; port : Unsigned_16) return Unsigned_64;
   function portOutp16
     (ports : CuBit.Handles.IO_Ports; port : Unsigned_16; val : Unsigned_16) return Unsigned_64;
   function portInp32 (ports : CuBit.Handles.IO_Ports; port : Unsigned_16) return Unsigned_64;
   function portOutp32
     (ports : CuBit.Handles.IO_Ports; port : Unsigned_16; val : Unsigned_32) return Unsigned_64;

   --  Device manager wrappers

   --  Legacy interrupt lines: CuBit.Handles.Lines.

   --  Map pages into a target process's address space (privileged: the
   --  caller holds CAP_PROCESS with RIGHT_GRANT for it). Kind
   --  MAP_INTO_BOOT_IMAGE: Source is a page-aligned byte offset into the
   --  boot image (initrd), mapped read-only. MAP_INTO_DEVICE: Source is the
   --  physical address of device pages the caller holds CAP_DEVICE_MEM
   --  for, mapped uncached.
   MAP_INTO_BOOT_IMAGE : constant Unsigned_64 := 1;
   MAP_INTO_DEVICE     : constant Unsigned_64 := 2;
   function mapInto
     (targetPID : Process_ID;
      physAddr  : Unsigned_64;
      virtAddr  : Unsigned_64;
      numPages  : Unsigned_64;
      flags     : Unsigned_64;
      authority : CuBit.Handles.Process;
      region    : CuBit.Handles.Device_Memory) return Unsigned_64;

   --  Set CPU affinity for a process.
   function setCpu
     (targetPID : Process_ID;
      cpu       : Unsigned_64;
      authority : CuBit.Handles.Process) return Unsigned_64;

   --  Declare this process' scheduler latency contract. The kernel records
   --  the class, period, and budget now; later scheduler work will use the
   --  same ABI for admission control, deadline ordering, and telemetry.
   function setLatencyContract
     (latencyClass : Unsigned_64;
      periodUs     : Unsigned_64;
      budgetUs     : Unsigned_64;
      flags        : Unsigned_64 := 0) return Unsigned_64;

   --  The process registered for a driver or service role (DRIVER_*), or
   --  No_Process: read from the kernel's registry publication
   --  (CuBit.System_Facts), no system call.
   function Registered_Driver (Driver : Unsigned_64) return Process_ID;

   --  This process.
   function Own_Process return Process_ID;

   --  Register for a service role, with a notification handle (Write)
   --  covering it. This process's identity word, or -1.
   function registerDriver
     (driver : Unsigned_64; authority : CuBit.Handles.Notification) return Unsigned_64;

   procedure debugPrint (str : String);

end CuBit.Messages;
