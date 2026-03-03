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
   SYSCALL_READ            : constant Unsigned_64 := 1;
   SYSCALL_CLOSE           : constant Unsigned_64 := 2;
   SYSCALL_SBRK            : constant Unsigned_64 := 8;
   SYSCALL_WRITE           : constant Unsigned_64 := 12;
   SYSCALL_OPEN            : constant Unsigned_64 := 13;
   SYSCALL_INFO            : constant Unsigned_64 := 15;
   SYSCALL_SEND            : constant Unsigned_64 := 16;
   SYSCALL_RECEIVE         : constant Unsigned_64 := 17;
   SYSCALL_REPLY           : constant Unsigned_64 := 18;
   SYSCALL_SEND_EVENT      : constant Unsigned_64 := 19;
   SYSCALL_RECEIVE_EVENT   : constant Unsigned_64 := 20;
   SYSCALL_CALL            : constant Unsigned_64 := 21;
   SYSCALL_RECEIVE_NB      : constant Unsigned_64 := 22;
   SYSCALL_SUBMIT          : constant Unsigned_64 := 23;
   SYSCALL_WAIT_COMPLETION : constant Unsigned_64 := 24;
   SYSCALL_POLL_COMPLETION : constant Unsigned_64 := 25;
   SYSCALL_RECEIVE_EVENT_NB : constant Unsigned_64 := 26;
   SYSCALL_GETTIME         : constant Unsigned_64 := 27;
   SYSCALL_SLEEP           : constant Unsigned_64 := 28;
   SYSCALL_GRANT           : constant Unsigned_64 := 102;
   SYSCALL_REVOKE          : constant Unsigned_64 := 103;
   SYSCALL_INB             : constant Unsigned_64 := 30;
   SYSCALL_OUTB            : constant Unsigned_64 := 31;
   SYSCALL_INW             : constant Unsigned_64 := 32;
   SYSCALL_OUTW            : constant Unsigned_64 := 33;
   SYSCALL_INS16           : constant Unsigned_64 := 34;
   SYSCALL_OUTS16          : constant Unsigned_64 := 35;
   SYSCALL_REGISTER_DRIVER : constant Unsigned_64 := 2000;

   --  Capability-aware IPC syscalls
   SYSCALL_CAP_SEND        : constant Unsigned_64 := 40;
   SYSCALL_CAP_CALL        : constant Unsigned_64 := 41;
   SYSCALL_CAP_SUBMIT      : constant Unsigned_64 := 42;

   --  Notification IPC syscalls
   SYSCALL_NOTIFY          : constant Unsigned_64 := 43;
   SYSCALL_NOTIFY_WAIT     : constant Unsigned_64 := 44;
   SYSCALL_NOTIFY_POLL     : constant Unsigned_64 := 45;

   --  Notification binding
   SYSCALL_BIND_NOTIFICATION   : constant Unsigned_64 := 46;
   SYSCALL_UNBIND_NOTIFICATION : constant Unsigned_64 := 47;

   --  Atomic reply+receive
   SYSCALL_REPLY_WAIT      : constant Unsigned_64 := 48;

   --  Access Controller syscalls
   SYSCALL_CONTROLACCESS   : constant Unsigned_64 := 100;
   SYSCALL_GETTICKET       : constant Unsigned_64 := 101;

   --  Well-known capability slots
   CAP_SLOT_SELF      : constant Unsigned_64 := 0;
   CAP_SLOT_FS        : constant Unsigned_64 := 1;
   CAP_SLOT_KEYBOARD  : constant Unsigned_64 := 2;
   CAP_SLOT_SELF_PROC : constant Unsigned_64 := 3;
   CAP_SLOT_ATA       : constant Unsigned_64 := 10;

   subtype CapabilitySlot is Unsigned_64 range 0 .. 63;

   STDOUT : constant Unsigned_64 := 1;

   --  Sysinfo query IDs (must match kernel/src/sysinfo.ads)
   SYSINFO_RAMDISK_ADDRESS    : constant Unsigned_64 := 1000;
   SYSINFO_SECONDARY_STACK    : constant Unsigned_64 := 1001;
   SYSINFO_REGISTERED_DRIVER  : constant Unsigned_64 := 2000;

   --  Driver IDs for SYSINFO_REGISTERED_DRIVER queries
   DRIVER_KEYBOARD : constant Unsigned_64 := 1;
   DRIVER_ATA      : constant Unsigned_64 := 2;

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

   --  Raw syscall wrapper

   function syscall
     (call : Unsigned_64; arg0 : Unsigned_64 := 0; arg1 : Unsigned_64 := 0;
      arg2 : Unsigned_64 := 0; arg3 : Unsigned_64 := 0;
      arg4 : Unsigned_64 := 0; arg5 : Unsigned_64 := 0)
      return Unsigned_64;

   --  Multi-word IPC Wrappers

   --  Synchronous send: block until reply. Returns the reply message tag.
   function send (dest : ProcessID; msg : Message) return MessageTag;

   --  Blocking receive: returns sender PID in from, message in msg.
   procedure receive (from : out ProcessID; msg : out Message);

   --  Reply to a sender (unblocks them).
   function reply
     (replyTo : ProcessID; msg : Message) return Unsigned_64;

   --  Atomic reply+receive (seL4 ReplyRecv pattern).
   --  Replies to replyTo with replyMsg, then blocks receiving next message.
   procedure replyWait
     (replyTo  : ProcessID;
      replyMsg : Message;
      from     : out ProcessID;
      msg      : in out Message);

   --  Non-blocking receive: returns True if a message was available.
   procedure receiveNB
     (from  : out ProcessID;
      msg   : out Message;
      found : out Boolean);

   --  Async non-blocking send (submit + token for completion tracking).
   function submit
     (dest  : ProcessID;
      msg   : Message;
      token : Unsigned_64) return Boolean;

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

   --  Block until notifyWord is non-zero, return and clear it.
   function notifyWait return Unsigned_64;

   --  Non-blocking poll: return notifyWord (0 if none), clear it.
   function notifyPoll return Unsigned_64;

   --  Bind a notification to the calling process. When blocked in receive(),
   --  the process will also be woken by signals to this notification.
   procedure bindNotification (notifPID : ProcessID);

   --  Remove the notification binding from the calling process.
   procedure unbindNotification;

   --  Send async event (non-blocking, intended for interrupt contexts).
   procedure sendEvent (dest : ProcessID; msg : Message);

   --  Blocking receive event.
   function receiveEvent return Message;

   --  Non-blocking receive event. Returns True if an event was available.
   function receiveEventNB (msg : out Message) return Boolean;

   --  Create a shared memory grant.
   procedure createGrant
     (grantee   : ProcessID;
      localAddr : System.Address;
      numPages  : Natural;
      readWrite : Boolean;
      grantId   : out Unsigned_64;
      success   : out Boolean);

   --  Revoke a shared memory grant.
   procedure revokeGrant (id : Unsigned_64);

   --  Port I/O wrappers for userspace drivers.
   function portIn8 (port : Unsigned_16) return Unsigned_64;
   function portOut8 (port : Unsigned_16; val : Unsigned_8) return Unsigned_64;
   function portIn16 (port : Unsigned_16) return Unsigned_64;
   function portOut16
     (port : Unsigned_16; val : Unsigned_16) return Unsigned_64;
   function portIns16
     (port  : Unsigned_16;
      addr  : System.Address;
      count : Unsigned_32) return Unsigned_64;

   --  Legacy/convenience wrappers

   function sendMsg
     (to : Unsigned_64; msg : Unsigned_64) return Unsigned_64;

   function recvMsg (from : out Unsigned_64) return Unsigned_64;

   function getInfo
     (query : Unsigned_64; detail : Unsigned_64 := 0) return Unsigned_64;

   function registerDriver (driver : Unsigned_64) return Unsigned_64;

   procedure debugPrint (str : String);

   function getSecondaryStack return System.Secondary_Stack.SS_Stack_Ptr
      with Export, Convention => C,
            External_Name => "__gnat_get_secondary_stack";

end CuBit.Messages;
