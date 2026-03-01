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

   STDOUT : constant Unsigned_64 := 1;

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
      tag   : MessageTag;
      words : MessageWords;
   end record;

   NULL_MESSAGE : constant Message :=
     (tag => NULL_TAG, words => (others => 0));

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

   --  Send async event (non-blocking, intended for interrupt contexts).
   procedure sendEvent (dest : ProcessID; msg : Message);

   --  Blocking receive event.
   function receiveEvent return Message;

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
