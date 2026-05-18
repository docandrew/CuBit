------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2021 Jon Andrew
--
--  @summary
--  IPC Messages / Syscalls
--
--  Full multi-word IPC wrappers matching kernel Process.IPC.
------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with System;
with System.Machine_Code; use System.Machine_Code;

package body CuBit.Messages is

   --  syscall
   --  Ada interface to syscall instruction (x86_64)
   --
   --  Note: The syscall instruction clobbers RCX (return address) and
   --  R11 (RFLAGS). The kernel entry moves R10 -> RCX for arg3, so arg3
   --  must go into R10 from userspace.

   function syscall
     (call : Unsigned_64; arg0 : Unsigned_64 := 0; arg1 : Unsigned_64 := 0;
      arg2 : Unsigned_64 := 0; arg3 : Unsigned_64 := 0;
      arg4 : Unsigned_64 := 0; arg5 : Unsigned_64 := 0) return Unsigned_64
   is
      use ASCII;

      ret : Unsigned_64;
   begin
      --  Use proper register constraints so the compiler places call in rax,
      --  arg0 in rdi, arg1 in rsi, arg2 in rdx.  For r10/r8/r9, which lack
      --  single-letter constraints, we use explicit mov from "rm" operands.
      --  The "=a" output captures the return value from rax in the same asm
      --  block, avoiding the split-block bug where rax could be clobbered
      --  between two separate Asm statements.
      Asm
        ("mov %5, %%r10" & LF &
         "mov %6, %%r8" & LF &
         "mov %7, %%r9" & LF &
         "syscall",
         Outputs => (Unsigned_64'Asm_Output ("=a", ret)),
         Inputs =>
           (Unsigned_64'Asm_Input ("a", call),
            Unsigned_64'Asm_Input ("D", arg0),
            Unsigned_64'Asm_Input ("S", arg1),
            Unsigned_64'Asm_Input ("d", arg2),
            Unsigned_64'Asm_Input ("rm", arg3),
            Unsigned_64'Asm_Input ("rm", arg4),
            Unsigned_64'Asm_Input ("rm", arg5)),
         Clobber => "r10, rcx, r8, r9, r11, memory",
         Volatile => True);

      return ret;
   end syscall;

   --  Conversion helpers

   function tagToU64 is new Ada.Unchecked_Conversion
      (MessageTag, Unsigned_64);
   function u64ToTag is new Ada.Unchecked_Conversion
      (Unsigned_64, MessageTag);
   function toNum is new Ada.Unchecked_Conversion
      (System.Address, Unsigned_64);

   --  send
   --  SEND: RDI=dest, RSI=tag, RDX=w0, R10=w1, R8=w2, R9=w3
   --  Returns: reply tag in RAX

   function send (dest : ProcessID; msg : Message) return MessageTag is
      retTag : Unsigned_64;
   begin
      retTag := syscall (SYSCALL_SEND,
                          dest,
                          tagToU64 (msg.tag),
                          msg.words (0),
                          msg.words (1),
                          msg.words (2),
                          msg.words (3));
      return u64ToTag (retTag);
   end send;

   --  receive
   --  RECEIVE: RDI=pointer to Message struct
   --  Returns: RAX=sender PID

   procedure receive (from : out ProcessID; msg : out Message) is
   begin
      from := syscall (SYSCALL_RECEIVE, toNum (msg'Address));
   end receive;

   --  reply
   --  REPLY: RDI=dest, RSI=tag, RDX=w0, R10=w1, R8=w2, R9=w3

   function reply
     (replyTo : ProcessID; msg : Message) return Unsigned_64
   is
   begin
      return syscall (SYSCALL_REPLY,
                       replyTo,
                       tagToU64 (msg.tag),
                       msg.words (0),
                       msg.words (1),
                       msg.words (2),
                       msg.words (3));
   end reply;

   --  replyCap
   --  REPLY_CAP: RDI=reply cap slot, RSI=tag, RDX=w0, R10=w1, R8=w2, R9=w3

   function replyCap
     (slot : CapabilitySlot; msg : Message) return Unsigned_64
   is
   begin
      return syscall (SYSCALL_REPLY_CAP,
                       slot,
                       tagToU64 (msg.tag),
                       msg.words (0),
                       msg.words (1),
                       msg.words (2),
                       msg.words (3));
   end replyCap;

   --  replyWait
   --  REPLY_WAIT: RDI=replyTo, RSI=pointer to Message
   --  Returns: RAX=sender PID of next received message

   procedure replyWait
     (replyTo  : ProcessID;
      replyMsg : Message;
      from     : out ProcessID;
      msg      : in out Message)
   is
   begin
      msg := replyMsg;
      from := syscall (SYSCALL_REPLY_WAIT, replyTo, toNum (msg'Address));
   end replyWait;

   --  Poll_Service_Request
   --  POLL_SERVICE_REQUEST: RDI=pointer to Message struct
   --  Returns: RAX=sender PID (0 if no service request)
   --
   --  This wrapper is intentionally semantic rather than queue-shaped. The
   --  kernel still stores several IPC classes in one mailbox ring, but this
   --  syscall asks for only request-like work destined for a service.
   procedure Poll_Service_Request
     (from  : out ProcessID;
      msg   : out Message;
      found : out Boolean)
   is
      ret : Unsigned_64;
   begin
      msg := NULL_MESSAGE;
      ret := syscall (SYSCALL_POLL_SERVICE_REQUEST, toNum (msg'Address));
      from := ret;
      found := (ret /= 0);
   end Poll_Service_Request;

   --  Poll_Any_Ipc
   --  POLL_ANY_IPC: RDI=pointer to Message struct
   --  Returns: RAX=sender PID, or 0 if no mixed IPC was available.
   --
   --  This preserves the old receiveNB behavior. It can consume events and
   --  notifications, so call sites should look unusual on purpose.
   procedure Poll_Any_Ipc
     (from  : out ProcessID;
      msg   : out Message;
      found : out Boolean)
   is
      ret : Unsigned_64;
   begin
      msg := NULL_MESSAGE;
      ret := syscall (SYSCALL_POLL_ANY_IPC, toNum (msg'Address));
      from := ret;
      found := (ret /= 0);
   end Poll_Any_Ipc;

   --  receiveNB
   --  Deprecated compatibility spelling for Poll_Any_Ipc.

   procedure receiveNB
     (from  : out ProcessID;
      msg   : out Message;
      found : out Boolean)
   is
   begin
      Poll_Any_Ipc (from, msg, found);
   end receiveNB;

   --  submit
   --  SUBMIT: RDI=dest, RSI=tag, RDX=w0, R10=w1, R8=w2, R9=token
   --  Note: w3 is sacrificed to pass token in R9

   function submit
     (dest  : ProcessID;
      msg   : Message;
      token : Unsigned_64) return Boolean
   is
      ret : Unsigned_64;
   begin
      ret := syscall (SYSCALL_SUBMIT,
                       dest,
                       tagToU64 (msg.tag),
                       msg.words (0),
                       msg.words (1),
                       msg.words (2),
                       token);
      return (ret = 1);
   end submit;

   --  waitCompletion
   --  WAIT_COMPLETION: RDI=pointer to buffer, RSI=maxEntries, RDX=minWait
   --  Returns: RAX=numReturned

   function waitCompletion
     (entries : System.Address;
      max     : Unsigned_64;
      min     : Unsigned_64) return Unsigned_64
   is
   begin
      return syscall (SYSCALL_WAIT_COMPLETION, toNum (entries), max, min);
   end waitCompletion;

   --  Poll_Completion
   --  POLL_COMPLETION: RDI=pointer to CompletionEntry
   --  Returns: RAX=1 if found, 0 if empty

   function Poll_Completion
     (result : System.Address) return Unsigned_64
   is
   begin
      return syscall (SYSCALL_POLL_COMPLETION, toNum (result));
   end Poll_Completion;

   function pollCompletion
     (result : System.Address) return Unsigned_64
   is
   begin
      return Poll_Completion (result);
   end pollCompletion;

   --  capSend
   --  CAP_SEND: RDI=cap_slot, RSI=tag, RDX=w0, R10=w1, R8=w2, R9=w3
   --  Returns: reply tag in RAX

   function capSend
     (slot : CapabilitySlot; msg : Message) return MessageTag
   is
      retTag : Unsigned_64;
   begin
      retTag := syscall (SYSCALL_CAP_SEND,
                          slot,
                          tagToU64 (msg.tag),
                          msg.words (0),
                          msg.words (1),
                          msg.words (2),
                          msg.words (3));
      return u64ToTag (retTag);
   end capSend;

   --  capCall
   --  CAP_CALL: RDI=cap_slot, RSI=pointer to Message struct (in/out)
   --  Returns: reply tag in RAX

   function capCall
     (slot : CapabilitySlot; msg : in out Message) return MessageTag
   is
      retTag : Unsigned_64;
   begin
      retTag := syscall (SYSCALL_CAP_CALL, slot, toNum (msg'Address));
      return u64ToTag (retTag);
   end capCall;

   --  capSubmit
   --  CAP_SUBMIT: RDI=cap_slot, RSI=tag, RDX=w0, R10=w1, R8=w2, R9=token

   function capSubmit
     (slot  : CapabilitySlot;
      msg   : Message;
      token : Unsigned_64) return Boolean
   is
      ret : Unsigned_64;
   begin
      ret := syscall (SYSCALL_CAP_SUBMIT,
                       slot,
                       tagToU64 (msg.tag),
                       msg.words (0),
                       msg.words (1),
                       msg.words (2),
                       token);
      return (ret = 1);
   end capSubmit;

   --  capNotify
   --  NOTIFY: RDI=cap_slot

   procedure capNotify (slot : CapabilitySlot) is
      ignore : Unsigned_64;
   begin
      ignore := syscall (SYSCALL_NOTIFY, slot);
   end capNotify;

   --  notifyWait
   --  NOTIFY_WAIT: no args
   --  Returns: notifyWord value in RAX

   function notifyWait return Unsigned_64 is
   begin
      return syscall (SYSCALL_NOTIFY_WAIT);
   end notifyWait;

   --  notifyPoll
   --  NOTIFY_POLL: no args
   --  Returns: notifyWord value in RAX (0 if none)

   function notifyPoll return Unsigned_64 is
   begin
      return syscall (SYSCALL_NOTIFY_POLL);
   end notifyPoll;

   --  bindNotification
   --  BIND_NOTIFICATION: RDI=notification capability slot

   procedure bindNotification (slot : CapabilitySlot) is
      ignore : Unsigned_64;
   begin
      ignore := syscall (SYSCALL_BIND_NOTIFICATION, slot);
   end bindNotification;

   --  unbindNotification
   --  UNBIND_NOTIFICATION: no args

   procedure unbindNotification is
      ignore : Unsigned_64;
   begin
      ignore := syscall (SYSCALL_UNBIND_NOTIFICATION);
   end unbindNotification;

   --  sendEvent
   --  SEND_EVENT: RDI=dest, RSI=tag, RDX=w0, R10=w1, R8=w2, R9=w3

   procedure sendEvent (dest : ProcessID; msg : Message) is
      ignore : Unsigned_64;
   begin
      ignore := syscall (SYSCALL_SEND_EVENT,
                          dest,
                          tagToU64 (msg.tag),
                          msg.words (0),
                          msg.words (1),
                          msg.words (2),
                          msg.words (3));
   end sendEvent;

   --  Wait_Event
   --  RECEIVE_EVENT: no args
   --  Returns: event tag in RAX

   function Wait_Event return Message is
      retTag : Unsigned_64;
      msg : Message := NULL_MESSAGE;
   begin
      retTag := syscall (SYSCALL_RECEIVE_EVENT);
      msg.tag := u64ToTag (retTag);
      return msg;
   end Wait_Event;

   function receiveEvent return Message is
   begin
      return Wait_Event;
   end receiveEvent;

   --  Poll_Event
   --  POLL_EVENT: RDI=pointer to Message struct
   --  Returns: RAX=1 if event found, 0 if not

   function Poll_Event (msg : out Message) return Boolean is
      ret : Unsigned_64;
   begin
      msg := NULL_MESSAGE;
      ret := syscall (SYSCALL_POLL_EVENT, toNum (msg'Address));
      return (ret = 1);
   end Poll_Event;

   function receiveEventNB (msg : out Message) return Boolean is
   begin
      return Poll_Event (msg);
   end receiveEventNB;

   --  createGrant
   --  GRANT: RDI=grantee, RSI=localAddr, RDX=numPages, R10=permission
   --  Returns: grant_id (or -1 on error)

   procedure createGrant
     (grantee   : ProcessID;
      localAddr : System.Address;
      numPages  : Natural;
      readWrite : Boolean;
      grantId   : out Unsigned_64;
      success   : out Boolean)
   is
      perm : Unsigned_64 := 0;
      ret  : Unsigned_64;
   begin
      if readWrite then
         perm := 1;
      end if;

      ret := syscall (SYSCALL_GRANT,
                       grantee,
                       toNum (localAddr),
                       Unsigned_64 (numPages),
                       perm);
      if ret = Unsigned_64'Last then
         grantId := 0;
         success := False;
      else
         grantId := ret;
         success := True;
      end if;
   end createGrant;

   --  revokeGrant
   --  REVOKE: RDI=grant_id

   procedure revokeGrant (id : Unsigned_64) is
      ignore : Unsigned_64;
   begin
      ignore := syscall (SYSCALL_REVOKE, id);
   end revokeGrant;

   function killProcess (pid : ProcessID) return Unsigned_64 is
   begin
      return syscall (SYSCALL_KILL, pid);
   end killProcess;

   procedure createGrantViaCap
     (slot      : CapabilitySlot;
      localAddr : System.Address;
      numPages  : Natural;
      readWrite : Boolean;
      grantId   : out Unsigned_64;
      success   : out Boolean)
   is
      function toNum is new Ada.Unchecked_Conversion
         (System.Address, Unsigned_64);
      rwFlag : Unsigned_64 := 0;
      ret    : Unsigned_64;
   begin
      if readWrite then
         rwFlag := 1;
      end if;
      ret := syscall (SYSCALL_GRANT_VIA_CAP,
                       slot,
                       toNum (localAddr),
                       Unsigned_64 (numPages),
                       rwFlag);
      if ret = Unsigned_64'Last then
         grantId := 0;
         success := False;
      else
         grantId := ret;
         success := True;
      end if;
   end createGrantViaCap;

   function setWellKnown
     (role : Unsigned_64;
      pid  : Unsigned_64) return Unsigned_64
   is
   begin
      return syscall (SYSCALL_SET_WELL_KNOWN, role, pid);
   end setWellKnown;

   --  Legacy wrappers

   function sendMsg (to : Unsigned_64; msg : Unsigned_64)
      return Unsigned_64
   is
   begin
      return syscall (SYSCALL_SEND, to, msg);
   end sendMsg;

   function recvMsg (from : out Unsigned_64) return Unsigned_64 is
      retfrom : Unsigned_64;
      pragma Unreferenced (from);
   begin
      return syscall (SYSCALL_RECEIVE, toNum (retfrom'Address));
   end recvMsg;

   function getInfo
     (query : Unsigned_64; detail : Unsigned_64 := 0) return Unsigned_64
   is
   begin
      return syscall (SYSCALL_INFO, query, detail);
   end getInfo;

   function registerDriver (driver : Unsigned_64) return Unsigned_64 is
   begin
      return syscall (SYSCALL_REGISTER_DRIVER, driver);
   end registerDriver;

   procedure debugPrint (str : String) is
      ignore : Unsigned_64;
   begin
      ignore :=
         syscall (SYSCALL_WRITE, STDOUT, toNum (str'Address), str'Length);
   end debugPrint;

   function getSecondaryStack return System.Secondary_Stack.SS_Stack_Ptr
   is
      function toPtr is
         new Ada.Unchecked_Conversion
            (Source => Unsigned_64,
             Target => System.Secondary_Stack.SS_Stack_Ptr);
   begin
      return toPtr (getInfo (SYSINFO_SECONDARY_STACK));
   end getSecondaryStack;

   --  saveReplyCap
   --  SAVE_REPLY_CAP: RDI=destSlot
   --  Returns: RAX=1 on success, 0 on failure

   function saveReplyCap (destSlot : Unsigned_64) return Unsigned_64 is
   begin
      return syscall (SYSCALL_SAVE_REPLY_CAP, destSlot);
   end saveReplyCap;

   --  Port I/O wrappers

   function portInp8 (port : Unsigned_16) return Unsigned_64 is
   begin
      return syscall (SYSCALL_INP8, Unsigned_64 (port));
   end portInp8;

   function portOutp8
     (port : Unsigned_16; val : Unsigned_8) return Unsigned_64
   is
   begin
      return syscall (SYSCALL_OUTP8, Unsigned_64 (port), Unsigned_64 (val));
   end portOutp8;

   function portInp16 (port : Unsigned_16) return Unsigned_64 is
   begin
      return syscall (SYSCALL_INP16, Unsigned_64 (port));
   end portInp16;

   function portOutp16
     (port : Unsigned_16; val : Unsigned_16) return Unsigned_64
   is
   begin
      return syscall (SYSCALL_OUTP16, Unsigned_64 (port), Unsigned_64 (val));
   end portOutp16;

   function portInps16
     (port  : Unsigned_16;
      addr  : System.Address;
      count : Unsigned_32) return Unsigned_64
   is
   begin
      return syscall (SYSCALL_INPS16,
                      Unsigned_64 (port),
                      toNum (addr),
                      Unsigned_64 (count));
   end portInps16;

   function portInp32 (port : Unsigned_16) return Unsigned_64 is
   begin
      return syscall (SYSCALL_INP32, Unsigned_64 (port));
   end portInp32;

   function portOutp32
     (port : Unsigned_16; val : Unsigned_32) return Unsigned_64
   is
   begin
      return syscall (SYSCALL_OUTP32, Unsigned_64 (port), Unsigned_64 (val));
   end portOutp32;

   function virtToPhys (addr : System.Address) return Unsigned_64 is
   begin
      return syscall (SYSCALL_VIRT_TO_PHYS, toNum (addr));
   end virtToPhys;

   --  Device manager wrappers

   function allocDma
     (targetPID : Unsigned_64;
      order     : Unsigned_64;
      virtBase  : Unsigned_64) return Unsigned_64
   is
   begin
      return syscall (SYSCALL_ALLOC_DMA, targetPID, order, virtBase);
   end allocDma;

   function enableIrq
     (vector    : Unsigned_64;
      ownerPID  : Unsigned_64;
      targetCPU : Unsigned_64) return Unsigned_64
   is
   begin
      return syscall (SYSCALL_ENABLE_IRQ, vector, ownerPID, targetCPU);
   end enableIrq;

   function mapInto
     (targetPID : Unsigned_64;
      physAddr  : Unsigned_64;
      virtAddr  : Unsigned_64;
      numPages  : Unsigned_64;
      flags     : Unsigned_64) return Unsigned_64
   is
   begin
      return syscall (SYSCALL_MAP_INTO,
                      targetPID, physAddr, virtAddr, numPages, flags);
   end mapInto;

   function setSysinfo
     (queryID : Unsigned_64;
      value   : Unsigned_64) return Unsigned_64
   is
   begin
      return syscall (SYSCALL_SET_SYSINFO, queryID, value);
   end setSysinfo;

   function setCpu
     (targetPID : Unsigned_64;
      cpu       : Unsigned_64) return Unsigned_64
   is
   begin
      return syscall (SYSCALL_SET_CPU, targetPID, cpu);
   end setCpu;

end CuBit.Messages;
