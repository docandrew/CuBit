------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2021 Jon Andrew
--
--  @summary
--  IPC Messages / Syscalls
--
--  Full multi-word IPC wrappers matching kernel Process.IPC.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Unchecked_Conversion;
with System;
with System.Storage_Elements;
with System.Machine_Code; use System.Machine_Code;

package body CuBit.Messages is

   --  syscall
   --  Ada interface to syscall instruction (x86_64)
   --
   --  Note: The syscall instruction clobbers RCX (return address) and
   --  R11 (RFLAGS). The kernel entry moves R10 -> RCX for arg3, so arg3
   --  must go into R10 from userspace.
   --  CuBit's kernel preserves all other general-purpose registers, including
   --  RDI/RSI/RDX (input-only operands below). R10/R8/R9 are clobbers here
   --  because this wrapper itself loads them before entering the kernel.

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

   function Wait_For_Activity_Until (Deadline : Unsigned_64)
      return Activity_Result
   is
      Result : constant Unsigned_64 := syscall
        (SYSCALL_WAIT_FOR_IPC_OR_COMPLETION_UNTIL_MONOTONIC_MILLISECOND,
         Deadline);
   begin
      case Result is
         when 1 => return Work_Available;
         when 0 => return Deadline_Reached;
         when others => return Unavailable;
      end case;
   end Wait_For_Activity_Until;

   --  Conversion helpers

   function tagToU64 is new Ada.Unchecked_Conversion
      (MessageTag, Unsigned_64);
   function u64ToTag is new Ada.Unchecked_Conversion
      (Unsigned_64, MessageTag);
   function toNum is new Ada.Unchecked_Conversion
      (System.Address, Unsigned_64);

   --  receive
   --  RECEIVE: RDI=pointer to Message struct
   --  Returns: RAX=sender PID

   procedure receive (from : out Process_ID; msg : out Message) is
   begin
      from := From_Word (syscall (SYSCALL_RECEIVE, toNum (msg'Address)));
   end receive;

   procedure receiveUntil
     (deadlineMs : Unsigned_64;
      from       : out Process_ID;
      msg        : out Message;
      received   : out Boolean)
   is
      result : constant Unsigned_64 :=
        syscall
          (SYSCALL_RECEIVE_UNTIL_MONOTONIC_MILLISECOND,
           toNum (msg'Address), deadlineMs);
   begin
      if result = Unsigned_64'Last then
         from := No_Process;
         received := False;
      else
         from := From_Word (result);
         received := True;
      end if;
   end receiveUntil;

   --  reply
   --  REPLY: RDI=dest, RSI=tag, RDX=w0, R10=w1, R8=w2, R9=w3

   function reply
     (replyTo : Process_ID; msg : Message) return Unsigned_64
   is
   begin
      return syscall (SYSCALL_REPLY,
                       To_Word (replyTo),
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
      return syscall (SYSCALL_REPLY_AND_CONSUME_REPLY_CAPABILITY,
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
     (replyTo  : Process_ID;
      replyMsg : Message;
      from     : out Process_ID;
      msg      : in out Message)
   is
   begin
      msg := replyMsg;
      from := From_Word (syscall (SYSCALL_REPLY_WAIT, To_Word (replyTo), toNum (msg'Address)));
   end replyWait;

   --  Poll_Service_Request
   --  POLL_SERVICE_REQUEST: RDI=pointer to Message struct
   --  Returns: RAX=sender PID (0 if no service request)
   --
   --  This wrapper is intentionally semantic rather than queue-shaped. The
   --  kernel still stores several IPC classes in one mailbox ring, but this
   --  syscall asks for only request-like work destined for a service.
   procedure Poll_Service_Request
     (from  : out Process_ID;
      msg   : out Message;
      found : out Boolean)
   is
      ret : Unsigned_64;
   begin
      msg := NULL_MESSAGE;
      ret := syscall (SYSCALL_POLL_SERVICE_REQUEST, toNum (msg'Address));
      from := From_Word (ret);
      found := (ret /= 0);
   end Poll_Service_Request;

   --  Poll_Any_Ipc
   --  POLL_ANY_IPC: RDI=pointer to Message struct
   --  Returns: RAX=sender PID, or 0 if no mixed IPC was available.
   --
   --  This preserves the old mixed-receive behavior. It can consume events,
   --  so call sites should look unusual on purpose.
   procedure Poll_Any_Ipc
     (from  : out Process_ID;
      msg   : out Message;
      found : out Boolean)
   is
      ret : Unsigned_64;
   begin
      msg := NULL_MESSAGE;
      ret := syscall (SYSCALL_POLL_ANY_IPC, toNum (msg'Address));
      from := From_Word (ret);
      found := (ret /= 0);
   end Poll_Any_Ipc;

   procedure Find_Endpoint_Capability
     (Target : Process_ID; Slot : out CapabilitySlot; Found : out Boolean)
   is
      --  INSPECT_CAPABILITY's fixed six-word result; only inspect our table.
      Info : array (0 .. 5) of Unsigned_64 := [others => 0];
      Self : constant Unsigned_64 := syscall (SYSCALL_GETPID);
      Result : Unsigned_64;
      Endpoint_Kind : constant Unsigned_64 := 1;
      Read_Write_Rights : constant Unsigned_64 := 3;
   begin
      Slot := CapabilitySlot'First;
      Found := False;
      if Target = No_Process then
         return;
      end if;
      for Candidate in CapabilitySlot loop
         Result := syscall
           (SYSCALL_INSPECT_CAPABILITY, Self, Candidate,
            Unsigned_64 (System.Storage_Elements.To_Integer (Info'Address)));
         if Result = 1 and then Info (0) = Endpoint_Kind and then
           (Info (1) and Read_Write_Rights) = Read_Write_Rights and then
           Info (3) = To_Word (Target)
         then
            Slot := Candidate;
            Found := True;
            return;
         end if;
      end loop;
   end Find_Endpoint_Capability;

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

   --  capSend
   --  CAP_SEND: RDI=cap_slot, RSI=tag, RDX=w0, R10=w1, R8=w2, R9=w3
   --  Returns: reply tag in RAX

   function Deadline_After (Milliseconds : Unsigned_64) return Unsigned_64 is
      Now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
   begin
      return (if Milliseconds >= Wait_Forever - Now then Wait_Forever else Now + Milliseconds);
   end Deadline_After;

   function capSend
     (slot : CapabilitySlot; msg : Message; Deadline : Unsigned_64) return MessageTag
   is
      retTag : Unsigned_64;
   begin
      --  Seven arguments: the deadline goes in R12, as capSubmit's token.
      Asm
        ("mov %5, %%r10" & ASCII.LF &
         "mov %6, %%r8" & ASCII.LF &
         "mov %7, %%r9" & ASCII.LF &
         "mov %8, %%r12" & ASCII.LF & "syscall",
         Outputs => Unsigned_64'Asm_Output ("=a", retTag),
         Inputs =>
           (Unsigned_64'Asm_Input
              ("a", SYSCALL_SEND_VIA_ENDPOINT_CAPABILITY),
            Unsigned_64'Asm_Input ("D", slot),
            Unsigned_64'Asm_Input ("S", tagToU64 (msg.tag)),
            Unsigned_64'Asm_Input ("d", msg.words (0)),
            Unsigned_64'Asm_Input ("rm", msg.words (1)),
            Unsigned_64'Asm_Input ("rm", msg.words (2)),
            Unsigned_64'Asm_Input ("rm", msg.words (3)),
            Unsigned_64'Asm_Input ("rm", Deadline)),
         Clobber => "r10, r8, r9, r12, rcx, r11, memory",
         Volatile => True);
      return u64ToTag (retTag);
   end capSend;

   --  capCall
   --  CAP_CALL: RDI=cap_slot, RSI=pointer to Message struct (in/out)
   --  Returns: reply tag in RAX

   function capCall
     (slot : CapabilitySlot; msg : in out Message; Deadline : Unsigned_64) return MessageTag
   is
      retTag : Unsigned_64;
   begin
      retTag := syscall
        (SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY, slot, toNum (msg'Address), Deadline);
      return u64ToTag (retTag);
   end capCall;

   --  capSubmit
   --  CAP_SUBMIT: RDI=slot, RSI=tag, RDX=w0, R10=w1, R8=w2, R9=w3,
   --  R12=completion token. All four payload words reach the receiver.

   function capSubmit
     (slot  : CapabilitySlot;
      msg   : Message;
      token : Unsigned_64) return Boolean
   is
      ret : Unsigned_64;
   begin
      Asm
        ("mov %5, %%r10" & ASCII.LF &
         "mov %6, %%r8" & ASCII.LF &
         "mov %7, %%r9" & ASCII.LF &
         "mov %8, %%r12" & ASCII.LF & "syscall",
         Outputs => Unsigned_64'Asm_Output ("=a", ret),
         Inputs =>
           (Unsigned_64'Asm_Input
              ("a", SYSCALL_SUBMIT_VIA_ENDPOINT_CAPABILITY),
            Unsigned_64'Asm_Input ("D", slot),
            Unsigned_64'Asm_Input ("S", tagToU64 (msg.tag)),
            Unsigned_64'Asm_Input ("d", msg.words (0)),
            Unsigned_64'Asm_Input ("rm", msg.words (1)),
            Unsigned_64'Asm_Input ("rm", msg.words (2)),
            Unsigned_64'Asm_Input ("rm", msg.words (3)),
            Unsigned_64'Asm_Input ("rm", token)),
         Clobber => "r10, r8, r9, r12, rcx, r11, memory",
         Volatile => True);
      return (ret = 1);
   end capSubmit;

   --  sendEvent
   --  SEND_EVENT: RDI=dest, RSI=tag, RDX=w0, R10=w1, R8=w2, R9=w3

   procedure sendEvent (dest : Process_ID; msg : Message) is
      ignore : Unsigned_64;
   begin
      ignore := syscall (SYSCALL_SEND_EVENT,
                          To_Word (dest),
                          tagToU64 (msg.tag),
                          msg.words (0),
                          msg.words (1),
                          msg.words (2),
                          msg.words (3));
   end sendEvent;

   function trySendEvent (dest : Process_ID; msg : Message) return Boolean is
   begin
      return syscall (SYSCALL_SEND_EVENT,
                      To_Word (dest),
                      tagToU64 (msg.tag),
                      msg.words (0),
                      msg.words (1),
                      msg.words (2),
                      msg.words (3)) = 1;
   end trySendEvent;

   --  Wait_Event
   --  RECEIVE_EVENT: no args; returns the event tag in RAX.

   function Wait_Event return Message is
      retTag : Unsigned_64;
      msg : Message := NULL_MESSAGE;
   begin
      retTag := syscall (SYSCALL_RECEIVE_EVENT);
      msg.tag := u64ToTag (retTag);
      return msg;
   end Wait_Event;

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

   --  revokeGrant
   --  REVOKE: RDI=grant_id

   procedure revokeGrant (id : Unsigned_64) is
      ignore : Unsigned_64;
   begin
      ignore := syscall (SYSCALL_REVOKE_SHARED_MEMORY_GRANT, id);
   end revokeGrant;

   function Registered_Driver (Driver : Unsigned_64) return Process_ID is
      Word : constant Unsigned_64 := getInfo (SYSINFO_REGISTERED_DRIVER, Driver);
   begin
      return (if Word = Unsigned_64'Last then No_Process else From_Word (Word));
   end Registered_Driver;

   function Own_Process return Process_ID is
     (From_Word (syscall (SYSCALL_GETPID)));

   function killProcess (pid : Process_ID) return Unsigned_64 is
   begin
      return syscall (SYSCALL_KILL, To_Word (pid));
   end killProcess;

   function setWellKnown
     (role : Unsigned_64;
      pid  : Process_ID) return Unsigned_64
   is
   begin
      return syscall (SYSCALL_SET_WELL_KNOWN, role, To_Word (pid));
   end setWellKnown;

   --  Legacy wrappers

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
      return syscall (SYSCALL_MOVE_REPLY_CAPABILITY, destSlot);
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
     (targetPID : Process_ID;
      order     : Unsigned_64;
      virtBase  : Unsigned_64) return Unsigned_64
   is
   begin
      return syscall (SYSCALL_ALLOC_DMA, To_Word (targetPID), order, virtBase);
   end allocDma;

   function enableIrq
     (vector    : Unsigned_64;
      ownerPID  : Process_ID;
      targetCPU : Unsigned_64;
      levelTriggered : Boolean := False;
      activeLow      : Boolean := False;
      messageSignaled : Boolean := False) return Unsigned_64
   is
      route : Unsigned_64 := targetCPU and 16#FF#;
   begin
      if levelTriggered then
         route := route or 16#100#;
      end if;
      if activeLow then
         route := route or 16#200#;
      end if;
      if messageSignaled then
         route := route or 16#400#;
      end if;
      return syscall (SYSCALL_ENABLE_IRQ, vector, To_Word (ownerPID), route);
   end enableIrq;

   function mapInto
     (targetPID : Process_ID;
      physAddr  : Unsigned_64;
      virtAddr  : Unsigned_64;
      numPages  : Unsigned_64;
      flags     : Unsigned_64) return Unsigned_64
   is
   begin
      return syscall (SYSCALL_MAP_INTO,
                      To_Word (targetPID), physAddr, virtAddr, numPages, flags);
   end mapInto;

   function setSysinfo
     (queryID : Unsigned_64;
      value   : Unsigned_64) return Unsigned_64
   is
   begin
      return syscall (SYSCALL_SET_SYSINFO, queryID, value);
   end setSysinfo;

   function setCpu
     (targetPID : Process_ID;
      cpu       : Unsigned_64) return Unsigned_64
   is
   begin
      return syscall (SYSCALL_SET_CPU, To_Word (targetPID), cpu);
   end setCpu;

   function setLatencyContract
     (latencyClass : Unsigned_64;
      periodUs     : Unsigned_64;
      budgetUs     : Unsigned_64;
      flags        : Unsigned_64 := 0) return Unsigned_64
   is
   begin
      return syscall (SYSCALL_SET_LATENCY_CONTRACT,
                      latencyClass, periodUs, budgetUs, flags);
   end setLatencyContract;

end CuBit.Messages;
