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
with System.Machine_Code; use System.Machine_Code;
with CuBit.Published_Clock;
with CuBit.System_Facts;
with CuBit.Handles.Control;
with CuBit.Handles.Devices;
with CuBit.Handles.IPC;

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
     (saved : CuBit.Handles.Reply; msg : Message) return Unsigned_64 is
     (if CuBit.Handles.IPC.Answer (saved, msg) then 1 else 0);

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
      Now : constant Unsigned_64 := CuBit.Published_Clock.Milliseconds;
   begin
      return (if Milliseconds >= Wait_Forever - Now then Wait_Forever else Now + Milliseconds);
   end Deadline_After;

   function capSend
     (target : CuBit.Handles.Endpoint; msg : Message; Deadline : Unsigned_64)
      return MessageTag is (CuBit.Handles.IPC.Send (target, msg, Deadline));

   --  capCall
   --  CAP_CALL: RDI=cap_slot, RSI=pointer to Message struct (in/out)
   --  Returns: reply tag in RAX

   function capCall
     (target : CuBit.Handles.Endpoint; msg : in out Message; Deadline : Unsigned_64)
      return MessageTag is
   begin
      return CuBit.Handles.IPC.Call (target, msg, Deadline);
   end capCall;

   --  capSubmit
   --  CAP_SUBMIT: RDI=slot, RSI=tag, RDX=w0, R10=w1, R8=w2, R9=w3,
   --  R12=completion token. All four payload words reach the receiver.

   function capSubmit
     (target : CuBit.Handles.Endpoint;
      msg    : Message;
      token  : Unsigned_64) return Boolean is
     (CuBit.Handles.IPC.Submit (target, msg, token));

   --  sendEvent
   --  SEND_EVENT: RDI=dest, RSI=tag, RDX=w0, R10=w1, R8=w2, R9=w3

   procedure sendEvent
     (dest : Process_ID; msg : Message; authority : CuBit.Handles.Notification)
   is
      ignore : constant Boolean := CuBit.Handles.Devices.Send_Event (dest, msg, authority);
   begin
      null;
   end sendEvent;

   procedure sendEvent
     (dest : Process_ID; msg : Message; authority : CuBit.Handles.Endpoint)
   is
      ignore : constant Boolean := CuBit.Handles.Devices.Send_Event (dest, msg, authority);
   begin
      null;
   end sendEvent;

   function trySendEvent
     (dest : Process_ID; msg : Message; authority : CuBit.Handles.Notification)
      return Boolean is (CuBit.Handles.Devices.Send_Event (dest, msg, authority));

   function trySendEvent
     (dest : Process_ID; msg : Message; authority : CuBit.Handles.Endpoint)
      return Boolean is (CuBit.Handles.Devices.Send_Event (dest, msg, authority));

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

   function Poll_Event (msg : out Message; publisher : out Process_ID) return Boolean is
      ret : Unsigned_64;
      identity : aliased Unsigned_64 := 0;
   begin
      msg := NULL_MESSAGE;
      ret := syscall (SYSCALL_POLL_EVENT, toNum (msg'Address), toNum (identity'Address));
      publisher := (if ret = 1 then From_Word (identity) else No_Process);
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
     (From_Word (CuBit.System_Facts.Registered (Driver)));

   function Own_Process return Process_ID is
     (From_Word (syscall (SYSCALL_GETPID)));

   function killProcess
     (pid : Process_ID; authority : CuBit.Handles.Process) return Unsigned_64 is
     (if CuBit.Handles.Control.Kill (pid, authority) then 0 else Unsigned_64'Last);

   function registerDriver
     (driver : Unsigned_64; authority : CuBit.Handles.Notification) return Unsigned_64 is
     (CuBit.Handles.Devices.Register (driver, authority));

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
      return toPtr (CuBit.System_Facts.Secondary_Stack_Address);
   end getSecondaryStack;

   --  saveReplyCap
   --  SAVE_REPLY_CAP: RDI=destSlot
   --  Returns: RAX=1 on success, 0 on failure

   --  Port I/O wrappers

   function portInp8 (ports : CuBit.Handles.IO_Ports; port : Unsigned_16) return Unsigned_64 is
     (CuBit.Handles.Devices.In_8 (ports, port));

   function portOutp8
     (ports : CuBit.Handles.IO_Ports; port : Unsigned_16; val : Unsigned_8) return Unsigned_64 is
     (if CuBit.Handles.Devices.Out_8 (ports, port, val) then 0 else Unsigned_64'Last);

   function portInp16 (ports : CuBit.Handles.IO_Ports; port : Unsigned_16) return Unsigned_64 is
     (CuBit.Handles.Devices.In_16 (ports, port));

   function portOutp16
     (ports : CuBit.Handles.IO_Ports; port : Unsigned_16; val : Unsigned_16) return Unsigned_64 is
     (if CuBit.Handles.Devices.Out_16 (ports, port, val) then 0 else Unsigned_64'Last);

   function portInp32 (ports : CuBit.Handles.IO_Ports; port : Unsigned_16) return Unsigned_64 is
     (CuBit.Handles.Devices.In_32 (ports, port));

   function portOutp32
     (ports : CuBit.Handles.IO_Ports; port : Unsigned_16; val : Unsigned_32) return Unsigned_64 is
     (if CuBit.Handles.Devices.Out_32 (ports, port, val) then 0 else Unsigned_64'Last);

   --  Device manager wrappers

   function allocDma
     (targetPID : Process_ID;
      order     : Unsigned_64;
      virtBase  : Unsigned_64;
      authority : CuBit.Handles.Process) return Unsigned_64 is
     (CuBit.Handles.Control.Allocate_DMA (targetPID, authority, order, virtBase, 0, 0));

   function enableIrq
     (vector    : Unsigned_64;
      ownerPID  : Process_ID;
      targetCPU : Unsigned_64;
      authority : CuBit.Handles.Process;
      lines     : CuBit.Handles.Interrupt;
      levelTriggered : Boolean := False;
      activeLow      : Boolean := False) return Unsigned_64
   is
      --  The route word (Syscall.Admin.handleEnableIrq).
      ROUTE_CPU_MASK   : constant Unsigned_64 := 16#00FF#;
      ROUTE_LEVEL      : constant Unsigned_64 := 16#0100#;
      ROUTE_ACTIVE_LOW : constant Unsigned_64 := 16#0200#;
      route : Unsigned_64 := targetCPU and ROUTE_CPU_MASK;
   begin
      if levelTriggered then
         route := route or ROUTE_LEVEL;
      end if;
      if activeLow then
         route := route or ROUTE_ACTIVE_LOW;
      end if;
      return (if CuBit.Handles.Control.Enable_IRQ
                    (vector, ownerPID, route, authority, lines)
              then 0 else Unsigned_64'Last);
   end enableIrq;

   function mapInto
     (targetPID : Process_ID;
      physAddr  : Unsigned_64;
      virtAddr  : Unsigned_64;
      numPages  : Unsigned_64;
      flags     : Unsigned_64;
      authority : CuBit.Handles.Process;
      region    : CuBit.Handles.Device_Memory) return Unsigned_64 is
     (if CuBit.Handles.Control.Map_Into
           (targetPID, authority, region, physAddr, virtAddr, numPages, flags)
      then 0 else Unsigned_64'Last);

   function setCpu
     (targetPID : Process_ID;
      cpu       : Unsigned_64;
      authority : CuBit.Handles.Process) return Unsigned_64 is
     (if CuBit.Handles.Control.Set_CPU (targetPID, authority, cpu)
      then 0 else Unsigned_64'Last);

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
