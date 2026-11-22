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
with CuBit.Call_Census;
with CuBit.Published_Clock;
with CuBit.Waits;
with CuBit.Wake_Words;
with CuBit.Readiness;
with CuBit.Channel_Bells;
with Wait_ABI;
with Readiness_Publication;
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
      CuBit.Call_Census.Note (call);
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

   --  The kernel's results (Process.IPC.Activity_*).
   Activity_Deadline : constant Unsigned_64 := Wait_ABI.Ready_Deadline;
   Activity_Work     : constant Unsigned_64 := Wait_ABI.Ready_Work;
   Activity_Event    : constant Unsigned_64 := Wait_ABI.Ready_Event;
   Activity_Word     : constant Unsigned_64 := Wait_ABI.Ready_Word;

   --  A wait took an entry for this process (a notice, a wake of the wake
   --  word): every readiness tracker counts it consumed, and a wake tells
   --  the channels to look (IPC-010 option B).
   procedure Took_By_Wait (Word : Unsigned_64);
   procedure Took_By_Wait (Word : Unsigned_64) is
   begin
      if Word = Activity_Event or else Word = Activity_Word then
         CuBit.Readiness.Note_Taken (CuBit.Readiness.Events);
      end if;
      if Word = Activity_Word then
         CuBit.Channel_Bells.Note_Wake;
      end if;
   end Took_By_Wait;

   --  Stop (see the spec). Set once at startup, before the first receive.
   Disposition : Stop_Disposition := Program_Reads_Stop;
   Quiesce_Step : Quiesce_Procedure := null;
   --  The Stop is being answered: a receive inside Quiesce_Step that sees
   --  another Stop leaves it to the answer already running.
   Answering_Stop : Boolean := False;

   procedure Exit_On_Stop is
   begin
      Quiesce_Step := null;
      Disposition := Exit_At_Once;
   end Exit_On_Stop;

   procedure On_Stop (Quiesce : not null Quiesce_Procedure) is
   begin
      Quiesce_Step := Quiesce;
      Disposition := Quiesce_Then_Exit;
   end On_Stop;

   function Stop_Answer return Stop_Disposition is (Disposition);

   procedure Answer_Stop (Msg : Message; From : Process_ID) is
      Ignore : Unsigned_64;
   begin
      if Disposition = Program_Reads_Stop or else Answering_Stop
        or else not Is_Kernel_Stop (Msg.tag.label, Msg.tag.length, From, Msg.words (0))
      then
         return;
      end if;
      Answering_Stop := True;
      if Disposition = Quiesce_Then_Exit and then Quiesce_Step /= null then
         Quiesce_Step.all;
      end if;
      Ignore := syscall (SYSCALL_EXIT, Stopped_Exit_Code);
   end Answer_Stop;

   function Wait_For_Activity_Until (Deadline : Unsigned_64)
      return Activity_Result
   is
      Result : constant Unsigned_64 :=
        CuBit.Waits.Wait (Wait_ABI.Ready_Word_Sources, Deadline, 0,
                          CuBit.Wake_Words.Address, CuBit.Wake_Words.Value);
   begin
      Took_By_Wait (Result);
      case Result is
         when Activity_Work | Activity_Word => return Work_Available;
         when Activity_Deadline => return Deadline_Reached;
         when others => return Unavailable;
      end case;
   end Wait_For_Activity_Until;

   procedure Wait_For_Activity_Or_Event
     (Deadline : Unsigned_64; Event : aliased out Message; Result : out Wake_Result)
   is
      Word : Unsigned_64;
   begin
      Event := NULL_MESSAGE;
      Word := CuBit.Waits.Wait
        (Wait_ABI.Ready_Word_Sources, Deadline,
         Unsigned_64 (System.Storage_Elements.To_Integer (Event'Address)),
         CuBit.Wake_Words.Address, CuBit.Wake_Words.Value);
      Took_By_Wait (Word);
      Result := (case Word is
                   when Activity_Event    => Event_Taken,
                   when Activity_Work | Activity_Word => Work_Available,
                   when Activity_Deadline => Deadline_Reached,
                   when others            => Unavailable);
      if Result = Event_Taken then
         Answer_Stop (Event, No_Process);   --  only kernel notices are taken
      end if;
   end Wait_For_Activity_Or_Event;

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
      from := From_Word (CuBit.Waits.Wait
        (Wait_ABI.Messages_Sources, Wait_ABI.Forever, toNum (msg'Address)));
      Answer_Stop (msg, from);
   end receive;

   procedure receiveUntil
     (deadlineMs : Unsigned_64;
      from       : out Process_ID;
      msg        : out Message;
      received   : out Boolean)
   is
      result : constant Unsigned_64 :=
        CuBit.Waits.Wait (Wait_ABI.Messages_Sources, deadlineMs, toNum (msg'Address));
   begin
      if result = Wait_ABI.Nothing then
         from := No_Process;
         received := False;
      else
         from := From_Word (result);
         received := True;
         Answer_Stop (msg, from);
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
      Answer_Stop (msg, from);
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
      ret := CuBit.Waits.Wait (Wait_ABI.Requests_Sources, Wait_ABI.Now, toNum (msg'Address));
      --  A request always has a sender (identity word nonzero).
      found := ret /= Wait_ABI.Nothing and then ret /= 0;
      from := (if found then From_Word (ret) else No_Process);
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
      ret := CuBit.Waits.Wait (Wait_ABI.Messages_Sources, Wait_ABI.Now, toNum (msg'Address));
      --  found: a message with a sender. A kernel notice (sender word 0)
      --  is reported as before: not found, the message filled in.
      found := ret /= Wait_ABI.Nothing and then ret /= 0;
      from := (if found then From_Word (ret) else No_Process);
      if found then
         Answer_Stop (msg, from);
      elsif ret = 0 then
         Answer_Stop (msg, No_Process);
      end if;
   end Poll_Any_Ipc;

   --  waitCompletion
   --  WAIT_COMPLETION: RDI=pointer to buffer, RSI=maxEntries, RDX=minWait
   --  Returns: RAX=numReturned

   function waitCompletion
     (entries : System.Address;
      max     : Unsigned_64;
      min     : Unsigned_64) return Unsigned_64
   is
      Result : constant Unsigned_64 := CuBit.Waits.Wait
        (Wait_ABI.Completions_Sources, Wait_ABI.Forever, toNum (entries), max, min);
   begin
      --  A malformed request (max 0, an unwritable range) is Last, as before.
      return (if Result = Wait_ABI.Unsupported_Form then Unsigned_64'Last else Result);
   end waitCompletion;

   --  Poll_Completion
   --  POLL_COMPLETION: RDI=pointer to CompletionEntry
   --  Returns: RAX=1 if found, 0 if empty

   function Poll_Completion
     (result : System.Address) return Unsigned_64
   is
      Taken : constant Unsigned_64 := CuBit.Waits.Wait
        (Wait_ABI.Completions_Sources, Wait_ABI.Now, toNum (result), 1, 0);
   begin
      return (if Taken = 1 then 1 else 0);
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
      ret := CuBit.Waits.Wait (Wait_ABI.Events_Sources, Wait_ABI.Now, toNum (msg'Address));
      if ret = Wait_ABI.Taken then
         --  An event lane entry: the kernel's, or one a process published
         --  (never with the control label, which the kernel refuses).
         Answer_Stop (msg, No_Process);
      end if;
      return ret = Wait_ABI.Taken;
   end Poll_Event;

   function Poll_Event (msg : out Message; publisher : out Process_ID) return Boolean is
      ret : Unsigned_64;
      identity : aliased Unsigned_64 := 0;
   begin
      msg := NULL_MESSAGE;
      ret := CuBit.Waits.Wait
        (Wait_ABI.Events_Sources, Wait_ABI.Now, toNum (msg'Address), toNum (identity'Address));
      publisher := (if ret = Wait_ABI.Taken then From_Word (identity) else No_Process);
      if ret = Wait_ABI.Taken then
         Answer_Stop (msg, publisher);
      end if;
      return ret = Wait_ABI.Taken;
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

   Own_Identity : Unsigned_64
     with Import, Volatile, Atomic,
          Address => System.Storage_Elements.To_Address
            (Readiness_Publication.Identity_Address);

   function Own_Process_Word return Unsigned_64 is (Own_Identity);

   function Own_Process return Process_ID is (From_Word (Own_Identity));

   function killProcess
     (pid : Process_ID; authority : CuBit.Handles.Process) return Unsigned_64 is
     (if CuBit.Handles.Control.Kill (pid, authority) then 0 else Unsigned_64'Last);

   function registerDriver
     (driver : Unsigned_64; authority : CuBit.Handles.Notification) return Unsigned_64 is
     (CuBit.Handles.Devices.Register (driver, authority));

   Sink : Console_Sink := null;

   procedure Set_Console_Sink (Sink : not null Console_Sink) is
   begin
      Messages.Sink := Sink;
   end Set_Console_Sink;

   function Console_Routed return Boolean is (Sink /= null);

   procedure debugPrint (str : String) is
      ignore : Unsigned_64;
   begin
      if Sink /= null then
         Sink (str);
      else
         ignore :=
            syscall (SYSCALL_WRITE, STDOUT, toNum (str'Address), str'Length);
      end if;
   end debugPrint;

   procedure Console_Put (Text : System.Address; Length : Unsigned_64) is
      Bounded : constant Natural :=
        (if Length > Unsigned_64 (Natural'Last) then Natural'Last else Natural (Length));
      Item : constant String (1 .. Bounded) with Import, Address => Text;
   begin
      if Bounded > 0 then
         debugPrint (Item);
      end if;
   end Console_Put;

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
