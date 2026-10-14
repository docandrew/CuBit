------------------------------------------------------------------------------
--  CuBit capability non-amplification regression app
--
--  This deliberately authorityless process attempts the operations that once
--  made the self CAP_PROCESS an ambient privilege-escalation path, and checks
--  the handle table's semantics (CAP-002, docs/capability-handles.md): a
--  forged word names nothing, a closed handle is refused, a stale handle
--  never reaches what was placed in its slot later, the handle limit is
--  enforced, and a capability is taken only where one was offered.
--
--  Raw system calls are deliberate: this test speaks to the kernel past the
--  runtime's typed glue, as a hostile program would. Its only handles are
--  the launcher's "self" and "self-process" entries.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with CuBit.System_Facts;
with CuBit.Handles; use CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Handles.Control;
with CuBit.Handles.Devices;
with CuBit.Handles.Startup;
with CuBit.Kernel_ABI;
with CuBit.Startup_State;
with CuBit.Startup_Tables;
with Startup;

procedure Main is
   use ASCII;

   package IPC renames CuBit.Handles.IPC;
   package ST renames CuBit.Startup_Tables;
   use type ABI.Refusal;
   use type ABI.Kind;
   use type ABI.Inspection;
   use type ST.Handle_Limit;
   use type IPC.Carried_Kind;
   use type IPC.Reply_Outcome;

   --  Raw ABI numbers this test calls past the glue (Capability_ABI and
   --  the kernel's syscall table; CuBit.Handles.Kernel is the runtime's).
   Raw_Send_Event : constant Unsigned_64 := 19;
   Raw_Spawn      : constant Unsigned_64 := 60;
   Raw_Inspect    : constant Unsigned_64 := 84;
   --  Retired: POLICY_MINT (72), MINT_FOR_INCARNATION (120),
   --  DELEGATE_ENDPOINT (121), RESTORE_REPLY (136).
   Retired_Policy_Mint       : constant Unsigned_64 := 72;
   Retired_Mint_Incarnation  : constant Unsigned_64 := 120;
   Retired_Delegate_Endpoint : constant Unsigned_64 := 121;
   Retired_Restore_Reply     : constant Unsigned_64 := 136;
   --  Every number a slot was once written as (the old 64-entry table).
   Old_Slot_Count : constant := 64;
   --  The manifest's handle limit (Handle_Limit.At_Most 8).
   Handle_Limit   : constant := 8;
   --  The launcher's two self entries count against it.
   Self_Entries   : constant := 2;
   ERROR_RESULT : constant Unsigned_64 := Unsigned_64'Last;
   --  INSPECT's answer for a live entry.
   Inspected    : constant Unsigned_64 := 0;
   --  One generation step in a handle word (generation << 32 | slot).
   Generation_Step : constant Unsigned_64 := 2 ** ABI.Slot_Field_Bits;
   Slot_Mask       : constant Unsigned_64 := Generation_Step - 1;
   --  Tokens of this test's requests to itself.
   Self_Token_First : constant Unsigned_64 := 16#CA9_0000#;
   Probe_Label      : constant Unsigned_32 := 16#7E59#;

   Inspection : aliased ABI.Inspection := [others => 0];

   function Address_Number is new Ada.Unchecked_Conversion
     (System.Address, Unsigned_64);
   function Number_Address is new Ada.Unchecked_Conversion
     (Unsigned_64, System.Address);

   PID    : Unsigned_64;
   Process_Manager : Process_ID;
   Result : Unsigned_64;
   Passed : Boolean := True;

   function Syscall_Registers_Preserved
     (Number, Argument : Unsigned_64) return Unsigned_64
     with Import, Convention => C,
          External_Name => "syscall_registers_preserved";

   --  The launcher's self entries, and their raw words for forging.
   Self         : Endpoint;
   Self_Process : CuBit.Handles.Process;
   Self_Word    : Unsigned_64;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         debugPrint ("capability-test: " & Name & " PASS" & LF);
      else
         debugPrint ("capability-test: " & Name & " FAIL" & LF);
         Passed := False;
      end if;
   end Check;

   --  INSPECT through the raw ABI: 0, or a refusal code. The kernel writes
   --  the record only for a live entry, whose generation is never 0; the
   --  buffer is cleared first, so a refusal must also leave it untouched.
   function Raw_Inspect_Word (Word : Unsigned_64) return Unsigned_64 is
   begin
      Inspection := [others => 0];
      return syscall (Raw_Inspect, Word, Address_Number (Inspection'Address));
   end Raw_Inspect_Word;

   --  Whether Word names a live entry of this process's table.
   function Names_Entry (Word : Unsigned_64) return Boolean is
     (Raw_Inspect_Word (Word) = Inspected and then Inspection (ABI.Generation) /= 0);

   --  Whether the kernel refused Word for Why, writing nothing.
   function Refused_As (Word : Unsigned_64; Why : ABI.Refusal) return Boolean is
     (Raw_Inspect_Word (Word) = ABI.Refusal_Word (Why)
      and then Inspection = ABI.Inspection'(others => 0));

   procedure Check_Heap_Admission is
      Before : constant Unsigned_64 := syscall (SYSCALL_SBRK, 0);
      Allocation : Unsigned_64;
      Zeroed : Boolean := True;
      type Page_Bytes is array (Natural range 0 .. 8191) of Unsigned_8;
   begin
      Check (Before /= ERROR_RESULT, "heap query");
      Allocation := syscall (SYSCALL_SBRK, Unsigned_64'Last);
      Check (Allocation = ERROR_RESULT and then syscall (SYSCALL_SBRK, 0) = Before,
             "heap wrapping request rejected without growth");
      -- Address-space overflow must reject before allocation. A normal large
      -- request is no longer rejected merely by an old fixed tracking budget.
      for Attempt in 1 .. 2 loop
         Allocation := syscall (SYSCALL_SBRK, 16#0000_8000_0000_0000#);
         Check (Allocation = ERROR_RESULT and then syscall (SYSCALL_SBRK, 0) = Before,
                "heap oversized request rejected without growth");
      end loop;
      Allocation := syscall (SYSCALL_SBRK, 8192);
      Check (Allocation = Before and then syscall (SYSCALL_SBRK, 0) = Before + 8192,
             "heap usable after rejected requests");
      if Allocation /= ERROR_RESULT then
         declare
            Bytes : Page_Bytes with Import, Address => Number_Address (Allocation);
         begin
            for I in Bytes'Range loop
               Zeroed := Zeroed and then Bytes (I) = 0;
               Bytes (I) := Unsigned_8 (I mod 251);
            end loop;
            Check (Zeroed and then Bytes (8191) = Unsigned_8 (8191 mod 251),
                   "heap admitted pages zeroed and writable");
         end;
      end if;
   end Check_Heap_Admission;

   procedure Check_Stack_Growth is
      type Stack_Bytes is array (Natural range 0 .. 65_535) of Unsigned_8;
      Bytes : Stack_Bytes with Volatile;
      Correct : Boolean := True;
   begin
      -- Touch each page, then revisit it. Volatile keeps the compiler from
      -- replacing this with a scalar and bypassing real user page faults.
      for Page in 0 .. 15 loop
         Bytes (Page * 4096) := Unsigned_8 (Page + 1);
      end loop;
      for Page in 0 .. 15 loop
         Correct := Correct and then Bytes (Page * 4096) = Unsigned_8 (Page + 1);
      end loop;
      Check (Correct, "stack demand pages writable and retained");
   end Check_Stack_Growth;
begin
   Check_Stack_Growth;
   Check_Heap_Admission;
   Check (Syscall_Registers_Preserved (SYSCALL_GETPID, 0) = 1,
          "syscall registers normal return");
   Check (Syscall_Registers_Preserved (Unsigned_64'Last, 0) = 1,
          "syscall registers unknown call");
   Check (Syscall_Registers_Preserved (Raw_Spawn, 0) = 1,
          "syscall registers denied call");
   Check (Syscall_Registers_Preserved (SYSCALL_SLEEP, 10) = 1,
          "syscall registers blocking return");

   PID := syscall (SYSCALL_GETPID);
   Check (PID > 0 and then PID /= ERROR_RESULT, "getpid");

   --  The startup table holds exactly what the launcher gives every
   --  process (its manifest requests nothing).
   Check (CuBit.Startup_State.Ready (Startup.Verify), "startup table loaded");
   Check (CuBit.Startup_State.Current.Count = Self_Entries
          and then CuBit.Startup_State.Current.Limit =
                   (Kind => ST.At_Most, Count => Handle_Limit),
          "startup table holds only the self entries, with the declared limit");
   Self := CuBit.Handles.Startup.Endpoint_Named (ST.Self_Text);
   Self_Process := CuBit.Handles.Startup.Process_Named (ST.Self_Process_Text);
   Self_Word := CuBit.Startup_State.Granted (ST.Self_Text, ST.Endpoint);
   Check (Valid (Self) and then Valid (Self_Process)
          and then Names_Entry (Self_Word),
          "self entries are current handles");

   --  No ambient authority by number: every word a slot was once written
   --  as (generation 0) names nothing, kernel tables included.
   declare
      All_Refused : Boolean := True;
   begin
      for Old_Slot in Unsigned_64 range 0 .. Old_Slot_Count - 1 loop
         All_Refused := All_Refused
           and then Refused_As (Old_Slot, ABI.Invalid_Handle);
      end loop;
      Check (All_Refused, "no ambient capability at any old slot number");
   end;

   --  Forged words in the raw glue: the self entry's slot with a generation
   --  never issued, a slot past the table, the all-ones sentinel.
   Check (Refused_As (Self_Word + Generation_Step, ABI.Invalid_Handle),
          "forged future generation refused");
   Check (Refused_As ((Self_Word and not Slot_Mask) or Slot_Mask, ABI.Invalid_Handle),
          "forged slot past the table refused");
   Check (not Names_Entry (ERROR_RESULT), "all-ones word refused");
   declare
      Forged_Call : aliased Message := NULL_MESSAGE;
      --  The reply tag's label is its low word: a refused call answers the
      --  null label (as an authorityless one does below), or the error word.
      Label_Mask : constant Unsigned_64 := 16#FFFF_FFFF#;
      Answer : constant Unsigned_64 :=
        syscall (CuBit.Kernel_ABI.Call_Via_Endpoint_Capability,
                 Self_Word + Generation_Step,
                 Address_Number (Forged_Call'Address), Wait_Forever);
   begin
      Check (Answer = ERROR_RESULT
             or else (Answer and Label_Mask) = Unsigned_64 (NULL_TAG.label),
             "call through a forged endpoint word refused");
   end;

   declare
      D : constant IPC.Description := IPC.Describe (Self_Process);
   begin
      Check (D.Current and then D.Kind = ABI.Process
             and then D.Held = Read_Write,
             "self process rights attenuated");
   end;

   --  KERN-003: an identity names one life. The self endpoint names this
   --  process by its identity.
   declare
      D : constant IPC.Description := IPC.Describe (Self);
   begin
      Check (D.Current and then D.Kind = ABI.Endpoint and then D.Object = PID,
             "self endpoint names this identity");
      Check (D.Current and then not D.Held.Grant,
             "self endpoint cannot be passed on");
   end;

   --  Nothing derives from a capability without Grant, whatever is asked:
   --  the same rights, fewer, or more (rights never widen).
   declare
      Same  : constant Endpoint := CuBit.Handles.Control.Derive (Copy (Self, Read_Write));
      Fewer : constant Endpoint := CuBit.Handles.Control.Derive (Copy (Self, Read_Only));
      More  : constant Endpoint := CuBit.Handles.Control.Derive
        (Copy (Self, (Read | Write | Grant => True, others => False)));
      Process_Copy : constant CuBit.Handles.Process :=
        CuBit.Handles.Control.Derive
          (Copy (Self_Process, (Read | Write | Execute | Grant => True, others => False)));
   begin
      Check (not Valid (Same) and then not Valid (Fewer) and then not Valid (More)
             and then not Valid (Process_Copy),
             "no derivation from a capability without grant");
      Check (IPC.Describe (Self).Held = Read_Write
             and then IPC.Describe (Self_Process).Held = Read_Write,
             "refused derivations leave the source unchanged");
   end;

   --  An endpoint/notification grant, not hardware IRQ ownership or a
   --  caller-spelled PID, is required to publish an unsolicited event.
   Process_Manager := Registered_Driver (DRIVER_PROCMGR);
   Check (not CuBit.Handles.Devices.Send_Event (Process_Manager, NULL_MESSAGE, No_Notification),
          "ambient event publication denied");
   Check (syscall (Raw_Send_Event, To_Word (Process_Manager), 0, 0, 0, 0, 0)
            = ERROR_RESULT,
          "raw event publication without a handle denied");
   Check (not CuBit.Handles.Devices.Send_Event (Process_Manager, NULL_MESSAGE, Self),
          "an endpoint to itself does not authorize events to another process");

   --  Deliberately invoke the retired ABI number, not a runtime alias.
   --  Unknown syscalls return zero; the old unchecked submit returned one.
   Result := syscall (23, To_Word (Process_Manager), 0, 0, 0, 0,  -- capability-check: forged on purpose
                      NO_COMPLETION_TOKEN);
   Check (Result = 0, "retired PID submit rejected");
   --  The retired INFO (15), RECEIVE_EVENT (20), VIRT_TO_PHYS (50),
   --  SET_SYSINFO (77) and SET_WELL_KNOWN (107) are unknown now
   --  (docs/syscall-retirement.md); each used to answer something nonzero
   --  here (the secondary stack, a physical address, a registration).
   Check (syscall (15, 1001) = 0, "retired INFO rejected");  -- capability-check: forged on purpose
   Check (syscall (50, Address_Number (Main'Address)) = 0, "retired VIRT_TO_PHYS rejected");  -- capability-check: forged on purpose
   Check (syscall (77, 1403, 1) = 0, "retired SET_SYSINFO rejected");  -- capability-check: forged on purpose
   Check (syscall (107, 1, PID) = 0, "retired SET_WELL_KNOWN rejected");  -- capability-check: forged on purpose
   --  Their replacements: the publications, and the typed self query.
   declare
      Statistics : CuBit.System_Facts.SP.Statistics;
      Read : Boolean;
   begin
      Check (CuBit.System_Facts.Processor_Count >= 1 and then
             CuBit.System_Facts.Memory_Bytes > 0, "system facts published");
      Check (Registered_Driver (DRIVER_PROCMGR) /= CuBit.Messages.No_Process,
             "the service registry names procmgr");
      CuBit.System_Facts.Read_Statistics (Statistics, Read);
      Check (Read and then Statistics.Owned_Bytes > 0 and then
             Statistics.Free_Memory_Bytes > 0 and then
             Statistics.Free_Memory_Bytes < CuBit.System_Facts.Memory_Bytes,
             "own statistics read");
      Check (not CuBit.System_Facts.Publish
               (CuBit.System_Facts.SP.Wall_Clock_Offset, 1),
             "only the clock service publishes the wall clock");
      Check (not CuBit.System_Facts.Publish
               (CuBit.System_Facts.SP.Boot_Display_Native, 1),
             "only devmgr publishes the boot display handoff");
   end;
   Check (not capSubmit (No_Endpoint, NULL_MESSAGE, NO_COMPLETION_TOKEN),
          "authorityless capability submit rejected");
   --  The flags only the kernel sets are refused on what a process sends,
   --  never silently cleared (a valid handle, so only the flag refuses).
   --  The protocol's own flags reach the receiver unchanged.
   declare
      type IPC_Flags_Array is array (Positive range <>) of IPC_Flags;
      Kernel_Only : constant IPC_Flags_Array :=
        [(Capability_Transferred => True, others => False),
         (Capability_Refused => True, others => False),
         (Capability_Offered => True, others => False)];
      Forged : Message := NULL_MESSAGE;
      Refused : Boolean := True;
      Delivered : Boolean := True;
   begin
      for Claim of Kernel_Only loop
         Forged.tag.ipc := Claim;
         Refused := Refused and then not capSubmit (Self, Forged, NO_COMPLETION_TOKEN);
      end loop;
      Check (Refused, "kernel-only message flags refused");
      for Value in Protocol_Flags loop
         declare
            Sent : Message := NULL_MESSAGE;
            Kept : aliased Message := NULL_MESSAGE;
         begin
            Sent.tag.label := Probe_Label;
            Sent.tag.protocol := Value;
            Delivered := Delivered
              and then capSubmit (Self, Sent, NO_COMPLETION_TOKEN)
              and then syscall (SYSCALL_POLL_ANY_IPC,
                                Unsigned_64 (System.Storage_Elements.To_Integer (Kept'Address))) /= 0
              and then Kept.tag.label = Probe_Label
              and then Kept.tag.protocol = Value
              and then Kept.tag.ipc = No_IPC_Flags;
         end;
      end loop;
      Check (Delivered, "protocol flags delivered unchanged");
   end;
   --  A synchronous call through it is refused too (the IPC fast path,
   --  docs/ipc-fastpath.md, checks authority once, in capSend).
   declare
      Call_Message : Message := NULL_MESSAGE;
   begin
      Check (capCall (No_Endpoint, Call_Message, CuBit.Messages.Wait_Forever).label = NULL_TAG.label,
             "authorityless capability call rejected");
   end;

   --  Hostile user pointers (docs/ipc-fastpath.md, "User memory"): every
   --  IPC system call copies through the kernel's checked copier, so a
   --  kernel address, a read-only user page and an unmapped user page are
   --  refused (not written, not read, and no kernel fault).
   declare
      Kernel_Address   : constant Unsigned_64 := 16#FFFF_8000_0010_0000#;
      Unmapped_Address : constant Unsigned_64 := 16#0000_7000_0000_0000#;
      --  The program's own code: mapped, user, read-only.
      Read_Only_Address : constant Unsigned_64 :=
        Unsigned_64 (System.Storage_Elements.To_Integer (Main'Address));
      Error : constant Unsigned_64 := ERROR_RESULT;
      Pointers : constant array (1 .. 3) of Unsigned_64 :=
        [Kernel_Address, Read_Only_Address, Unmapped_Address];
      function Name (P : Positive) return String is
        (case P is when 1 => "kernel", when 2 => "read-only", when others => "unmapped");
   begin
      for P in Pointers'Range loop
         --  A message is waiting (sent to this process itself), so a
         --  receive that wrote through the pointer would report its sender.
         --  Refused, the message stays queued for a valid buffer.
         declare
            Pending : Message := NULL_MESSAGE;
            Queued : Boolean;
         begin
            Pending.tag.label := Probe_Label;
            Queued := capSubmit (Self, Pending, NO_COMPLETION_TOKEN);
            Check (Queued, "probe queued to itself (" & Name (P) & ")");
         end;
         Check (syscall (SYSCALL_POLL_ANY_IPC, Pointers (P)) = 0,
                "receive into a " & Name (P) & " address refused");
         declare
            Kept : aliased Message := NULL_MESSAGE;
         begin
            Check (syscall (SYSCALL_POLL_ANY_IPC,
                            Unsigned_64 (System.Storage_Elements.To_Integer (Kept'Address))) /= 0
                   and then Kept.tag.label = Probe_Label,
                   "the refused message was kept (" & Name (P) & ")");
         end;
         --  Through the real self endpoint, so only the pointer is wrong.
         Check (syscall (CuBit.Kernel_ABI.Call_Via_Endpoint_Capability,
                         Self_Word, Pointers (P), Wait_Forever) = Error,
                "call from a " & Name (P) & " address refused");
         Check (syscall (SYSCALL_WAIT_COMPLETION, Pointers (P), 1, 0) = Error,
                "completions into a " & Name (P) & " address refused");
      end loop;
      Check (syscall (SYSCALL_WRITE, 1, Kernel_Address, 64) = 0,
             "write from a kernel address prints nothing");
   end;

   --  The retired minting calls are unknown: no authority from nothing,
   --  and nothing placed at a number of the caller's choosing.
   Check (syscall (Retired_Policy_Mint, PID, ABI.Kind'Enum_Rep (ABI.Endpoint), PID, 0,
                   ABI.Right_Read or ABI.Right_Write, 32) = 0
          and then syscall (Retired_Mint_Incarnation, PID, ABI.Kind'Enum_Rep (ABI.Endpoint),
                            PID, 0, ABI.Right_Read or ABI.Right_Write, 32) = 0
          and then syscall (Retired_Delegate_Endpoint, PID, 0, 32, 0, 0) = 0
          and then syscall (Retired_Restore_Reply, 32) = 0,
          "retired mint, delegate and restore calls rejected");
   Check (Refused_As (32, ABI.Invalid_Handle),
          "mint denial leaves the old slot number empty");

   --  A capability is taken only where one was offered, while answering
   --  that call: with no call from procmgr in hand, nothing is taken.
   declare
      Taken : constant IPC.Carried := IPC.Accept_Offered (Process_Manager);
      Self_Taken : constant IPC.Carried := IPC.Accept_Offered (Own_Process);
   begin
      Check (Taken.Kind = IPC.Refused and then Taken.Why = ABI.Not_Permitted,
             "no offer taken without a call to answer");
      Check (Self_Taken.Kind = IPC.Refused, "no offer taken from itself");
   end;
   --  A capability is given only in a reply to a request that asked: with
   --  no request from procmgr in hand, Reply_With is refused.
   declare
      Given : constant IPC.Reply_Result :=
        IPC.Reply_With (Process_Manager, NULL_MESSAGE, Copy (Self, Read_Only));
   begin
      Check (Given.Outcome = IPC.Refused, "no capability given without a request");
   end;

   --  An authorityless process cannot spawn, and its own process handle
   --  (Read and Write, no Execute) is no spawn authority either. A null ELF
   --  is never dereferenced: authority admission precedes ELF validation.
   declare
      Ambient : constant CuBit.Handles.Control.Spawn_Result :=
        CuBit.Handles.Control.Spawn
          (CuBit.Handles.No_Process, System.Null_Address, 0, 0, "", Own_Process,
           (Kind => ST.Standard_Limit));
      By_Self : constant CuBit.Handles.Control.Spawn_Result :=
        CuBit.Handles.Control.Spawn
          (Self_Process, System.Null_Address, 0, 0, "", Own_Process,
           (Kind => ST.Standard_Limit));
   begin
      Check (not Ambient.Spawned, "ambient spawn denied");
      Check (not By_Self.Spawned, "spawn through the self process denied");
      Check (syscall (Raw_Spawn) /= 0, "raw spawn without a handle denied");
   end;

   --  The table's semantics, with handles this process can create: each
   --  request it submits to itself carries a reply authority, which
   --  Save_Reply places in its table (kernel-chosen, generation-checked).
   declare
      type Saved_Index is range 1 .. Handle_Limit;
      Saved : array (Saved_Index) of CuBit.Handles.Reply := [others => No_Reply];
      Count : Natural := 0;
      Last_Refusal : ABI.Refusal := ABI.Invalid_Argument;
      Token : Unsigned_64 := Self_Token_First;
      Refused_At_Limit : Boolean := False;

      --  Ask itself once and save the reply authority of that request.
      procedure Save_One (Into : out CuBit.Handles.Reply; Why : out ABI.Refusal; OK : out Boolean) is
         Request : Message := NULL_MESSAGE;
         From : Process_ID;
         Got : Message;
         Found : Boolean;
      begin
         Into := No_Reply;
         Why := ABI.Invalid_Argument;
         OK := False;
         Request.tag.label := Probe_Label;
         Token := Token + 1;
         if not capSubmit (Self, Request, Token) then
            return;
         end if;
         Poll_Service_Request (From, Got, Found);
         if not Found or else Got.tag.label /= Probe_Label then
            return;
         end if;
         declare
            Result : constant IPC.Saved_Reply := IPC.Save_Reply;
         begin
            if Result.Saved then
               Into := Result.Handle;
               OK := True;
            else
               Why := Result.Why;
               --  Not saved: answer it now, so its completion is not lost.
               Found := CuBit.Messages.reply (From, NULL_MESSAGE) /= ERROR_RESULT;
            end if;
         end;
      end Save_One;

      procedure Drain_Completions is
         Completion : aliased CompletionEntry;
      begin
         while Poll_Completion (Completion'Address) /= 0 loop
            null;
         end loop;
      end Drain_Completions;

      OK : Boolean;
      Old : CuBit.Handles.Reply;
      Fresh : CuBit.Handles.Reply;
      Why : ABI.Refusal;
   begin
      --  The handle limit: placements stop at the declared limit with
      --  Limit_Reached, never by overwriting.
      for Attempt in Saved_Index loop
         Save_One (Saved (Attempt), Why, OK);
         if OK then
            Count := Count + 1;
         else
            Last_Refusal := Why;
            Refused_At_Limit := Why = ABI.Limit_Reached;
            exit;
         end if;
      end loop;
      Check (Count = Handle_Limit - Self_Entries and then Refused_At_Limit,
             "handle limit reached (Limit_Reached)" &
             (if Refused_At_Limit then "" else " got " & ABI.Refusal'Image (Last_Refusal)));
      Check (IPC.Describe (Self).Current, "the self entries survive the limit");

      if Count >= 1 then
         --  A closed handle is refused; closing frees one place.
         Old := Saved (1);
         Check (IPC.Close (Old).Closed, "close a saved reply");
         declare
            Again : constant IPC.Close_Result := IPC.Close (Old);
         begin
            Check (not Again.Closed and then Again.Why = ABI.Stale_Handle,
                   "a closed handle is refused (Stale)");
         end;
         Check (not IPC.Answer (Old, NULL_MESSAGE), "a closed reply cannot answer");
         Save_One (Fresh, Why, OK);
         Check (OK, "closing frees a place under the limit");
         --  A stale handle never reaches what was placed after it, even in
         --  the slot it named.
         if OK then
            declare
               Stale : constant IPC.Close_Result := IPC.Close (Old);
            begin
               Check (not Stale.Closed and then Stale.Why = ABI.Stale_Handle,
                      "a stale handle is refused after its slot is reused");
            end;
            Check (IPC.Close (Fresh).Closed,
                   "the new entry was untouched by the stale handle");
         end if;
         for Index in Saved_Index range 2 .. Saved_Index (Count) loop
            Check (IPC.Close (Saved (Index)).Closed, "close the remaining saved replies");
         end loop;
      end if;
      Drain_Completions;

      --  Closing its own self entry: later use is refused, and the raw word
      --  is stale too.
      Check (IPC.Close (Self_Process).Closed, "close the self process entry");
      declare
         D : constant IPC.Description := IPC.Describe (Self_Process);
      begin
         Check (not D.Current and then D.Why = ABI.Stale_Handle,
                "a closed process handle is refused (Stale)");
      end;
   end;

   if Passed then
      debugPrint ("capability-test: all tests passed" & LF);
   else
      debugPrint ("capability-test: test failure" & LF);
   end if;

   loop
      Result := syscall (SYSCALL_SLEEP, 1_000);
   end loop;
end Main;
