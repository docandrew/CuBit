pragma Ada_2022;
with ACPI_Backend_Firmware_Native;
with ACPI_Backend_NVS_Native;
with ACPI_Backend_FACS_Binding;
with ACPI_Backend_Global_Signal;
with ACPI_Backend_Global_Lock;
with ACPI_Global_Lock_Scoped_Sessions;
with ACPI_Global_Lock_Requests;
with ACPI_Global_Lock_Maintenance;
with ACPI_Backend_Memory_Access;
with ACPI_Backend_Port_Access;
with ACPI_PM1_Global_Release;
with ACPI_FACS_Header;
with ACPI_FADT.Registers;
with ACPI_Native_Log;
with ACPI_Native_Lock_Notes;
package body ACPI_Local_Hardware is
   use Interfaces;
   package Firmware renames ACPI_Backend_Firmware_Native;
   package Memory_Classes renames CuBit.ACPI_Memory_Classes;
   use type B.Tick, Mapping_Owner.Outcome, Firmware.Policy.Outcome, Bindings.Owner.Outcome;
   procedure Map_Window (Held : CuBit.Handles.Device_Memory;
      Physical, Virtual, Pages : Word; Read_Only : Boolean; Mapped : out Boolean) is
   begin
      Mapping_Owner.Map_Window (Mapping,Held,Physical,Virtual,Pages,Read_Only,Last_Mapping);
      Mapped := Last_Mapping.Status = Mapping_Owner.Window_Mapped;
   end Map_Window;
   function Startup_Time_Remains return Boolean is
      Now : B.Tick;
      Available : Boolean;
   begin
      ACPI_Backend_Kernel_Handles.Read_Clock (Now,Available);
      if not Available or else Now < Last_Startup_Time or else Now >= Until_Time then return False; end if;
      Last_Startup_Time := Now; return True;
   end Startup_Time_Remains;
   Bound : Bindings.Owner.Report;
      type Memory_Facts is array (1 .. CuBit.ACPI_Windows.Maximum_Memory_Windows)
        of Firmware.Policy.Accepted_Memory;
      Validated_Memory : Memory_Facts;
      -- Window IDs follow the trusted startup batch (ports, then memory).
      -- IDs alone establish no class or extent: compare the retained view
      -- with a fresh inspection of that exact retained launch handle.
      function Is_NVS (Window : Catalog.W.Window_ID) return Boolean is
         use type Catalog.W.Window_ID, Catalog.W.P.Address_Space;
      begin
         for I in 1 .. Grant.Memory_Used loop
            if Window = Catalog.W.Window_ID (Grant.IO_Used + I) then
               declare
                  Actual : constant Firmware.Policy.Report := Firmware.Inspect_Claim
                    (Grant.Memory (I).Memory, Grant.Memory (I).First_Page, Grant.Memory (I).Pages,
                     Grant.Memory (I).Kind, Grant.Memory (I).Cache);
                  View : constant Catalog.W.Window_Result := Catalog.Describe_Window (Hardware, Window);
               begin
                  return Actual.Status = Firmware.Policy.Admitted
                    and then Firmware.Policy.Allows_Named_Reads (Actual.Memory)
                    and then View.Allowed and then View.Value.Space = Catalog.W.P.Memory_Space
                    and then View.Value.Base = Actual.Memory.First_Page * Memory_Classes.Page_Bytes
                    and then View.Value.Length = Actual.Memory.Pages * Memory_Classes.Page_Bytes;
               end;
            end if;
         end loop;
         return False;
      end Is_NVS;
      package NVS_Native is new ACPI_Backend_NVS_Native
        (Catalog, Hardware, Is_NVS, CuBit.ACPI_Windows.Maximum_Memory_Windows);
      package NVS renames NVS_Native.Records;
      use type NVS.Outcome;
      NVS_State : NVS.Context;
      NVS_Status : NVS.Outcome;
      type Lock_Scope is new Unsigned_64;
      procedure Lock_Clock (Value : out Unsigned_64; Good : out Boolean) is
         Raw : B.Tick;
      begin
         ACPI_Backend_Kernel_Handles.Read_Clock (Raw, Good); Value := Unsigned_64 (Raw);
      end Lock_Clock;
      function Coherent_FACS (Window : Catalog.W.Window_ID) return Boolean is
         use type Catalog.W.Window_ID, Catalog.W.P.Address_Space;
      begin
         for I in 1 .. Grant.Memory_Used loop
            if Window = Catalog.W.Window_ID (Grant.IO_Used + I) then
               declare
                  Actual : constant Firmware.Policy.Report := Firmware.Inspect_Claim
                    (Grant.Memory (I).Memory, Grant.Memory (I).First_Page, Grant.Memory (I).Pages,
                     Grant.Memory (I).Kind, Grant.Memory (I).Cache);
                  View : constant Catalog.W.Window_Result := Catalog.Describe_Window (Hardware, Window);
               begin
                  return Actual.Status = Firmware.Policy.Admitted
                    and then Firmware.Policy.Coherent_For_FACS (Actual.Memory)
                    and then View.Allowed and then View.Value.Space = Catalog.W.P.Memory_Space
                    and then View.Value.Readable and then View.Value.Writable
                    and then View.Value.Base = Actual.Memory.First_Page * Memory_Classes.Page_Bytes
                    and then View.Value.Length = Actual.Memory.Pages * Memory_Classes.Page_Bytes;
               end;
            end if;
         end loop;
         return False;
      end Coherent_FACS;
      procedure FACS_Byte (Virtual : Unsigned_64; Value : out Unsigned_8; Completed : out Boolean) is
         Read_Back : Unsigned_64;
      begin
         ACPI_Backend_Memory_Access.Transfer
           (Virtual, Catalog.W.T.Byte_Access, False, 0, Read_Back, Completed);
         Value := (if Completed then Unsigned_8 (Read_Back) else 0);
      end FACS_Byte;
      package FACS is new ACPI_Backend_FACS_Binding
        (Catalog.Bindings, Catalog.Context, Catalog.Describe_Window, Catalog.Select_Access, Coherent_FACS, FACS_Byte);
      FACS_State : FACS.Context;
      FACS_Result : FACS.Report;
      procedure Signal_Word
        (View : Catalog.Bindings.Access_View; For_Write : Boolean; Input : Unsigned_16;
         Output : out Unsigned_16; Status : out ACPI_PM1_Global_Release.Completion) is
         Value : Unsigned_64 := 0;
         Completed : Boolean := False;
         use type Catalog.W.P.Address_Space;
      begin
         Output := 0; Status := ACPI_PM1_Global_Release.Not_Attempted;
         if not View.Allowed then return; end if;
         if View.Space = Catalog.W.P.Memory_Space then
            ACPI_Backend_Memory_Access.Transfer
              (View.Virtual_Address, Catalog.W.T.Word_Access, For_Write, Unsigned_64 (Input), Value, Completed);
         else
            ACPI_Backend_Port_Access.Transfer
              (View.Ports, View.Port, Catalog.W.T.Word_Access, For_Write, Unsigned_64 (Input), Value, Completed);
         end if;
         Status := ACPI_PM1_Global_Release.Classify_Transfer (For_Write, Completed, Value);
         if Completed and then Value <= Unsigned_64 (Unsigned_16'Last) then Output := Unsigned_16 (Value); end if;
      end Signal_Word;
      package Signaling is new ACPI_Backend_Global_Signal
        (Catalog.Bindings, Catalog.Context, Catalog.Select_Access, Signal_Word);
      Signal_State : Signaling.Context;
      Signal_Result : Signaling.Report;
      procedure Observe_Signal (Report : Signaling.Report) is
         package N renames ACPI_Native_Lock_Notes;
      begin
         N.Observe_Signal
           ((case Report.Status is when Signaling.Signaled => N.Trace.Signaled,
              when Signaling.Not_Sent => N.Trace.Signal_Not_Sent,
              when others => N.Trace.Signal_Unknown),Report.Reads,Report.Writes);
      end Observe_Signal;
      package Physical_Lock is new ACPI_Backend_Global_Lock
        (Catalog.Bindings, Catalog.Context, FACS, Signaling, Hardware, FACS_State, Signal_State,
         Lock_Scope, 0, Lock_Clock,
         Observe_Word => ACPI_Native_Lock_Notes.Observe_Word,
         Observe_CAS => ACPI_Native_Lock_Notes.Observe_CAS,
         Observe_Signal => Observe_Signal);
      package Lock_Owner renames Physical_Lock.Owner;
      package Lock_Sessions is new ACPI_Global_Lock_Scoped_Sessions
        (CuBit.Messages.Process_ID, CuBit.Messages.No_Process, Lock_Scope, 0, Lock_Owner);
      package Lock_Requests is new ACPI_Global_Lock_Requests
        (CuBit.Messages.Process_ID, CuBit.Messages.No_Process, Lock_Scope, 0, Lock_Owner, Lock_Sessions);
      package Lock_Maintenance is new ACPI_Global_Lock_Maintenance
        (CuBit.Messages.Process_ID, CuBit.Messages.No_Process, Lock_Scope, 0, Lock_Owner, Lock_Sessions, Lock_Requests, Lock_Clock);
      Lock_State : Lock_Requests.Context;
      Lock_Schedule : Lock_Maintenance.Context;
      Lock_Status : Lock_Owner.Outcome;
      Last_Lock : Lock_Maintenance.Report;
      use type Lock_Owner.Outcome, FACS.Outcome, Signaling.Outcome, Signaling.Bank, Catalog.W.Window_ID;
      function Unique_Window
        (Space : Catalog.W.P.Address_Space; Address : Unsigned_64; Width : Catalog.W.T.Access_Width)
         return Catalog.W.Window_ID is
         Selected : Catalog.W.Window_ID := Catalog.W.No_Window;
         use type Catalog.W.P.Address_Space;
      begin
         if Space = Catalog.W.P.Memory_Space then
            for I in 1 .. Grant.Memory_Used loop
               if Firmware.Policy.Generic_Field_Conflict (Validated_Memory (I), Address, Unsigned_64 (Catalog.W.T.Octets (Width))) then
                  return Catalog.W.No_Window;
               end if;
            end loop;
         end if;
         for I in 1 .. Bound.Bound_Windows loop
            declare
               ID : constant Catalog.W.Window_ID := Catalog.W.Window_ID (I);
            begin
               if Catalog.Select_Access (Hardware, ID, Space, Address, Width, False).Allowed
                 and then Catalog.Select_Access (Hardware, ID, Space, Address, Width, True).Allowed
               then
                  if Selected /= Catalog.W.No_Window then return Catalog.W.No_Window; end if;
                  Selected := ID;
               end if;
            end;
         end loop;
         return Selected;
      end Unique_Window;
      procedure Prepare_Lock is
         Address : constant Unsigned_64 :=
           (if Facts.X_FACS.Present and then Facts.X_FACS.Value /= 0 then Facts.X_FACS.Value else Unsigned_64 (Facts.FACS));
         Window : Catalog.W.Window_ID := Catalog.W.No_Window;
         Banks : Signaling.Windows := [others => Catalog.W.No_Window];
         use type Catalog.W.P.Address_Space;
      begin
         if Address /= 0 and then Address <= Unsigned_64'Last - Unsigned_64 (ACPI_FACS_Header.Minimum_Length - 1) then
            for I in 1 .. Grant.Memory_Used loop
               declare
                  ID : constant Catalog.W.Window_ID := Catalog.W.Window_ID (Grant.IO_Used + I);
               begin
                  if Coherent_FACS (ID)
                    and then Catalog.Select_Access (Hardware, ID, Catalog.W.P.Memory_Space, Address, Catalog.W.T.Byte_Access, False).Allowed
                    and then Catalog.Select_Access (Hardware, ID, Catalog.W.P.Memory_Space, Address + Unsigned_64 (ACPI_FACS_Header.Minimum_Length - 1), Catalog.W.T.Byte_Access, False).Allowed
                  then
                     if Window /= Catalog.W.No_Window then
                        ACPI_Native_Log.Warning ("acpi: stage global-lock fail AMBIGUOUS_FACS"); return;
                     end if;
                     Window := ID;
                  end if;
               end;
            end loop;
         end if;
         FACS.Prepare (FACS_State, Hardware, Facts, Window, FACS_Result);
         if FACS_Result.Status /= FACS.Ready then
            ACPI_Native_Log.Info ("acpi: stage global-lock skip " & FACS_Result.Status'Image); return;
         end if;
         for Bank in Signaling.Bank loop
            declare
               Kind : constant ACPI_FADT.Block_Kind :=
                 (if Bank = Signaling.PM1A then ACPI_FADT.PM1A_Control else ACPI_FADT.PM1B_Control);
               Block : constant ACPI_FADT.Registers.Block_Result := ACPI_FADT.Registers.Describe (Facts, Kind, Unsigned_64'Last);
               use type ACPI_FADT.Registers.Status, ACPI_FADT.Registers.Space_Kind;
            begin
               if Block.Code = ACPI_FADT.Registers.Described then
                  Banks (Bank) := Unique_Window
                    ((if Block.Space = ACPI_FADT.Registers.System_IO then Catalog.W.P.IO_Space else Catalog.W.P.Memory_Space),
                     Block.Base, Catalog.W.T.Word_Access);
               end if;
            end;
         end loop;
         Signaling.Prepare (Signal_State, Hardware, Facts, Banks, Signal_Result);
         if Signal_Result.Status /= Signaling.Prepared then
            ACPI_Native_Log.Warning ("acpi: stage global-lock fail SIGNAL_" & Signal_Result.Status'Image); return;
         end if;
         if not CuBit.Messages.Is_Process (Self) then
            ACPI_Native_Log.Warning ("acpi: stage global-lock fail SELF_UNAVAILABLE"); return;
         end if;
         -- The full kernel process incarnation supplies the nonzero domain;
         -- no narrowed PID, time-derived identity or caller payload is used.
         Lock_Requests.Configure (Lock_State, Lock_Scope (Local_Scope), CuBit.Messages.To_Word (Self), Lock_Status);
         if Lock_Status = Lock_Owner.Configured then
            ACPI_Native_Log.Info ("acpi: stage global-lock ok finite-lease access=LOCAL_TYPED_OWNER");
         else ACPI_Native_Log.Warning ("acpi: stage global-lock fail " & Lock_Status'Image); end if;
      end Prepare_Lock;
      function To_Lock_Outcome (Value : Lock_Owner.Outcome) return ACPI_Global_Lock_Protocol.Lock_Outcome is
        (case Value is
           when Lock_Owner.Configured => ACPI_Global_Lock_Protocol.Configured,
           when Lock_Owner.Invalid_Configuration => ACPI_Global_Lock_Protocol.Invalid_Configuration,
           when Lock_Owner.Already_Configured => ACPI_Global_Lock_Protocol.Already_Configured,
           when Lock_Owner.Acquired => ACPI_Global_Lock_Protocol.Acquired,
           when Lock_Owner.Acquired_After_Deadline => ACPI_Global_Lock_Protocol.Acquired_After_Deadline,
           when Lock_Owner.Acquired_Clock_Failed => ACPI_Global_Lock_Protocol.Acquired_Clock_Failed,
           when Lock_Owner.Released => ACPI_Global_Lock_Protocol.Released,
           when Lock_Owner.Waiting_For_Firmware => ACPI_Global_Lock_Protocol.Waiting_For_Firmware,
           when Lock_Owner.Contended => ACPI_Global_Lock_Protocol.Contended,
           when Lock_Owner.Expired => ACPI_Global_Lock_Protocol.Expired,
           when Lock_Owner.Clock_Failed => ACPI_Global_Lock_Protocol.Clock_Failed,
           when Lock_Owner.Clock_Reversed => ACPI_Global_Lock_Protocol.Clock_Reversed,
           when Lock_Owner.Invalid_Deadline => ACPI_Global_Lock_Protocol.Invalid_Deadline,
           when Lock_Owner.Unauthorized => ACPI_Global_Lock_Protocol.Unauthorized,
           when Lock_Owner.Not_Ready => ACPI_Global_Lock_Protocol.Not_Ready,
           when Lock_Owner.Already_Held => ACPI_Global_Lock_Protocol.Already_Held,
           when Lock_Owner.Stale_Lease => ACPI_Global_Lock_Protocol.Stale_Lease,
           when Lock_Owner.Lease_Exhausted => ACPI_Global_Lock_Protocol.Lease_Exhausted,
           when Lock_Owner.Read_Unavailable => ACPI_Global_Lock_Protocol.Read_Unavailable,
           when Lock_Owner.Atomic_Refused => ACPI_Global_Lock_Protocol.Atomic_Refused,
           when Lock_Owner.Atomic_Indeterminate => ACPI_Global_Lock_Protocol.Atomic_Indeterminate,
           when Lock_Owner.Signal_Deferred => ACPI_Global_Lock_Protocol.Signal_Deferred,
           when Lock_Owner.Signal_Indeterminate => ACPI_Global_Lock_Protocol.Signal_Indeterminate,
           when Lock_Owner.Reentered => ACPI_Global_Lock_Protocol.Reentered);

   Issued_Nonce : Word := 0;
   function Observe return Report is (Info);
   function Pending return Boolean is
      Engine : constant Lock_Owner.Statistics := Lock_Requests.Observe (Lock_State).Engine;
   begin
      return Catalog.Observe (Hardware).Working or else Catalog.Observe (Hardware).Pending /= 0
        or else Engine.Working or else Engine.Stage in Lock_Owner.Held | Lock_Owner.Acquire_Uncertain |
           Lock_Owner.Release_Uncertain | Lock_Owner.Signal_Uncertain;
   end Pending;
   function Admitting return Boolean is
      Engine : constant Lock_Owner.Statistics := Lock_Requests.Observe (Lock_State).Engine;
   begin
      return State = Ready and then not Catalog.Observe (Hardware).Working
        and then Catalog.Observe (Hardware).Pending = 0 and then not Catalog.Observe (Hardware).Quarantined
        and then not Engine.Working and then Engine.Stage in Lock_Owner.Fresh | Lock_Owner.Ready;
   end Admitting;
   function May_Exit return Boolean is
     (Lock_Requests.May_Exit (Lock_State) and then not Catalog.Observe (Hardware).Working
      and then Catalog.Observe (Hardware).Pending = 0);
   procedure Claim_Unused_Arena (First, Limit : out Word; Accepted : out Boolean) is
   begin
      First := 0; Limit := 0; Accepted := False;
      if State = Ready then Catalog.Claim_Unused_Device_Arena (Hardware,First,Limit,Accepted); end if;
   end Claim_Unused_Arena;
   procedure Initialize (Input : aliased in CuBit.ACPI_Windows.Grant;
      FADT : ACPI_FADT.Description; Deadline : B.Tick; Result : out Report)
   is
      Now : B.Tick;
      Available, Sealed : Boolean;
      Mapping_Status : Mapping_Owner.Outcome;
      procedure Fail (Why : Reason) is
      begin
         State := Failed; Info.Stage := Failed; Info.Status := Why; Result := Info;
         ACPI_Native_Log.Warning ("acpi: stage local-hardware fail " & Why'Image &
            " binding=" & Info.Binding.Status'Image & " mapping=" & Last_Mapping.Status'Image);
      end Fail;
   begin
      Result := Info;
      if State /= Fresh then Result.Status := Already_Attempted; return; end if;
      State := Initializing; Info.Stage := Initializing;
      if not Arena_Parameters_Valid or else Local_Scope = 0
        or else not Arena_Is_Exclusive (Virtual_First,Virtual_Limit)
      then Fail (Invalid_Arena); return; end if;
      Self := CuBit.Messages.Own_Process;
      if not CuBit.Messages.Is_Process (Self) then Fail (Invalid_Identity); return; end if;
      ACPI_Backend_Kernel_Handles.Read_Clock (Now,Available);
      if not Available or else Deadline = B.No_Finite_Deadline or else Now >= Deadline
      then Fail (Invalid_Clock); return; end if;
      Last_Startup_Time := Now; Until_Time := Deadline;
      -- Copy only initialized launch entries before callbacks. Retain them for
      -- this package's entire lifetime, including partial mapping and stop.
      Grant.IO_Used := Input.IO_Used; Grant.Memory_Used := Input.Memory_Used;
      for I in 1 .. Input.IO_Used loop Grant.IO (I) := Input.IO (I); end loop;
      for I in 1 .. Input.Memory_Used loop Grant.Memory (I) := Input.Memory (I); end loop;
      Facts := FADT;
      if Grant.IO_Used = 0 and then Grant.Memory_Used = 0 then Fail (Empty_Grant); return; end if;
      for I in 1 .. Grant.Memory_Used loop
         declare Actual : constant Firmware.Policy.Report := Firmware.Inspect_Claim
           (Grant.Memory (I).Memory,Grant.Memory (I).First_Page,Grant.Memory (I).Pages,
            Grant.Memory (I).Kind,Grant.Memory (I).Cache);
         begin
            if Actual.Status /= Firmware.Policy.Admitted then Fail (Invalid_Memory_Class); return; end if;
            Validated_Memory (I) := Actual.Memory;
         end;
      end loop;
      Mapping_Owner.Configure (Mapping,Deadline,Mapping_Status);
      if Mapping_Status /= Mapping_Owner.Configured then Fail (Mapping_Refused); return; end if;
      -- Tables were already copied and parsed by Core; no second snapshot.
      declare
         package Registers renames ACPI_FADT.Registers;
         Timer : constant Registers.Block_Result := Registers.Describe (Facts,ACPI_FADT.PM_Timer,Unsigned_64'Last);
         use type Registers.Status, Registers.Space_Kind;
      begin
         if Timer.Code = Registers.Described and then Timer.Space = Registers.System_Memory then
            for I in 1 .. Grant.Memory_Used loop
               if Firmware.Policy.Generic_Field_Conflict (Validated_Memory (I),Timer.Base,Unsigned_64 (Timer.Length)) then
                  Fail (Control_Timer_Refused); return;
               end if;
            end loop;
         end if;
      end;
      Bindings.Initialize (Bound_Run,Hardware,Grant,Facts,Deadline,Bound);
      Info.Binding := Bound.Binding;
      if Bound.Status /= Bindings.Owner.Ready then Fail (Binding_Refused); return; end if;
      Mapping_Owner.Seal (Mapping,Sealed);
      if not Sealed then Fail (Mapping_Incomplete); return; end if;
      NVS.Configure (NVS_State,Local_Scope,NVS_Status);
      if NVS_Status /= NVS.Configured then Fail (NVS_Refused); return; end if;
      for I in 1 .. Grant.Memory_Used loop
         if not Startup_Time_Remains then Fail (Deadline_Stopped); return; end if;
         if Firmware.Policy.Allows_Named_Reads (Validated_Memory (I)) then
            NVS.Add (NVS_State,NVS.Record_ID (I),Catalog.W.Window_ID (Grant.IO_Used+I),NVS_Status);
            if NVS_Status /= NVS.Added then Fail (NVS_Refused); return; end if;
         end if;
      end loop;
      NVS.Seal (NVS_State,NVS_Status);
      if NVS_Status /= NVS.Sealed then Fail (NVS_Refused); return; end if;
      if not Startup_Time_Remains then Fail (Deadline_Stopped); return; end if;
      Prepare_Lock;
      if not Startup_Time_Remains then Fail (Deadline_Stopped); return; end if;
      Info.Registers := Catalog.Observe (Hardware).Registers;
      Info.NVS_Records := NVS.Observe (NVS_State).Records;
      Info.Lock_Ready := Lock_Requests.Observe (Lock_State).Configured;
      State := Ready; Info.Stage := Ready; Info.Status := Initialized; Result := Info;
      ACPI_Native_Log.Info ("acpi: stage local-hardware ok registers=" & Info.Registers'Image &
        " nvs=" & Info.NVS_Records'Image & " lock=" & Info.Lock_Ready'Image);
   end Initialize;
   procedure Acquire (Item : in out Lease; Deadline_Microseconds : Word;
      Budget : Lock_Protocol.Attempt_Budget; Result : out Lock_Protocol.Lock_Outcome) is
      Now : Word;
      Available : Boolean;
      Token : Lock_Requests.Ticket;
      Reply : Lock_Requests.Reply;
      Kind : Lock_Requests.Reply_Kind;
      use type Lock_Requests.Reply_Kind;
      use type Lock_Protocol.Lock_Outcome;
   begin
      Result := Lock_Protocol.Not_Ready;
      if State /= Ready or else Item.Prepared or else Issued_Nonce = Word'Last then return; end if;
      Issued_Nonce := Issued_Nonce+1;
      Lock_Clock (Now,Available);
      Lock_Requests.Prepare (Lock_State,Lock_Scope (Local_Scope),Self,Issued_Nonce,
        Now,Available,Deadline_Microseconds,Lock_Owner.Attempt_Budget (Budget),Token,Kind);
      if Kind /= Lock_Requests.Ready then return; end if;
      Item.Prepared := True; Item.Domain := Token.Domain; Item.Sequence := Token.Sequence;
      Lock_Requests.Acquire (Lock_State,Lock_Scope (Local_Scope),Self,Token,Reply);
      Result := To_Lock_Outcome (Reply.Outcome);
      -- A prepared request can be refused before it obtains a physical
      -- session (contention, invalid clock, unavailable word). Retire that
      -- local reservation only on independent idle-owner evidence. Keep
      -- every held/late/uncertain physical session for explicit cleanup.
      if Result /= Lock_Protocol.Acquired and then Lock_Requests.May_Exit (Lock_State) then
         Item.Prepared := False; Item.Domain := 0; Item.Sequence := 0;
      end if;
   end Acquire;
   procedure Check_Access (Item : Lease; Result : out Lock_Protocol.Lock_Outcome) is
      Reply : Lock_Requests.Reply;
   begin
      Result := Lock_Protocol.Not_Ready;
      if State /= Ready or else not Item.Prepared then return; end if;
      Lock_Requests.Check_Access (Lock_State,Lock_Scope (Local_Scope),Self,
        (Item.Domain,Item.Sequence),Reply);
      Result := To_Lock_Outcome (Reply.Outcome);
   end Check_Access;
   function Needs_Cleanup (Item : Lease) return Boolean is (Item.Prepared);
   procedure Release (Item : in out Lease; Result : out Lock_Protocol.Lock_Outcome) is
      Reply : Lock_Requests.Reply;
      Terminal : Lock_Requests.Reply;
      use type Lock_Requests.Reply_Kind;
   begin
      Result := Lock_Protocol.Not_Ready;
      if not Item.Prepared then return; end if;
      -- Maintenance records terminal cleanup separately from the original
      -- release result (which can remain Signal_Deferred/Indeterminate).
      Lock_Requests.Query (Lock_State,Lock_Scope (Local_Scope),Self,
        (Item.Domain,Item.Sequence),Lock_Requests.Termination,Terminal);
      if Terminal.Kind = Lock_Requests.Cached and then Terminal.Outcome = Lock_Owner.Released then
         Reply := Terminal;
      else
         Lock_Requests.Release (Lock_State,Lock_Scope (Local_Scope),Self,
           (Item.Domain,Item.Sequence),Reply);
      end if;
      Result := To_Lock_Outcome (Reply.Outcome);
      if Reply.Kind in Lock_Requests.Recorded | Lock_Requests.Cached
        and then Reply.Outcome = Lock_Owner.Released then
         Item.Prepared := False; Item.Domain := 0; Item.Sequence := 0;
      end if;
   end Release;
   procedure Begin_Stop is
   begin
      if State = Stopped then return; end if;
      State := Stopping; Info.Stage := Stopping;
      Lock_Requests.Begin_Stop (Lock_State,Lock_Status);
   end Begin_Stop;
   procedure Maintain (Wake_Deadline_Milliseconds : out Word; May_Continue : out Boolean) is
   begin
      ACPI_Native_Lock_Notes.Begin_Maintenance;
      Lock_Maintenance.Run (Lock_Schedule,Lock_State,Last_Lock);
      ACPI_Native_Lock_Notes.Finish_Maintenance (To_Lock_Outcome (Last_Lock.Cleanup));
      declare Engine : constant Lock_Owner.Statistics := Lock_Requests.Observe (Lock_State).Engine; begin
         ACPI_Native_Lock_Notes.Flush (not Engine.Working
            and then Engine.Stage in Lock_Owner.Fresh | Lock_Owner.Ready | Lock_Owner.Signal_Pending
            and then not Catalog.Observe (Hardware).Working and then Catalog.Observe (Hardware).Pending = 0);
      end;
      Wake_Deadline_Milliseconds := Last_Lock.Deadline;
      if State = Stopping and then May_Exit then State := Stopped; Info.Stage := Stopped; end if;
      May_Continue := State /= Stopped;
   end Maintain;
end ACPI_Local_Hardware;
