pragma Ada_2022;
with Ada.Command_Line;
with Interfaces;
with CuBit.Messages;
with CuBit.ACPI_Windows;
with CuBit.ACPI_Memory_Classes;
with CuBit.Handles;
with ACPI_Backend_Mapping_Startup;
with ACPI_Backend_Firmware_Native;
with ACPI_Backend_FADT_Native;
with ACPI_Backend_Binding_Types;
with ACPI_Backend_Kernel_Handles;
with ACPI_Backend_Native_Catalog;
with ACPI_Backend_Startup_Native;
with ACPI_Backend_Server;
with ACPI_Backend_Loop_Native;
with ACPI_Native_Log;
with ACPI_Native_Events;
with ACPI_FADT;
with ACPI_FADT.Registers;
with ACPI_Backend_NVS_Native;
with ACPI_Native_PCI_Windows;
with ACPI_Backend_Device_Arenas; use type ACPI_Backend_Device_Arenas.Outcome;
with CuBit.Process_IDs;
with ACPI_PCI_Protocol; use type ACPI_PCI_Protocol.Outcome;
with ACPI_Backend_Locked_NVS;
with ACPI_Backend_NVS_Endpoint;
with ACPI_NVS_Protocol;
with ACPI_Register_Roles;
with ACPI_FACS_Header;
with ACPI_Backend_FACS_Binding;
with ACPI_Backend_Global_Signal;
with ACPI_Backend_Global_Lock;
with ACPI_Backend_Memory_Access;
with ACPI_Backend_Port_Access;
with ACPI_PM1_Global_Release;
with ACPI_Global_Lock_Scoped_Sessions;
with ACPI_Global_Lock_Requests;
with ACPI_Global_Lock_Dispatch;
with ACPI_Global_Lock_Maintenance;
with ACPI_Global_Lock_Protocol;
with ACPI_Native_Lock_Notes;
with ACPI_Region_Protocol;
with Startup;
with CuBit.Handles.IPC;
with CuBit.Handles.Startup;
with ACPI_Firmware_Data_Lease_Check;
with ACPI_Native_Firmware_Backend;
with ACPI_Firmware_Data_Activation;
procedure ACPI_Backend_Main is
   use Interfaces;
   package B renames ACPI_Backend_Binding_Types;
   package Tables renames ACPI_Backend_FADT_Native;
   package Memory_Classes renames CuBit.ACPI_Memory_Classes;
   package Firmware renames ACPI_Backend_Firmware_Native;
   -- Exclusive device area for this backend process, per the kernel mapping
   -- contract. It is outside owned-memory and received-grant apertures.
   -- No other backend component may allocate/map here; backing lasts to exit.
   Firmware_Arena_First : constant Unsigned_64 := 16#6000_0000_0000#;
   Firmware_Arena_Bytes : constant Unsigned_64 := 2 ** 36;
   Firmware_Arena_Limit : constant Unsigned_64 := Firmware_Arena_First + Firmware_Arena_Bytes;
   Firmware_Physical_Limit : constant Unsigned_64 := Memory_Classes.Page_Limit * Memory_Classes.Page_Bytes;
   Mapping_Page_Limit : constant Positive := Positive (Firmware_Arena_Bytes / Memory_Classes.Page_Bytes);
   Kernel_Map_Page_Limit : constant Positive := 1024;
   Combined_Window_Limit : constant Positive :=
     CuBit.ACPI_Windows.Maximum_IO_Windows + CuBit.ACPI_Windows.Maximum_Memory_Windows;
   pragma Compile_Time_Error
     (Firmware_Arena_First mod Memory_Classes.Page_Bytes /= 0
       or else Firmware_Arena_Limit > B.User_Virtual_Limit, "invalid firmware device arena");
   package Mapping_Owner is new ACPI_Backend_Mapping_Startup
     (CuBit.Handles.Device_Memory, CuBit.Handles.No_Device_Memory,
      ACPI_Backend_Kernel_Handles.Map_Memory, ACPI_Backend_Kernel_Handles.Read_Clock,
      CuBit.ACPI_Windows.Maximum_Memory_Windows, Mapping_Page_Limit, Kernel_Map_Page_Limit,
      Firmware_Physical_Limit, Firmware_Arena_Limit);
   use type Mapping_Owner.Outcome, Firmware.Policy.Outcome;
   Mapping : Mapping_Owner.Context;
   Last_Mapping : Mapping_Owner.Report;
   procedure Map_Window
     (Held : CuBit.Handles.Device_Memory; Physical, Virtual, Pages : Unsigned_64;
      Read_Only : Boolean; Mapped : out Boolean) is
   begin
      Mapping_Owner.Map_Window (Mapping, Held, Physical, Virtual, Pages, Read_Only, Last_Mapping);
      Mapped := Last_Mapping.Status = Mapping_Owner.Window_Mapped;
   end Map_Window;
   Named_Register_Limit : constant Positive := 1;
   Scope_Limit : constant Positive := 1;
   package Native is new ACPI_Backend_Native_Catalog
     (Combined_Window_Limit, Named_Register_Limit, Scope_Limit,
      Firmware_Arena_First, Firmware_Arena_Limit, Mapping_Page_Limit, Map_Window);
   package Catalog renames Native.Owner;
   package Bindings is new ACPI_Backend_Startup_Native (Catalog);
   Hardware : Catalog.Context;
   Bound_Run : Bindings.Owner.Context;
   procedure Event
     (Publisher : CuBit.Messages.Process_ID; Item : CuBit.Messages.Message; Stop : out Boolean) is
      Handled : Boolean;
   begin ACPI_Native_Events.Handle (Publisher, Item, Stop, Handled); end Event;
   procedure Quarantine (Item : Catalog.Statistics) is
   begin
      ACPI_Native_Log.Warning ("acpi-backend: stage hardware fail QUARANTINED pending=" & Natural'Image (Item.Pending));
   end Quarantine;
   Startup_Timeout_Microseconds : constant B.Tick := 10_000_000;
   Inventory_Description_Budget : constant Positive := 4096;
   Register_Service_Role : constant Unsigned_64 := 35; -- agreed acpi-registers catalog role
   use type B.Tick, Tables.Source.Outcome, Bindings.Owner.Outcome;
   Now, Deadline : B.Tick;
   Available : Boolean;
   Facts : ACPI_FADT.Description;
   Read_Result : Tables.Report;
   Bound : Bindings.Owner.Report;
   Registration_Result : Unsigned_64;
   PCI_Endpoint : constant CuBit.Handles.Startup.Optional_Endpoint := Startup.Pci_Windows;
   PCI_First, PCI_Limit : Unsigned_64 := 0;
   PCI_Notes : Natural := 0;
   Maximum_PCI_Notes : constant Positive := 16;
   Mapping_Status : Mapping_Owner.Outcome;
   Mapping_Sealed : Boolean;
   Last_Startup_Time : B.Tick;
   function Startup_Time_Remains return Boolean is
   begin
      ACPI_Backend_Kernel_Handles.Read_Clock (Now, Available);
      if not Available or else Now < Last_Startup_Time or else Now >= Deadline then return False; end if;
      Last_Startup_Time := Now; return True;
   end Startup_Time_Remains;
begin
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   ACPI_Native_Log.Info ("acpi-backend: stage service-start ok firmware-windows");
   ACPI_Backend_Kernel_Handles.Read_Clock (Now, Available);
   if not Available or else Now >= B.No_Finite_Deadline - Startup_Timeout_Microseconds then
      ACPI_Native_Log.Warning ("acpi-backend: stage startup fail CLOCK_UNAVAILABLE");
      ACPI_Native_Log.Warning ("acpi-backend: bring-up reached service-start (1/5)"); return;
   end if;
   Deadline := Now + Startup_Timeout_Microseconds;
   Last_Startup_Time := Now;
   declare
      Grant : aliased constant CuBit.ACPI_Windows.Grant := Startup.Hardware;
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
      NVS_State : NVS.Context;
      NVS_Status : NVS.Outcome;
      package NVS_Endpoint is new ACPI_Backend_NVS_Endpoint (NVS);
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
      package Lock_Dispatch is new ACPI_Global_Lock_Dispatch
        (CuBit.Messages.Process_ID, CuBit.Messages.No_Process, Lock_Scope, 0, Lock_Owner, Lock_Sessions, Lock_Requests, Lock_Clock);
      package Lock_Maintenance is new ACPI_Global_Lock_Maintenance
        (CuBit.Messages.Process_ID, CuBit.Messages.No_Process, Lock_Scope, 0, Lock_Owner, Lock_Sessions, Lock_Requests, Lock_Clock);
      Lock_State : Lock_Requests.Context;
      Lock_Schedule : Lock_Maintenance.Context;
      Lock_Status : Lock_Owner.Outcome;
      Last_Lock : Lock_Maintenance.Report;
      Stop_Requested, Retention_Noted, Loop_Failed : Boolean := False;
      package Data_Lease is new ACPI_Firmware_Data_Lease_Check
        (CuBit.Messages.Process_ID,CuBit.Messages.No_Process,CuBit.Messages.From_Word,
         Lock_Scope,0,Lock_Owner,Lock_Sessions,Lock_Requests,Lock_State,
         Lock_Scope (ACPI_Register_Roles.Frontend_Tag));
      function Data_Caller (Caller : Unsigned_64) return Boolean is
        (CuBit.Messages.Is_Process (CuBit.Messages.From_Word (Caller)));
      function Data_Admission return Boolean is
        (not Stop_Requested and then not Loop_Failed and then not Lock_Requests.Closing (Lock_State)
         and then not Catalog.Observe (Hardware).Quarantined);
      package Firmware_Data is new ACPI_Native_Firmware_Backend
        (CuBit.Handles.IPC.Close,Data_Lease.Check,Data_Caller,Data_Admission,Kernel_Map_Page_Limit,
         Allow_Writes => False);

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
         Self : constant CuBit.Messages.Process_ID := CuBit.Messages.Own_Process;
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
                        ACPI_Native_Log.Warning ("acpi-backend: stage global-lock fail AMBIGUOUS_FACS"); return;
                     end if;
                     Window := ID;
                  end if;
               end;
            end loop;
         end if;
         FACS.Prepare (FACS_State, Hardware, Facts, Window, FACS_Result);
         if FACS_Result.Status /= FACS.Ready then
            ACPI_Native_Log.Info ("acpi-backend: stage global-lock skip " & FACS_Result.Status'Image); return;
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
            ACPI_Native_Log.Warning ("acpi-backend: stage global-lock fail SIGNAL_" & Signal_Result.Status'Image); return;
         end if;
         if not CuBit.Messages.Is_Process (Self) then
            ACPI_Native_Log.Warning ("acpi-backend: stage global-lock fail SELF_UNAVAILABLE"); return;
         end if;
         -- The full kernel process incarnation supplies the nonzero domain;
         -- no narrowed PID, time-derived identity or caller payload is used.
         Lock_Requests.Configure (Lock_State, Lock_Scope (ACPI_Register_Roles.Frontend_Tag), CuBit.Messages.To_Word (Self), Lock_Status);
         if Lock_Status = Lock_Owner.Configured then
            ACPI_Native_Log.Info ("acpi-backend: stage global-lock ok finite-lease access=CONTROL_PROTOCOL");
         else ACPI_Native_Log.Warning ("acpi-backend: stage global-lock fail " & Lock_Status'Image); end if;
      end Prepare_Lock;
      procedure Lock_Event
        (Publisher : CuBit.Messages.Process_ID; Item : CuBit.Messages.Message; Stop : out Boolean) is
      begin
         Event (Publisher, Item, Stop);
         if Stop then
            Stop_Requested := True;
            Lock_Requests.Begin_Stop (Lock_State, Lock_Status);
            Stop := Lock_Requests.May_Exit (Lock_State);
         end if;
      end Lock_Event;
      function Wire_Outcome (Value : Lock_Owner.Outcome) return ACPI_Global_Lock_Protocol.Lock_Outcome is
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
      procedure Maintain_Lock (Wake : in out Unsigned_64; Proceed : in out Boolean) is
      begin
         ACPI_Native_Lock_Notes.Begin_Maintenance;
         Lock_Maintenance.Run (Lock_Schedule, Lock_State, Last_Lock);
         ACPI_Native_Lock_Notes.Finish_Maintenance (Wire_Outcome (Last_Lock.Cleanup));
         -- Owner's known post-release phase, not session quiescence, controls
         -- logging. No logging while Held or any uncertain physical phase.
         declare Engine : constant Lock_Owner.Statistics := Lock_Requests.Observe (Lock_State).Engine; begin
            ACPI_Native_Lock_Notes.Flush
              (not Engine.Working and then Engine.Stage in Lock_Owner.Fresh | Lock_Owner.Ready | Lock_Owner.Signal_Pending);
         end;
         Wake := Last_Lock.Deadline;
         if Lock_Requests.Closing (Lock_State) and then Lock_Requests.May_Exit (Lock_State) then Proceed := False; end if;
      end Maintain_Lock;
      package Locked_NVS is new ACPI_Backend_Locked_NVS
        (NVS, NVS_State, CuBit.Messages.Process_ID, CuBit.Messages.No_Process,
         Lock_Scope, 0, Lock_Owner, Lock_Sessions, Lock_Requests);
      package Locked_Data renames Locked_NVS.Service;
      Locked_Data_State : Locked_Data.Context;
      procedure Additional_Request
        (From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message; Response : in out CuBit.Messages.Message;
         Handled : in out Boolean) is
         package P renames ACPI_Global_Lock_Protocol;
         Input : constant P.Packet :=
           (Request.tag.label, Request.tag.length, CuBit.Messages.Flag_Byte (Request.tag),
            Request.tag.reserved, ACPI_Region_Protocol.Words (Request.words));
         Output : P.Packet;
         Decoded : constant P.Request := P.Decode_Request (Input);
      begin
         ACPI_Native_Lock_Notes.Handle_Dump (From,Request,Response,Handled);
         if Handled then return; end if;
         if Decoded.Valid then
            ACPI_Native_Lock_Notes.Begin_Request (From,Request.authorityTag,Decoded.Kind);
         end if;
         Lock_Dispatch.Handle (Lock_State, Lock_Scope (Request.authorityTag), From, Input, Output, Handled);
         if Decoded.Valid then
            ACPI_Native_Lock_Notes.Finish_Request (P.Decode_Reply (Output,Decoded.Kind));
         end if;
         if Handled then
            Response := CuBit.Messages.NULL_MESSAGE;
            Response.tag := (label => Output.Label, length => Output.Length,
              protocol => CuBit.Messages.Protocol_Flags (Output.Flags), ipc => <>, reserved => Output.Reserved);
            Response.words := CuBit.Messages.MessageWords (Output.Data); return;
         end if;
         -- Same serialized owner and retained record catalog as ordinary NVS.
         -- Queries may recover historical results while quiescing; every new
         -- datum effect performs its own live full-caller lease check.
         Locked_Data.Dispatch (Locked_Data_State, Lock_State, Lock_Scope (Request.authorityTag),
           From, Input, Output, Handled);
         if Handled then
            Response := CuBit.Messages.NULL_MESSAGE;
            Response.tag := (label => Output.Label, length => Output.Length,
              protocol => CuBit.Messages.Protocol_Flags (Output.Flags), ipc => <>, reserved => Output.Reserved);
            Response.words := CuBit.Messages.MessageWords (Output.Data); return;
         end if;
         -- Data history/descriptions and scope Close must remain reachable
         -- before the generic quiescing fallback consumes unknown labels.
         Firmware_Data.Additional_Request (From,Request,Response,Handled);
         if Handled then return;
         elsif Lock_Requests.Closing (Lock_State) then
            -- Quiescing consumes every non-lock request before legacy/data
            -- dispatch. Existing lock queries/release remain correlated.
            Handled := True; Response := CuBit.Messages.NULL_MESSAGE;
            if Request.tag.label in ACPI_NVS_Protocol.Header_Operation |
              ACPI_NVS_Protocol.Describe_Operation | ACPI_NVS_Protocol.Read_Operation
            then
               Output := ACPI_NVS_Protocol.Error_Reply
                 ((case Request.tag.label is
                     when ACPI_NVS_Protocol.Header_Operation => ACPI_NVS_Protocol.Header,
                     when ACPI_NVS_Protocol.Describe_Operation => ACPI_NVS_Protocol.Describe,
                     when others => ACPI_NVS_Protocol.Read), ACPI_NVS_Protocol.Unavailable);
               Response.tag.label := Output.Label; Response.tag.length := Output.Length;
               Response.words := CuBit.Messages.MessageWords (Output.Data);
            elsif ACPI_PCI_Protocol.Owned_Label (Request.tag.label) then
               Output := ACPI_PCI_Protocol.Error_Reply
                 ((if Request.tag.label = ACPI_PCI_Protocol.Header_Operation then ACPI_PCI_Protocol.Header
                   elsif Request.tag.label = ACPI_PCI_Protocol.Describe_Operation then ACPI_PCI_Protocol.Describe
                   else ACPI_PCI_Protocol.Read), ACPI_PCI_Protocol.Unavailable);
               Response.tag.label := Output.Label; Response.tag.length := Output.Length;
               Response.words := CuBit.Messages.MessageWords (Output.Data);
            else
               Response.tag.label := ACPI_Region_Protocol.Operation_Reply; Response.tag.length := 4;
            end if;
            return;
         end if;
         Handled := Request.tag.label in ACPI_NVS_Protocol.Header_Operation |
           ACPI_NVS_Protocol.Describe_Operation | ACPI_NVS_Protocol.Read_Operation;
         if Handled then NVS_Endpoint.Dispatch (NVS_State, Request, Response); return; end if;
         Handled := ACPI_PCI_Protocol.Owned_Label (Request.tag.label);
         if Handled then
            declare
               Before : constant Natural :=
                 (if Input.Label = ACPI_PCI_Protocol.Header_Operation
                  then ACPI_Native_PCI_Windows.Observe.Rows else 0);
               Parsed : constant ACPI_PCI_Protocol.Request := ACPI_PCI_Protocol.Decode_Request (Input);
               Engine : constant Lock_Owner.Statistics := Lock_Requests.Observe (Lock_State).Engine;
            begin
               ACPI_Native_PCI_Windows.Dispatch (Request.authorityTag, Input, Output,
                 Allow_Setup => not Engine.Working and then Engine.Stage in Lock_Owner.Fresh | Lock_Owner.Ready);
               if Parsed.Valid and then Input.Label = ACPI_PCI_Protocol.Header_Operation
                 and then ACPI_Native_PCI_Windows.Observe.Rows > Before
                 and then PCI_Notes < Maximum_PCI_Notes
               then
                  PCI_Notes := PCI_Notes + 1;
                  declare
                     Reply : constant ACPI_PCI_Protocol.Reply :=
                       ACPI_PCI_Protocol.Decode_Reply (Output, ACPI_PCI_Protocol.Header);
                     Note : constant String := "acpi-backend: stage pci-bus " &
                       (if Reply.Valid and then Reply.Status = ACPI_PCI_Protocol.Ready then "ok " else "fail ") &
                       Reply.Status'Image & " seg=" & Parsed.Key.Segment'Image &
                       " bus=" & Parsed.Key.Bus'Image;
                  begin
                     if Reply.Valid and then Reply.Status = ACPI_PCI_Protocol.Ready
                     then ACPI_Native_Log.Info (Note); else ACPI_Native_Log.Warning (Note); end if;
                  end;
               end if;
            end;
            Response := CuBit.Messages.NULL_MESSAGE;
            Response.tag := (label => Output.Label, length => Output.Length,
              protocol => CuBit.Messages.Protocol_Flags (Output.Flags), ipc => <>, reserved => Output.Reserved);
            Response.words := CuBit.Messages.MessageWords (Output.Data);
         end if;
      end Additional_Request;
      package Server is new ACPI_Backend_Server
        (Catalog, ACPI_Backend_Loop_Native.Poll_Completion, CuBit.Messages.Poll_Event,
         CuBit.Messages.Poll_Service_Request, CuBit.Messages.reply,
         CuBit.Messages.Wait_For_Activity_Until, Lock_Event, Quarantine, Additional_Request, Maintain_Lock);
      use type Server.Reason, NVS.Outcome;
      Loop_Result : Server.Report;
   begin
      if Grant.IO_Used = 0 and then Grant.Memory_Used = 0 then
         ACPI_Native_Log.Warning ("acpi-backend: stage hardware fail EMPTY_GRANT");
         ACPI_Native_Log.Warning ("acpi-backend: bring-up reached service-start (1/5)"); return;
      end if;
      -- Check all copied memory claims against fresh kernel facts before any
      -- mapping. The binding owner independently rechecks the same handle.
      for I in 1 .. Grant.Memory_Used loop
         declare
            Actual : constant Firmware.Policy.Report := Firmware.Inspect_Claim
              (Grant.Memory (I).Memory, Grant.Memory (I).First_Page, Grant.Memory (I).Pages,
               Grant.Memory (I).Kind, Grant.Memory (I).Cache);
         begin
            if Actual.Status /= Firmware.Policy.Admitted then
               ACPI_Native_Log.Warning ("acpi-backend: stage firmware-windows fail " & Actual.Status'Image &
                 " window=" & Natural'Image (I));
               ACPI_Native_Log.Warning ("acpi-backend: bring-up reached service-start (1/5)"); return;
            end if;
            Validated_Memory (I) := Actual.Memory;
         end;
      end loop;
      ACPI_Native_Log.Info ("acpi-backend: stage firmware-windows ok memory=" &
        Natural'Image (Grant.Memory_Used) & " ports=" & Natural'Image (Grant.IO_Used));
      Mapping_Owner.Configure (Mapping, Deadline, Mapping_Status);
      if Mapping_Status /= Mapping_Owner.Configured then
         ACPI_Native_Log.Warning ("acpi-backend: stage mapping fail " & Mapping_Status'Image);
         ACPI_Native_Log.Warning ("acpi-backend: bring-up reached service-start (1/5)"); return;
      end if;
      Tables.Read (Startup.Tables, Tables.Source.R.Tick (Deadline), Inventory_Description_Budget, Facts, Read_Result);
      if Read_Result.Source_Result.Status /= Tables.Source.FADT_Ready then
         ACPI_Native_Log.Warning ("acpi-backend: stage fadt-source fail " & Read_Result.Source_Result.Status'Image &
           " copy=" & Read_Result.Source_Result.Copy.Status'Image & " kernel=" & Read_Result.Last_Kernel_Status'Image);
         ACPI_Native_Log.Warning ("acpi-backend: bring-up reached service-start (1/5)"); return;
      end if;
      ACPI_Native_Log.Info ("acpi-backend: stage fadt-source ok rev=" & Unsigned_8'Image (Facts.Revision) &
        " bytes=" & Unsigned_64'Image (Read_Result.Source_Result.Required_Bytes));
      -- Control pages may share their trailing bytes with copied SDTs. They
      -- supply FACS access only, never an ordinary named timer/field record.
      declare
         package Registers renames ACPI_FADT.Registers;
         Timer : constant Registers.Block_Result := Registers.Describe
           (Facts, ACPI_FADT.PM_Timer, Unsigned_64'Last);
         use type Registers.Status, Registers.Space_Kind;
      begin
         if Timer.Code = Registers.Described and then Timer.Space = Registers.System_Memory then
            for I in 1 .. Grant.Memory_Used loop
               if Firmware.Policy.Generic_Field_Conflict
                 (Validated_Memory (I), Timer.Base, Unsigned_64 (Timer.Length))
               then
                  ACPI_Native_Log.Warning ("acpi-backend: stage hardware fail CONTROL_TIMER_REFUSED");
                  ACPI_Native_Log.Warning ("acpi-backend: bring-up reached fadt-source (2/5)"); return;
               end if;
            end loop;
         end if;
      end;
      Bindings.Initialize (Bound_Run, Hardware, Grant, Facts, Deadline, Bound);
      if Bound.Status /= Bindings.Owner.Ready then
         ACPI_Native_Log.Warning ("acpi-backend: stage hardware fail " & Bound.Status'Image &
           " binding=" & Bound.Binding.Status'Image & " windows=" & Natural'Image (Bound.Bound_Windows) &
           " map=" & Last_Mapping.Status'Image & " progress=" & Last_Mapping.Mapping'Image);
         ACPI_Native_Log.Warning ("acpi-backend: bring-up reached fadt-source (2/5)"); return;
      end if;
      Mapping_Owner.Seal (Mapping, Mapping_Sealed);
      if not Mapping_Sealed then
         ACPI_Native_Log.Warning ("acpi-backend: stage mapping fail INCOMPLETE");
         ACPI_Native_Log.Warning ("acpi-backend: bring-up reached fadt-source (2/5)"); return;
      end if;
      ACPI_Native_Log.Info ("acpi-backend: stage hardware ok windows=" & Natural'Image (Bound.Bound_Windows));
      NVS.Configure (NVS_State, Unsigned_64 (ACPI_Register_Roles.Frontend_Tag), NVS_Status);
      if NVS_Status /= NVS.Configured then
         ACPI_Native_Log.Warning ("acpi-backend: stage nvs-catalog fail " & NVS_Status'Image);
         ACPI_Native_Log.Warning ("acpi-backend: bring-up reached hardware (3/5)"); return;
      end if;
      for I in 1 .. Grant.Memory_Used loop
         if not Startup_Time_Remains then
            ACPI_Native_Log.Warning ("acpi-backend: stage nvs-catalog fail CLOCK_OR_DEADLINE");
            ACPI_Native_Log.Warning ("acpi-backend: bring-up reached hardware (3/5)"); return;
         end if;
         if Firmware.Policy.Allows_Named_Reads (Validated_Memory (I)) then
            NVS.Add (NVS_State, NVS.Record_ID (I), Catalog.W.Window_ID (Grant.IO_Used + I), NVS_Status);
            if NVS_Status /= NVS.Added then
               ACPI_Native_Log.Warning ("acpi-backend: stage nvs-catalog fail " & NVS_Status'Image &
                 " window=" & Natural'Image (I));
               ACPI_Native_Log.Warning ("acpi-backend: bring-up reached hardware (3/5)"); return;
            end if;
         end if;
      end loop;
      NVS.Seal (NVS_State, NVS_Status);
      if NVS_Status /= NVS.Sealed then
         ACPI_Native_Log.Warning ("acpi-backend: stage nvs-catalog fail " & NVS_Status'Image);
         ACPI_Native_Log.Warning ("acpi-backend: bring-up reached hardware (3/5)"); return;
      end if;
      if not Startup_Time_Remains then
         ACPI_Native_Log.Warning ("acpi-backend: stage nvs-catalog fail CLOCK_OR_DEADLINE");
         ACPI_Native_Log.Warning ("acpi-backend: bring-up reached hardware (3/5)"); return;
      end if;
      ACPI_Native_Log.Info ("acpi-backend: stage nvs-catalog ok records=" &
        Natural'Image (NVS.Observe (NVS_State).Records) & " access=READ_ONLY");
      if not Startup_Time_Remains then
         ACPI_Native_Log.Warning ("acpi-backend: stage global-lock fail CLOCK_OR_DEADLINE"); return;
      end if;
      Prepare_Lock;
      if not Lock_Requests.Observe (Lock_State).Configured then
         ACPI_Native_Log.Info ("acpi-backend: stage scoped-nvs skip LOCK_UNAVAILABLE");
      elsif NVS.Observe (NVS_State).Records = 0 then
         ACPI_Native_Log.Info ("acpi-backend: stage scoped-nvs skip NO_NVS_RECORDS");
      else
         ACPI_Native_Log.Info ("acpi-backend: stage scoped-nvs ok READY_READ_ONLY");
      end if;
      if not Startup_Time_Remains then
         ACPI_Native_Log.Warning ("acpi-backend: stage global-lock fail CLOCK_OR_DEADLINE"); return;
      end if;
      if PCI_Endpoint.Present or else ACPI_Firmware_Data_Activation.Enabled then
         declare
            Data_First, Data_Limit : Unsigned_64;
            Accepted : Boolean;
            Partition : ACPI_Backend_Device_Arenas.Selection;
         begin
            -- One-shot handoff from the sealed catalog. Only this trusted
            -- startup code partitions its unused device VA tail.
            Catalog.Claim_Unused_Device_Arena (Hardware, Data_First, Data_Limit, Accepted);
            if not Accepted then
               ACPI_Native_Log.Warning ("acpi-backend: stage device-arenas fail UNAVAILABLE");
               if ACPI_Firmware_Data_Activation.Enabled then return; end if;
            else
               Partition := ACPI_Backend_Device_Arenas.Plan
                 ((Data_First, Data_Limit), PCI_Endpoint.Present, ACPI_Firmware_Data_Activation.Enabled);
               if Partition.Status /= ACPI_Backend_Device_Arenas.Ready then
                  ACPI_Native_Log.Warning ("acpi-backend: stage device-arenas fail INVALID_RANGE");
                  if ACPI_Firmware_Data_Activation.Enabled then return; end if;
               else
                  if Partition.Has_PCI then
                     PCI_First := Partition.PCI.First; PCI_Limit := Partition.PCI.Limit;
                  end if;
                  if Partition.Has_Data then
                     Data_First := Partition.Data.First; Data_Limit := Partition.Data.Limit;
                  end if;
               end if;
               if ACPI_Firmware_Data_Activation.Enabled then
                  declare
                     Endpoint : constant CuBit.Handles.Startup.Optional_Endpoint := Startup.Hardware_Data;
                  begin
                     if not Endpoint.Present then
                        ACPI_Native_Log.Warning ("acpi-backend: stage firmware-data fail NOT_APPROVED"); return;
                     end if;
                     Firmware_Data.Initialize (Endpoint.Value, Data_First, Data_Limit, Accepted);
                     if not Accepted then
                        ACPI_Native_Log.Warning ("acpi-backend: stage firmware-data fail " &
                          Firmware_Data.Observe.Initialization'Image); return;
                     end if;
                     ACPI_Native_Log.Info ("acpi-backend: stage firmware-data ok READY_READ_ONLY");
                  end;
               else ACPI_Native_Log.Info ("acpi-backend: stage firmware-data skip DISABLED"); end if;
            end if;
         end;
      else ACPI_Native_Log.Info ("acpi-backend: stage device-arenas skip NOT_REQUIRED"); end if;
      ACPI_Native_Log.Info ("acpi-backend: stage catalog ok named=" & Natural'Image (Catalog.Observe (Hardware).Registers) &
        " timer=" & Bound.Timer_Status'Image & " block=" & Bound.Timer_Block'Image);
      Registration_Result := CuBit.Messages.registerDriver (Register_Service_Role, Startup.Registration);
      if Registration_Result = Unsigned_64'Last then
         ACPI_Native_Log.Warning ("acpi-backend: stage registration fail REFUSED");
         ACPI_Native_Log.Warning ("acpi-backend: bring-up reached catalog (4/5)"); return;
      end if;
      if PCI_Endpoint.Present then
         declare
            D : constant CuBit.Handles.IPC.Description := CuBit.Handles.IPC.Describe (PCI_Endpoint.Value);
            Accepted : Boolean := False;
         begin
            if D.Current then
               ACPI_Native_PCI_Windows.Initialize
                 (PCI_Endpoint.Value, CuBit.Process_IDs.From_Word (D.Object),
                  Unsigned_64 (ACPI_Register_Roles.Frontend_Tag), Registration_Result,
                  PCI_First, PCI_Limit, Accepted);
            end if;
            if Accepted then ACPI_Native_Log.Info ("acpi-backend: stage pci-windows ok READ_ONLY_ON_REQUEST");
            else ACPI_Native_Log.Warning ("acpi-backend: stage pci-windows fail ENDPOINT_OR_ARENA"); end if;
         end;
      else ACPI_Native_Log.Info ("acpi-backend: stage pci-windows skip NOT_APPROVED"); end if;
      ACPI_Native_Log.Info ("acpi-backend: stage registration ok timer-nvs-and-lock");
      ACPI_Native_Log.Info ("acpi-backend: bring-up complete firmware-windows");
      loop
         Server.Serve (Hardware, Loop_Result);
         if Loop_Result.Status not in Server.Stop_Requested | Server.Maintenance_Stopped then Loop_Failed := True; end if;
         exit when Lock_Requests.May_Exit (Lock_State);
         -- Every abnormal return closes admission before another receive.
         -- Keep retained FACS/PM1 mappings and contexts alive until release
         -- is known. Unknown effects never permit return from this scope.
         Lock_Requests.Begin_Stop (Lock_State, Lock_Status);
         Lock_Maintenance.Run (Lock_Schedule, Lock_State, Last_Lock);
         if not Retention_Noted then
            ACPI_Native_Log.Warning ("acpi-backend: stage serve fail RETAINED_LOCK reason=" &
              Loop_Result.Status'Image & " cleanup=" & Last_Lock.Cleanup'Image);
            Retention_Noted := True;
         end if;
         exit when Lock_Requests.May_Exit (Lock_State);
         declare
            Yielded : constant Unsigned_64 := CuBit.Messages.syscall (CuBit.Messages.SYSCALL_YIELD);
            pragma Unreferenced (Yielded);
         begin null; end;
      end loop;
      if Stop_Requested and then not Loop_Failed and then Loop_Result.Status in Server.Stop_Requested | Server.Maintenance_Stopped then
         ACPI_Native_Log.Info ("acpi-backend: stage serve ok STOP_REQUESTED");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
      else ACPI_Native_Log.Warning ("acpi-backend: stage serve fail " & Loop_Result.Status'Image); end if;
   end;
end ACPI_Backend_Main;
