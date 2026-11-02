pragma Ada_2022;
with Ada.Text_IO;
with GNAT.Source_Info;
with Interfaces;
with AML_Execute;
with AML_Delays;
with AML_Decode;
with ACPI_Service_Core;
with ACPI_Device_Initialization;
with ACPI_Device_Policy;
with ACPI_Field_Startup_Owner;
with ACPI_Startup_Evaluation;
with ACPI_Unprovisioned_Regions;
with Observer_Fixture;
with Ada.Command_Line;
with ACPI_Backend_Catalog;
with ACPI_Backend_Inventory;
with ACPI_Backend_Binding_Types;
with ACPI_Inventory_Protocol;
with ACPI_NVS_Protocol;
with ACPI_Register_Roles;
with Composed_Fixture;
procedure Lifecycle_Tests is
   use Interfaces;
   package Proto renames ACPI_Inventory_Protocol;
   package Wire renames Proto.Wire;
   package T renames Proto.T;
   package B renames ACPI_Backend_Binding_Types;
   use type T.Access_Width, B.Outcome, Wire.Time_Instant, Wire.Packet;
   First_Port : constant Unsigned_16 := 16#600#;
   Window_Length : constant Unsigned_64 := 16;
   Admitted_Cells : constant Positive := 12;
   First_ID : constant Unsigned_64 := 16#8000_0000_0000_0001#;
   Scope : constant Unsigned_64 := Unsigned_64 (ACPI_Register_Roles.Frontend_Tag);
   Readonly_Scope : constant Unsigned_64 := 16#4143_5049_5244_4F4E#; -- ACPIRDON, model-only child
   Checks, Calls, Reads, Writes : Natural := 0;
   type Cells is array (Natural range 0 .. 15) of Unsigned_8;
   Memory : Cells := [others => 0];
   type Effect is record Writing : Boolean := False; Cell : Natural := 0; Value : Unsigned_64 := 0; end record;
   type Effects is array (Positive range 1 .. 128) of Effect;
   Trace_Count : Natural := 0;
   Incomplete_Selector : Boolean := False;
   procedure Assert (Value : Boolean; Location : String := GNAT.Source_Info.Source_Location) is
   begin Checks := Checks + 1; if not Value then raise Program_Error with Checks'Image & " at " & Location; end if; end Assert;
   procedure Inspect (Authority : Unsigned_64; Item : out B.Description) is
   begin
      Assert (Authority = 1);
      Item := (True, B.Port_Resource, Unsigned_64 (First_Port), Window_Length, True, True, True);
   end Inspect;
   procedure Map
     (Authority, Physical, Virtual, Pages : Unsigned_64; Read_Only : Boolean; Mapped : out Boolean) is
      pragma Unreferenced (Authority, Physical, Virtual, Pages, Read_Only);
   begin Mapped := False; Assert (False); end Map;
   procedure Clock (Now : out B.Tick; Available : out Boolean) is
   begin Now := 1; Available := True; end Clock;
   procedure Memory_IO
     (Virtual : Unsigned_64; Width : T.Access_Width; For_Write : Boolean;
      Input : Unsigned_64; Output : out Unsigned_64; Completed : out Boolean) is
      pragma Unreferenced (Virtual, Width, For_Write, Input);
   begin Output := 0; Completed := False; Assert (False); end Memory_IO;
   procedure Port_IO
     (Authority : Unsigned_64; Port : Unsigned_16; Width : T.Access_Width;
      For_Write : Boolean; Input : Unsigned_64; Output : out Unsigned_64; Completed : out Boolean) is
      Cell : Natural;
   begin
      Assert (Authority = 1 and then Width = T.Byte_Access and then
        Port >= First_Port and then Unsigned_64 (Port - First_Port) < Window_Length);
      Cell := Natural (Port - First_Port);
      Assert (Cell < Admitted_Cells and then Trace_Count < Effects'Last);
      if For_Write then
         Assert (Input <= Unsigned_64 (Unsigned_8'Last)); Memory (Cell) := Unsigned_8 (Input);
         Writes := Writes + 1; Output := 0;
      else
         Assert (Input = 0); Reads := Reads + 1; Output := Unsigned_64 (Memory (Cell));
      end if;
      Trace_Count := Trace_Count + 1;
      Completed := not (Incomplete_Selector and then For_Write and then Cell = 8);
   end Port_IO;
   package Backend is new ACPI_Backend_Catalog
     (Unsigned_64, 0, Unsigned_64, 0, Inspect, Inspect, Map, Clock, Memory_IO, Port_IO,
      1, 32, 2, 16#6000_0000#, 16#6000_2000#, 1);
   package Inventory is new ACPI_Backend_Inventory (Backend);
   package W renames Backend.W;
   package P renames Backend.P;
   Hardware : Backend.Context;
   type Channel is null record;
   Current_Time : Wire.Time_Instant := 10;
   Clock_Available : Boolean := True;
   procedure Read_Clock (Now : out Wire.Time_Instant; Available : out Boolean) is
   begin Now := Current_Time; Available := Clock_Available; end Read_Clock;
   procedure Exchange
     (Transport : in out Channel; Destination : Unsigned_64; Deadline : Wire.Time_Instant;
      Item : Wire.Packet; Reply : out Wire.Packet; Delivered : out Wire.Delivery) is
      pragma Unreferenced (Transport);
   begin
      Assert (Destination in 1 | 2 and then Deadline = 100); Calls := Calls + 1;
      if Item.Label = ACPI_NVS_Protocol.Header_Operation then
         -- This fixture admits scalar port registers and no NVS data records.
         -- The composed owner still requires a valid, explicit inventory reply.
         Assert (Item = ACPI_NVS_Protocol.Header_Request);
         Reply := ACPI_NVS_Protocol.Header_Result (1, 0);
      elsif Item.Label in Proto.Header_Operation | Proto.Part_Operation then
         Inventory.Dispatch (Hardware, (if Destination = 1 then Scope else Readonly_Scope), Item, Reply);
      else Backend.Dispatch (Hardware, (if Destination = 1 then Scope else Readonly_Scope), Item, Reply); end if;
      Delivered := Wire.Reply_Received;
   end Exchange;
   package Core is new ACPI_Service_Core
     (AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 64, Retained_Result_Capacity => 32);
   package Lifecycle is new ACPI_Field_Startup_Owner
     (Core, Channel, Unsigned_64, 0, Read_Clock, Exchange, 16, 16, 4);
   package Selected is new ACPI_Startup_Evaluation (Core, Lifecycle);
   package Empty_Regions is new ACPI_Unprovisioned_Regions (Core);
   use type Selected.Mode;
   use type Lifecycle.Phase, Core.Stage_Status, Core.Freeze_Status, Core.Values.Access_Status,
     Core.Namespace.Lookup_Status, Core.Values.Module_Outcome, AML_Execute.Execution_Status,
     Core.Values.Value_Kind, AML_Decode.Bytes;
   Fixture_Bytes : constant Positive := 4096;
   Service : aliased Core.State (1, Fixture_Bytes, Fixture_Bytes, 1);
   Bound : B.Report;
   Added : Backend.Add_Report;
   Accepted : Boolean;
   Staged : Core.Stage_Status;
   Frozen : Core.Freeze_Status;
   Execution : Core.Execution_Report;
   Status : Core.Values.Access_Status;
   Members : Core.Namespace.Initialization_Report;
   Found : Core.Namespace.Lookup_Result;
   Description : Core.Node_Descriptor;
   Value : Core.Values.Result;
   Input, Output : Core.Values.Value_Handle;
   Shape : Core.Values.Value_Description;
   Bytes : AML_Decode.Bytes (1 .. 4);
   Copied, Before, Saved_Writes : Natural;
   Scenario : constant String := Ada.Command_Line.Argument (1);
   Barrier_Calls : Natural := 0;
   procedure Regions
     (Current : aliased in out Core.State; Budget : Natural;
      Charged : out Natural; Outcome : out ACPI_Device_Policy.Region_Outcome) is
   begin
      Barrier_Calls := Barrier_Calls + 1;
      Selected.Prepare_Fields (Current, Accepted); Assert (Accepted);
      Empty_Regions.Initialize (Current, Budget, Charged, Outcome);
   end Regions;
   package Driver is new ACPI_Device_Initialization (Core, Regions, Selected.Evaluate_Named);
   Run : Driver.Session;
   use type Driver.Phase, Backend.Add_Code;
   procedure Find (Name : String) is
   begin
      Core.Find_Object (Service, Core.Namespace.Root, Name, Found, Status);
      Assert (Status = Core.Values.Available and then Found.Status = Core.Namespace.Found);
      Core.Describe_Node (Service, Found.Node, Description, Status); Assert (Status = Core.Values.Available);
   end Find;
   procedure Refused is
   begin
      Assert (not Accepted and then Calls = Before and then Reads = 0 and then Writes = 0);
      Lifecycle.Begin_Startup (1, 100, Accepted);
      Assert (not Accepted and then Calls = Before);
   end Refused;
begin
   Backend.Bind_Ports (Hardware,
     (1, P.IO_Space, Unsigned_64 (First_Port), Window_Length, True, True, True), 1, 100, Bound);
   Assert (Bound.Status = B.Bound);
   Backend.Authorize_Scope (Hardware, Scope, Accepted); Assert (Accepted);
   Backend.Authorize_Scope (Hardware, Readonly_Scope, Accepted); Assert (Accepted);
   for Cell in 0 .. Admitted_Cells - 1 loop
      Backend.Add (Hardware, 1,
        (Tag => Scope, Base => Unsigned_64 (First_Port) + Unsigned_64 (Cell), Length => 1,
         Space => P.IO_Space, Readable => True, Writable => True,
         Widths => [T.Byte_Access => True, others => False],
         Register_Authority => (First_ID + Unsigned_64 (Cell), True, True, True, 16#FF#),
         Register_Offset => 0, Register_Width => T.Byte_Access), W.Exact_Writes_Admitted, Added);
      Assert (Added.Status = Backend.Added);
   end loop;
   for Cell in 0 .. Admitted_Cells - 1 loop
      Backend.Add (Hardware, 1,
        (Tag => Readonly_Scope, Base => Unsigned_64 (First_Port) + Unsigned_64 (Cell), Length => 1,
         Space => P.IO_Space, Readable => True, Writable => False,
         Widths => [T.Byte_Access => True, others => False],
         Register_Authority => (First_ID + Unsigned_64 (Cell), True, False, False, 0),
         Register_Offset => 0, Register_Width => T.Byte_Access), W.Unestablished, Added);
      Assert (Added.Status = Backend.Added);
   end loop;
   Backend.Seal (Hardware, Accepted); Assert (Accepted);
   Assert (Lifecycle.Observe.Stage = Lifecycle.Fresh and then not Lifecycle.Observe.IO_Pending);
   Core.Stage_Table (Service, 1, Core.DSDT,
     (if Scenario = "observer" then Observer_Fixture.Data else Composed_Fixture.Data), Staged); Assert (Staged = Core.Staged);
   if Scenario /= "module-failure" then
      Core.Freeze_Catalog (Service, Frozen); Assert (Frozen = Core.Catalog_Frozen_OK);
   end if;
   Before := Calls;
   if Scenario = "observer" then
      Selected.Select_Observer (Accepted); Assert (Accepted and then Selected.Current = Selected.Observer and then Calls = Before);
      Selected.Select_Registers (Accepted); Assert (not Accepted and then Calls = Before);
      Selected.Execute_Module_Startup (Service, 100_000, Execution, Status); Assert (Status = Core.Values.Available and then Calls = Before);
      Core.Initialize_Members (Service, Members, Status); Assert (Status = Core.Values.Available);
      Driver.Begin_Initialization (Run, Service, 100_000, Accepted); Assert (Accepted);
      Driver.Complete_Initialization (Run, Service, Accepted);
      Assert (Accepted and then Driver.Observe (Run).Stage = Driver.Completed and then Calls = Before);
      Find ("CNST"); Selected.Evaluate_Named (Service, Description, 100_000, Value, Status);
      Assert (Status = Core.Values.Available and then Value.Status = AML_Execute.Returned and then Value.Number = 16#42# and then Calls = Before);
      Find ("GUID"); Selected.Evaluate_Named (Service, Description, 100_000, Value, Status);
      Assert (Status = Core.Values.Available and then Value.Status = AML_Execute.Object_Returned and then Core.Retained_Results (Service) = 1);
      Output := Value.Handle; Core.Release_Result (Service, Output, Status); Assert (Status = Core.Values.Available);
      Selected.Close_Startup; Selected.Evaluate_Named (Service, Description, 100_000, Value, Status);
      Assert (Status /= Core.Values.Available and then Value.Charged = 0 and then Calls = Before and then Core.Retained_Results (Service) = 0);
   elsif Scenario = "selector-refused" then
      Selected.Select_Registers (Accepted); Assert (not Accepted and then Selected.Current = Selected.Refused and then Calls = Before);
      Selected.Select_Observer (Accepted); Assert (not Accepted and then Calls = Before);
      Selected.Execute_Module_Startup (Service, 100_000, Execution, Status); Assert (Status /= Core.Values.Available and then Calls = Before);
   elsif Scenario = "selector-early" then
      Selected.Execute_Module_Startup (Service, 100_000, Execution, Status); Assert (Status /= Core.Values.Available and then Calls = Before);
      Selected.Select_Observer (Accepted); Assert (not Accepted and then Calls = Before);
   elsif Scenario = "selector-closed" then
      Selected.Close_Startup; Selected.Select_Observer (Accepted); Assert (not Accepted and then Calls = Before);
      Selected.Select_Registers (Accepted); Assert (not Accepted and then Calls = Before);
   elsif Scenario = "no-endpoint" then
      Lifecycle.Begin_Startup (0, 100, Accepted); Refused;
   elsif Scenario = "expired" then
      Current_Time := 100; Lifecycle.Begin_Startup (1, 100, Accepted); Refused;
   elsif Scenario = "infinite" then
      Lifecycle.Begin_Startup (1, Wire.No_Finite_Deadline, Accepted); Refused;
   elsif Scenario = "no-clock" then
      Clock_Available := False; Lifecycle.Begin_Startup (1, 100, Accepted);
      Clock_Available := True; Refused;
   elsif Scenario = "closed" then
      Lifecycle.Close_Startup; Lifecycle.Begin_Startup (1, 100, Accepted); Refused;
   elsif Scenario = "early-module" then
      Lifecycle.Execute_Module_Startup (Service, 100_000, Execution, Status);
      Assert (Status /= Core.Values.Available and then Calls = Before);
      Lifecycle.Begin_Startup (1, 100, Accepted); Refused;
   else
      Lifecycle.Begin_Startup (1, 100, Accepted);
      Assert (Accepted and then Lifecycle.Observe.Stage = Lifecycle.Ready
        and then Lifecycle.Observe.Registers = Admitted_Cells and then Reads = 0 and then Writes = 0);
      Before := Calls;
      Lifecycle.Begin_Startup (2, 100, Accepted); Assert (not Accepted and then Calls = Before);
      if Scenario in "deadline" | "reversal" | "clock-lost" then
         if Scenario = "deadline" then Current_Time := 100;
         elsif Scenario = "reversal" then Current_Time := 9;
         else Clock_Available := False; end if;
         Lifecycle.Execute_Module_Startup (Service, 100_000, Execution, Status);
         Assert (Status /= Core.Values.Available and then Lifecycle.Observe.Stage = Lifecycle.Closed
           and then Calls = Before and then Writes = 0 and then Reads = 0);
      elsif Scenario = "module-failure" then
         Lifecycle.Execute_Module_Startup (Service, 100_000, Execution, Status);
         Assert (Status /= Core.Values.Available and then Lifecycle.Observe.Stage = Lifecycle.Failed);
         Lifecycle.Execute_Module_Startup (Service, 100_000, Execution, Status);
         Assert (Status /= Core.Values.Available and then Calls = Before);
      elsif Scenario = "normal" or else Scenario = "raw" then
         Selected.Select_Registers (Accepted); Assert (Accepted and then Selected.Current = Selected.Register_Provider);
         Selected.Select_Observer (Accepted); Assert (not Accepted);
         Selected.Evaluate_Named (Service, Core.Values.No_Node, 100_000, Value, Status);
         Assert (Status /= Core.Values.Available and then Value.Charged = 0 and then Calls = Before);
         if Scenario = "raw" then
            Selected.Invoke_Retained (Service, Core.Namespace.Root,
              [others => 0], 0, 100_000, Value, Status);
            Assert (Status = Core.Values.Wrong_Phase and then Value.Charged = 0
              and then Calls = Before and then Core.Retained_Results (Service) = 0);
         end if;
         Selected.Execute_Module_Startup (Service, 100_000, Execution, Status);
         Assert (Status = Core.Values.Available and then Lifecycle.Observe.Module_Completed
           and then Execution.Modules.Outcome = Core.Values.Modules_Completed and then Calls = Before);
         Selected.Execute_Module_Startup (Service, 100_000, Execution, Status);
         Assert (Status /= Core.Values.Available and then Calls = Before);
         Core.Initialize_Members (Service, Members, Status); Assert (Status = Core.Values.Available);
         Driver.Claim_Startup (Run, Accepted); Assert (Accepted);
         Driver.Begin_Initialization (Run, Service, 100_000, Accepted);
         Assert (Accepted and then Driver.Observe (Run).Stage = Driver.Awaiting_Regions);
         Assert (Memory (0) = 16#42# and then Memory (1) = 16#43# and then Memory (2) = 0);
         Before := Calls;
         Driver.Complete_Initialization (Run, Service, Accepted);
         Assert (Accepted and then Driver.Observe (Run).Stage = Driver.Incomplete and then Barrier_Calls = 1);
         Assert (Calls = Before and then Memory (2) = 0);
         if Scenario = "raw" then
            Find ("RGN2"); Before := Calls;
            Selected.Invoke_Retained (Service, Found.Node,
              [0 => 1, 1 => 1, others => 0], 2, 0, Value, Status);
            Assert (Value.Status = AML_Execute.Budget_Exceeded and then Value.Charged = 0
              and then Calls = Before and then Core.Retained_Results (Service) = 0);
            Selected.Invoke_Retained (Service, Found.Node,
              [0 => 1, 1 => 1, others => 0], 2, 100_000, Value, Status);
            Assert (Status = Core.Values.Available and then Value.Status = AML_Execute.Returned
              and then Value.Number = 2 and then Memory (0) = 1
              and then Core.Retained_Results (Service) = 0);
            Selected.Invoke_Retained (Service, Found.Node,
              [0 => 0, 1 => 16#42#, others => 0], 2, 100_000, Value, Status);
            Assert (Status = Core.Values.Available and then Value.Status = AML_Execute.Returned
              and then Value.Number = 16#42# and then Memory (0) = 16#42#);
         end if;
         Find ("GUID"); Core.Observe_Named_Value (Service, Found.Node, Value, Status);
         Assert (Status = Core.Values.Available and then Value.Status = AML_Execute.Object_Returned);
         Input := Value.Handle; Assert (Core.Retained_Results (Service) = 1);
         Find ("MTH4"); Before := Calls;
         Selected.Invoke_With_Arguments (Service, Found.Node,
           [0 => (Core.Values.Retained_Argument, Input), 1 => (Core.Values.Immediate_Argument, 1),
            2 => (Core.Values.Immediate_Argument, 16#5A#), others => (Core.Values.Immediate_Argument, 0)],
           4, 0, Value, Status);
         Assert (Value.Status = AML_Execute.Budget_Exceeded and then Value.Charged = 0
           and then Calls = Before and then Core.Retained_Results (Service) = 1);
         Selected.Invoke_With_Arguments (Service, Found.Node,
           [0 => (Core.Values.Retained_Argument, Input), 1 => (Core.Values.Immediate_Argument, 1),
            2 => (Core.Values.Immediate_Argument, 16#5A#), others => (Core.Values.Immediate_Argument, 0)],
           4, 100_000, Value, Status);
         Assert (Status = Core.Values.Available and then Value.Status = AML_Execute.Object_Returned
           and then Memory (0) = 16#5A# and then Core.Retained_Results (Service) = 2);
         Output := Value.Handle;
         Core.Read_Result_Bytes (Service, Output, 0, Bytes, Copied, Status);
         Assert (Status = Core.Values.Available and then Copied = 4 and then Bytes = AML_Decode.Bytes'[16#10#,16#20#,16#30#,16#40#]);
         Core.Release_Result (Service, Output, Status); Assert (Status = Core.Values.Available);
         Core.Describe_Result (Service, Input, Shape, Status);
         Assert (Status = Core.Values.Available and then Shape.Kind = Core.Values.Buffer_Description
           and then Core.Retained_Results (Service) = 1);
         Core.Release_Result (Service, Input, Status); Assert (Status = Core.Values.Available and then Core.Retained_Results (Service) = 0);
         Find ("BRW0"); Incomplete_Selector := True; Saved_Writes := Writes;
         if Scenario = "raw" then
            Selected.Invoke_Retained (Service, Found.Node,
              [others => 16#EF#], 1, 100_000, Value, Status);
         else
         Selected.Invoke_With_Arguments (Service, Found.Node,
           [others => (Core.Values.Immediate_Argument, 16#EF#)], 1, 100_000, Value, Status);
         end if;
         Assert (Value.Status = AML_Execute.Field_Indeterminate and then Writes = Saved_Writes + 1
           and then Lifecycle.Observe.IO_Pending and then Lifecycle.Uncertain.Present);
         Before := Calls;
         Selected.Close_Startup;
         Assert (Lifecycle.Observe.IO_Pending and then Lifecycle.Uncertain.Present);
         if Scenario = "raw" then
            Selected.Invoke_Retained (Service, Found.Node,
              [others => 16#EF#], 1, 100_000, Value, Status);
         else
         Selected.Invoke_With_Arguments (Service, Found.Node,
           [others => (Core.Values.Immediate_Argument, 16#EF#)], 1, 100_000, Value, Status);
         end if;
         Assert (Status /= Core.Values.Available and then Value.Charged = 0 and then Calls = Before and then Writes = Saved_Writes + 1);
         Selected.Prepare_Fields (Service, Accepted); Assert (not Accepted and then Calls = Before);
      else raise Program_Error with "unknown scenario";
      end if;
   end if;
   Ada.Text_IO.Put_Line ("Lifecycle checks" & Checks'Image);
end Lifecycle_Tests;
