pragma Ada_2022;
with Ada.Text_IO;
with GNAT.Source_Info;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with AML_Objects;
with AML_Execute;
with AML_Decode;
with AML_Delays;
with ACPI_Service_Core;
with ACPI_Backend_Catalog;
with ACPI_Backend_Binding_Types;
with ACPI_Local_Register_Provider;
with ACPI_Field_Provider_View;
with ACPI_Field_Module_Startup;
with ACPI_Field_Evaluation;
with ACPI_Device_Initialization;
with ACPI_Device_Policy;
with ACPI_Unprovisioned_Regions;
with AML_Field_Access;
with ACPI_FADT.Transactions;
with Firmware_Tables;
with Composed_Fixture;
with Locked_Fixture;
with ACPI_Local_Field_Scope;
with ACPI_Global_Lock_Protocol;
procedure Local_Field_Core_Tests is
   package A renames AML_Field_Access;
   package B renames ACPI_Backend_Binding_Types;
   package T renames ACPI_FADT.Transactions;
   use type B.Outcome, T.Access_Width, Firmware_Tables.Byte;
   use type A.Completion, A.Observation, AML_Decode.Bytes;
   Scope : constant Unsigned_64 := 16#F123_4567_ABCD_0001#;
   Symbol : constant A.Endpoint_ID := 16#C123_4567_ABCD_0002#;
   First_ID : constant Unsigned_64 := 16#D123_4567_ABCD_0003#;
   Base : constant Unsigned_64 := 16#600#;
   type Cells is array (Natural range 0 .. 15) of Unsigned_8;
   Memory : Cells := [others => 0];
   Checks, Reads, Writes : Natural := 0;
   Locked : constant Boolean := Ada.Command_Line.Argument (2) = "locked";
   type Lease is limited record Prepared : Boolean := False; end record;
   Physical_Held : Boolean := False;
   Acquires, Releases : Natural := 0;
   Incomplete_Selector : Boolean := False;
   procedure Check (Good : Boolean; Location : String := GNAT.Source_Info.Source_Location) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with "local field Core" & Checks'Image & " at " & Location; end if; end Check;
   procedure Inspect (Authority : Natural; Item : out B.Description) is
   begin Check (Authority = 1); Item := (True,B.Port_Resource,Base,16,True,True,True); end Inspect;
   procedure Map (Authority : Natural; Physical, Virtual, Pages : Unsigned_64;
      Read_Only : Boolean; Mapped : out Boolean) is
      pragma Unreferenced (Authority, Physical, Virtual, Pages, Read_Only);
   begin Mapped := False; Check (False); end Map;
   procedure Clock (Now : out B.Tick; Available : out Boolean) is
   begin Now := 1; Available := True; end Clock;
   procedure Memory_IO (Address : Unsigned_64; Width : T.Access_Width;
      Writing : Boolean; Input : Unsigned_64; Output : out Unsigned_64; Completed : out Boolean) is
      pragma Unreferenced (Address, Width, Writing, Input);
   begin Output := 0; Completed := False; Check (False); end Memory_IO;
   procedure Port_IO (Authority : Natural; Port : Unsigned_16; Width : T.Access_Width;
      Writing : Boolean; Input : Unsigned_64; Output : out Unsigned_64; Completed : out Boolean) is
      Cell : constant Natural := Natural (Unsigned_64 (Port) - Base);
   begin
      Check (Physical_Held = Locked);
      Check (Authority = 1 and Cell < 12 and Width = T.Byte_Access);
      if Writing then Check (Input <= 16#FF#); Memory (Cell) := Unsigned_8 (Input); Writes := Writes + 1; Output := 0;
      else Check (Input = 0); Reads := Reads + 1; Output := Unsigned_64 (Memory (Cell)); end if;
      Completed := not (Writing and Cell = 8 and Incomplete_Selector);
   end Port_IO;
   package Catalog is new ACPI_Backend_Catalog
     (Natural,0,Natural,0,Inspect,Inspect,Map,Clock,Memory_IO,Port_IO,1,12,1,
      16#6000_0000#,16#6000_2000#,1);
   Hardware : Catalog.Context;
   package Provider is new ACPI_Local_Register_Provider (Catalog,Hardware,Scope,12);
   IO : Provider.Context;
   package LP renames ACPI_Global_Lock_Protocol;
   function Admission return Boolean is (not Physical_Held);
   function Due (Item : Lease) return Boolean is (Item.Prepared);
   procedure Window (Deadline : out Unsigned_64; Budget : out LP.Attempt_Budget; Available : out Boolean) is
   begin Deadline := 100; Budget := 3; Available := True; end Window;
   procedure Acquire (Item : in out Lease; Deadline : Unsigned_64; Budget : LP.Attempt_Budget;
      Result : out LP.Lock_Outcome) is
   begin
      Check (not Item.Prepared and not Physical_Held and Deadline = 100 and Budget = 3);
      Item.Prepared := True; Physical_Held := True; Acquires := Acquires + 1; Result := LP.Acquired;
   end Acquire;
   procedure Validate (Item : Lease; Result : out LP.Lock_Outcome) is
   begin Check (Item.Prepared and Physical_Held); Result := LP.Configured; end Validate;
   procedure Release (Item : in out Lease; Result : out LP.Lock_Outcome) is
   begin
      Check (Item.Prepared and Physical_Held); Item.Prepared := False; Physical_Held := False;
      Releases := Releases + 1; Result := LP.Released;
   end Release;
   package Guard is new ACPI_Local_Field_Scope (Lease,Admission,Due,Window,Acquire,Validate,Release,16);
   Transaction : Guard.Context;
   procedure Begin_Field (State : in out Provider.Context;
      Requires_Lock, For_Write : Boolean; Accepted : in out Boolean) is
   begin
      Accepted := False;
      if Provider.Is_Ready (State) then Guard.Begin_Field (Transaction,Requires_Lock,For_Write,Accepted); end if;
   end Begin_Field;
   procedure End_Field (State : in out Provider.Context; Accepted : in out Boolean) is
   begin
      if Provider.Pending (State) then Guard.Mark_Fault (Transaction); end if;
      Guard.End_Field (Transaction,Accepted);
      Accepted := Accepted and then not Provider.Pending (State);
   end End_Field;
   procedure Dispatch (State : in out Provider.Context; Item : A.Request; Reply : out A.Response) is
      Accepted : Boolean;
   begin
      Reply := (others => <>); Guard.Check_Datum (Transaction,Accepted);
      if not Accepted then return; end if;
      Provider.Dispatch (State,Item,Reply);
      if Provider.Pending (State) then Guard.Mark_Fault (Transaction); end if;
   end Dispatch;
   package View is new ACPI_Field_Provider_View
      (Provider.Context,Provider.Is_Ready,Provider.Identity,Provider.Pending,
       Provider.Register_Count,Provider.Export_Target,Provider.Select_Target,
       Provider.Describe,Provider.Authorize,Dispatch,Provider.Reconciled,
       Begin_Transaction => Begin_Field, End_Transaction => End_Field,Allow_Locked_Fields => True);
   package Core is new ACPI_Service_Core
     (AML_Delays.Unavailable_Provider,Namespace_Node_Capacity => 64,Retained_Result_Capacity => 32);
   package Modules is new ACPI_Field_Module_Startup (Core,View,16);
   package Evaluation is new ACPI_Field_Evaluation (Core,View,16);
   Run : Evaluation.Context;
   package Empty_Regions is new ACPI_Unprovisioned_Regions (Core);
   procedure Regions (Service : aliased in out Core.State; Budget : Natural;
      Charged : out Natural; Outcome : out ACPI_Device_Policy.Region_Outcome) is
      OK : Boolean;
   begin
      Evaluation.Prepare (Run,Service,IO,OK); Check (OK);
      Empty_Regions.Initialize (Service,Budget,Charged,Outcome);
   end Regions;
   procedure Evaluate (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result; Status : out Core.Values.Access_Status) is
   begin Evaluation.Evaluate_Named (Run,Service,IO,Item,Budget,Result,Status); end Evaluate;
   package Driver is new ACPI_Device_Initialization (Core,Regions,Evaluate);
   Driver_Run : Driver.Session;
   use type Catalog.Add_Code, Core.Stage_Status, Core.Freeze_Status, Core.Values.Access_Status,
      Core.Namespace.Lookup_Status, AML_Execute.Execution_Status, Driver.Phase;
   Service : aliased Core.State (1,4096,4096,1,
      Value_Object_Capacity => AML_Objects.Max_Objects,Value_Byte_Capacity => AML_Objects.Max_Bytes,
      Value_Element_Capacity => AML_Objects.Max_Elements);
   Data : Firmware_Tables.Bytes := (if Locked then Locked_Fixture.Data else Composed_Fixture.Data);
   Sum : Firmware_Tables.Byte := 0;
   Width : constant String := Ada.Command_Line.Argument (1);
   Bound : B.Report;
   Added : Catalog.Add_Report;
   OK : Boolean;
   Staged : Core.Stage_Status;
   Frozen : Core.Freeze_Status;
   Execution : Core.Execution_Report;
   Status : Core.Values.Access_Status;
   Members : Core.Namespace.Initialization_Report;
   Found : Core.Namespace.Lookup_Result;
   Description : Core.Node_Descriptor;
   Value : Core.Values.Result;
   Saved_Reads, Saved_Writes : Natural;
   Result_Handle : Core.Values.Value_Handle;
   Result_Bytes : AML_Decode.Bytes (1 .. 8);
   Copied : Natural;
   procedure Find (Name : String) is
   begin
      Core.Find_Object (Service,Core.Namespace.Root,Name,Found,Status);
      Check (Status = Core.Values.Available and Found.Status = Core.Namespace.Found);
      Core.Describe_Node (Service,Found.Node,Description,Status); Check (Status = Core.Values.Available);
   end Find;
   procedure Invoke (Name : String; Arg : AML_Decode.Integer_Value;
      Count : Core.Values.Argument_Count := 1) is
   begin
      Find (Name);
      Evaluation.Invoke_Retained (Run,Service,IO,Found.Node,[0 => Arg,others => 0],Count,100_000,Value,Status);
   end Invoke;
begin
   Check (Width in "32" | "64");
   Data (9) := (if Width = "32" then 1 else 2); Data (10) := 0;
   for Byte of Data loop Sum := Sum + Byte; end loop; Data (10) := 0 - Sum;
   Catalog.Bind_Ports (Hardware,(1,Catalog.P.IO_Space,Base,16,True,True,True),1,100,Bound); Check (Bound.Status = B.Bound);
   Catalog.Authorize_Scope (Hardware,Scope,OK); Check (OK);
   for Cell in 0 .. 11 loop
      Catalog.Add (Hardware,1,
        (Tag => Scope,Base => Base+Unsigned_64 (Cell),Length => 1,Space => Catalog.P.IO_Space,
         Readable => True,Writable => True,Widths => [T.Byte_Access => True,others => False],
         Register_Authority => (First_ID+Unsigned_64 (Cell),True,True,True,16#FF#),
         Register_Offset => 0,Register_Width => T.Byte_Access),Catalog.W.Exact_Writes_Admitted,Added);
      Check (Added.Status = Catalog.Added);
   end loop;
   Catalog.Seal (Hardware,OK); Check (OK);
   Provider.Initialize (IO,Symbol,OK); Check (OK);
   Core.Stage_Table (Service,1,Core.DSDT,Data,Staged); Check (Staged = Core.Staged);
   Core.Freeze_Catalog (Service,Frozen); Check (Frozen = Core.Catalog_Frozen_OK);
   Modules.Execute_Frozen (Service,IO,100_000,Execution,Status);
   Check (Status = Core.Values.Available and Reads = 0 and Writes = 0);
   Core.Initialize_Members (Service,Members,Status); Check (Status = Core.Values.Available);
   Driver.Claim_Startup (Driver_Run,OK); Check (OK);
   Driver.Begin_Initialization (Driver_Run,Service,100_000,OK);
   Check (OK and Driver.Observe (Driver_Run).Stage = Driver.Awaiting_Regions);
   Check (Memory (0) = 16#42# and Memory (1) = 16#43# and Memory (2) = 0);
   Driver.Complete_Initialization (Driver_Run,Service,OK);
   Check (OK and Driver.Observe (Driver_Run).Stage = Driver.Incomplete and Memory (2) = 0);
   -- The real region barrier remains closed; no fake controller/_REG readiness.
   Invoke ("WRW0",16#1234_5678#);
   Check (Status = Core.Values.Available);
   if Width = "32" then
      Check (Value.Status = AML_Execute.Object_Returned);
      Result_Handle := Value.Handle;
      Core.Read_Result_Bytes (Service,Result_Handle,0,Result_Bytes,Copied,Status);
      Check (Status = Core.Values.Available and then Copied = 8 and then
         Result_Bytes = AML_Decode.Bytes'[16#78#,16#56#,16#34#,16#12#,0,0,0,0]);
      Core.Release_Result (Service,Result_Handle,Status); Check (Status = Core.Values.Available);
   else
      Check (Value.Status = AML_Execute.Returned and then Value.Number = 16#1234_5678#);
   end if;
   Check (Memory (0) = 16#78# and Memory (1) = 16#56# and Memory (2) = 16#34# and Memory (3) = 16#12#);
   Invoke ("PART",16#A#);
   Check (Status = Core.Values.Available and then Value.Status = AML_Execute.Returned and then Value.Number = 16#7A# and then Memory (0) = 16#7A#);
   Invoke ("BRW0",16#AB#);
   Check (Status = Core.Values.Available and then Value.Status = AML_Execute.Returned and then Value.Number = 16#AB#);
   Check (Memory (8) = 3 and Memory (11) = 16#AB# and not Physical_Held);
   Invoke ("IRW0",16#CD#);
   Check (Status = Core.Values.Available and then Value.Status = AML_Execute.Returned and then Value.Number = 16#CD#);
   Check (Memory (9) = 1 and Memory (10) = 16#CD# and not Physical_Held);
   Saved_Reads := Reads; Saved_Writes := Writes;
   Invoke ("UNRD",0,0); Check (Value.Status = AML_Execute.Unsupported_Value and Reads = Saved_Reads and Writes = Saved_Writes);
   Incomplete_Selector := True; Saved_Writes := Writes;
   Invoke ("BRW0",16#EF#);
   Check (Value.Status = AML_Execute.Field_Indeterminate and Writes = Saved_Writes + 1);
   Check (Provider.Pending (IO) and Provider.Uncertain (IO).Present and Catalog.Observe (Hardware).Pending = 1);
   Saved_Reads := Reads; Saved_Writes := Writes;
   Invoke ("BRW0",16#EF#);
   Check (Status /= Core.Values.Available and Reads = Saved_Reads and Writes = Saved_Writes);
   Check (Core.Retained_Results (Service) = 0);
   Check (not Physical_Held and Acquires = Releases);
   Check ((Acquires > 0) = Locked);
   Ada.Text_IO.Put_Line ("LOCAL FIELD CORE width=" & Width & " mode=" & Ada.Command_Line.Argument (2) & " checks" & Checks'Image);
end Local_Field_Core_Tests;
