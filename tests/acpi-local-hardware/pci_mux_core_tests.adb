pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.Source_Info;
with Interfaces; use Interfaces;
with Firmware_Tables;
with AML_Decode;
with AML_Delays;
with AML_Objects;
with AML_Execute;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Names;
with ACPI_Service_Core;
with ACPI_Platform_Region_Cache;
with ACPI_Platform_Field_Provider;
with ACPI_Local_Field_Mux;
with ACPI_Local_PCI_Provider;
with ACPI_Local_PCI_Load;
with ACPI_Local_PCI_Results;
with ACPI_PCI_Bus_Geometry;
with ACPI_PCI_Protocol;
with ACPI_Local_NVS_Provider;
with ACPI_NVS_Writes;
with ACPI_NVS_Write_Policy;
with ACPI_Backend_NVS_Records;
with ACPI_Local_Register_Provider;
with ACPI_Backend_Catalog;
with ACPI_Backend_Binding_Types;
with ACPI_Firmware_Data_Layouts;
with ACPI_Global_Lock_Protocol;
with ACPI_Local_Field_Scope;
with ACPI_Field_Provider_View;
with ACPI_Field_Module_Startup;
with ACPI_Field_Evaluation;
with Platform_Pool_ABI;
with PCI_Mux_Fixture;
procedure PCI_Mux_Core_Tests is
   package A renames AML_Field_Access;
   package G renames AML_Field_Layout;
   package L renames ACPI_Firmware_Data_Layouts;
   package P renames ACPI_Global_Lock_Protocol;
   package ABI renames Platform_Pool_ABI;
   subtype Word is Unsigned_64;
   use type A.Target,A.Completion, AML_Decode.Byte, P.Lock_Outcome, L.T.Access_Width;
   use type AML_Decode.Bytes;
   Width : constant String := Ada.Command_Line.Argument (1);
   Mode : constant String := Ada.Command_Line.Argument (2);
   Enabled : constant Boolean := Mode /= "disabled" and Mode /= "legacy-only";
   VA : constant Word := 16#6000_0000_0000#;
   Physical : constant Word := 16#FED0_0000#;
   NVS_Physical : constant Word := 16#8000_0000#;
   Named_VA : constant Word := VA+16#100000#;
   NVS_Maps,NVS_Reads,NVS_Writes,Before_NVS,Before_Named : Natural := 0;
   NVS_Value : Word := 16#42#;
   PCI_Prepares,PCI_Reads,Before_PCI : Natural := 0;
   Logical_Read : Boolean := False;
   Completion_Armed : Boolean := False;
   FACS_Address : constant Word := NVS_Physical+4096;
   Reply : A.Response;
   Saved_Named,Saved_NVS,Saved_Pool : A.Target;
   Checks, Maps, Effects, Callbacks, Acquires, Releases : Natural := 0;
   Held : Boolean := False;
   Selector_Lease : Natural := 0;
   type Lease is limited record Prepared : Boolean := False; end record;
   type Cells is array (Natural range 0 .. 4095) of Unsigned_8;
   Bytes : Cells := [others => 0];
   procedure Check (Good : Boolean; Where : String := GNAT.Source_Info.Source_Location) is
   begin Checks := Checks+1; if not Good then raise Program_Error with Mode & Checks'Image & " " & Where; end if; end Check;
   function Admission return Boolean is (not Held);
   function Due (Item : Lease) return Boolean is (Item.Prepared);
   procedure Window (Deadline : out Word; Budget : out P.Attempt_Budget; Available : out Boolean) is
   begin Deadline := 100; Budget := 3; Available := True; end Window;
   procedure Inspect_Time (Deadline : out Word; Available : out Boolean) is
   begin Deadline := 100; Available := True; end Inspect_Time;
   procedure Acquire (Item : in out Lease; Deadline : Word; Budget : P.Attempt_Budget; Result : out P.Lock_Outcome) is
   begin
      Check (not Held and not Item.Prepared and Deadline = 100 and Budget = 3);
      Held := True; Item.Prepared := True; Acquires := Acquires+1; Result := P.Acquired;
   end Acquire;
   procedure Validate_Lease (Item : Lease; Result : out P.Lock_Outcome) is
   begin Check (Held and Item.Prepared); Result := P.Configured; end Validate_Lease;
   procedure Release (Item : in out Lease; Result : out P.Lock_Outcome) is
   begin Check (Held and Item.Prepared); Held := False; Item.Prepared := False; Releases := Releases+1; Result := P.Released; end Release;
   package Guard is new ACPI_Local_Field_Scope (Lease,Admission,Due,Window,Acquire,Validate_Lease,Release,16,Inspect_Time);
   Transaction : Guard.Context;
   function Inspect (Cap : Natural) return Boolean is (Cap = 1);
   function Exclusive (First,Limit : Word) return Boolean is (First = VA and Limit = VA+16*L.Page_Bytes);
   procedure Clock (Now : out Word; Available : out Boolean) is
   begin Now := 1; Available := True; end Clock;
   function Allowed (Writing : Boolean) return Boolean is
      pragma Unreferenced (Writing);
      OK : Boolean;
   begin Guard.Check_Datum (Transaction,OK); return OK; end Allowed;
   procedure Map (Cap : Natural; PA,Virtual,Pages : Word; Read_Only : Boolean;
      Status : out Word; Why : out ABI.Refusal_Record) is
   begin
      Check (Cap = 1 and PA = Physical and Virtual = VA and Pages = 1 and not Read_Only);
      Maps := Maps+1; Status := (if Mode = "refused" then ABI.Refusal_Word (ABI.Claimed) else 0);
      Why := [others => 0]; Why (ABI.Code) := Status;
   end Map;
   procedure Transfer (Address : Word; Access_Width : L.T.Access_Width; Writing : Boolean;
      Input : Word; Output : out Word; Completed : out Boolean) is
      Offset : Natural;
   begin
      Check (Held and Address >= VA+16 and Address < VA+48 and Access_Width = L.T.Byte_Access);
      Offset := Natural (Address-VA); Effects := Effects+1;
      if Offset in 32 | 34 | 36 | 38 then
         Check (Selector_Lease = Acquires and Selector_Lease > 0);
      end if;
      Output := Word (Bytes (Offset));
      if Writing then Check (Input <= 255); Bytes (Offset) := Unsigned_8 (Input); Output := 0; end if;
      Completed := Mode /= "unknown" and then not (Completion_Armed and then
        ((Mode = "bank-completion-unknown" and then Offset = 38)
          or else (Mode = "index-completion-unknown" and then Offset = 36)));
   end Transfer;
   package Cache is new ACPI_Platform_Region_Cache (Natural,0,Inspect,Exclusive,Map,Clock,Allowed,Transfer,8,16);
   Memory : Cache.Context;
   package Core is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 128,Retained_Result_Capacity => 32,Enable_PCI_Context => True);
   type Declaration is record
      Region : Core.Namespace.Operation_Region_Attributes;
      Name : AML_Names.Segment;
      Node : Core.Namespace.Node_ID;
   end record;
   First_Node : Word := 0;
   procedure Validate (Item : Declaration; Geometry : G.Register_Geometry;
      Origin : out Cache.Source; Accepted : out Boolean) is
   begin
      Check (Item.Name = "MBAR" and Item.Node > Core.Namespace.Root);
      Check (Item.Region.Origin.Known and not Item.Region.Syntax.Address_Is_Literal and not Item.Region.Syntax.Length_Is_Literal);
      Check (Item.Region.Address = Physical+16 and Item.Region.Length = 32);
      Check (Geometry.Region.Base = Item.Region.Address and Geometry.Region.Length = Item.Region.Length);
      if First_Node = 0 then First_Node := Word (Item.Node); end if;
      Origin := (Item.Name,16#F123_4567_0000_0001#,0,Word (Item.Node));
      Accepted := Mode /= "bad-declaration";
   end Validate;
   function Compatible (First_Page,Pages : Word) return Boolean is
   begin Check (First_Page = Physical/L.Page_Bytes and Pages = 1); return True; end Compatible;
   package Platform is new ACPI_Platform_Field_Provider (Cache,Memory,Declaration,Validate,Compatible);
   package B renames ACPI_Backend_Binding_Types;
   Selector : Word := 0;
   Named_Effects : Natural := 0;
   procedure Inspect_Named (Cap : Natural; Info : out B.Description) is
   begin
      if Cap = 1 then Info := (True,B.Port_Resource,16#600#,1,True,True,True);
      else Check (Cap = 2); Info := (True,B.Memory_Resource,NVS_Physical,8192,True,Mode /= "readonly",False); end if;
   end Inspect_Named;
   procedure Map_Named (Cap : Natural; PA,Virtual,Pages : Word; Read_Only : Boolean; Mapped : out Boolean) is
   begin
      Check (Cap = 2 and PA = NVS_Physical and Virtual = Named_VA and Pages = 2 and Read_Only = (Mode = "readonly"));
      NVS_Maps := NVS_Maps+1; Mapped := True;
   end Map_Named;
   procedure Named_Clock (Now : out B.Tick; Available : out Boolean) is
   begin Now := 1; Available := True; end Named_Clock;
   procedure No_Memory (Address : Word; Width : L.T.Access_Width; Writing : Boolean; Input : Word; Output : out Word; Completed : out Boolean) is
      pragma Unreferenced (Address,Width,Writing,Input);
   begin Output := 0; Completed := False; Check (False); end No_Memory;
   procedure Named_Port (Cap : Natural; Port : Unsigned_16; Width : L.T.Access_Width; Writing : Boolean; Input : Word; Output : out Word; Completed : out Boolean) is
   begin
      Check (Held and Cap = 1 and Port = 16#600# and Width = L.T.Byte_Access);
      Named_Effects := Named_Effects+1; Selector_Lease := Acquires;
      if Writing then Selector := Input; Output := 0; else Output := Selector; end if;
      Completed := True;
   end Named_Port;
   package Catalog is new ACPI_Backend_Catalog (Natural,0,Natural,0,Inspect_Named,Inspect_Named,
      Map_Named,Named_Clock,No_Memory,Named_Port,2,4,1,Named_VA,Named_VA+8192,2);
   Hardware : Catalog.Context;
   Local_Scope : constant Word := 16#F123_4567_0000_0001#;
   package Named is new ACPI_Local_Register_Provider (Catalog,Hardware,Local_Scope,4);
   function Is_NVS (Window : Catalog.W.Window_ID) return Boolean is
      use type Catalog.W.Window_ID;
   begin return Window = 2; end Is_NVS;
   procedure Read_NVS (Virtual : Word; Width : L.T.Access_Width; Value : out Word; Completed : out Boolean) is
   begin
      Check (Held and Selector = 7 and Selector_Lease = Acquires and Selector_Lease > 0);
      Check (Virtual >= Named_VA+16 and Virtual < Named_VA+48 and Width = L.T.Byte_Access);
      NVS_Reads := NVS_Reads+1; Value := NVS_Value; Completed := Mode /= "nvs-unknown";
   end Read_NVS;
   package Records is new ACPI_Backend_NVS_Records (Catalog,Hardware,Is_NVS,Read_NVS,1);
   Backing : Records.Context;
   Backing_Status : Records.Outcome;
   use type Records.Outcome;
   procedure Write_NVS (Physical,Virtual : Word; Width : L.T.Access_Width;
      Value : Word; Perform : Boolean; Result : in out ACPI_NVS_Writes.Completion) is
   begin
      Result := ACPI_NVS_Writes.Unavailable;
      Check (Virtual = Named_VA+(Physical-NVS_Physical));
      if not ACPI_NVS_Write_Policy.Permitted (Physical,Width,FACS_Address) then return; end if;
      if not Perform then Result := ACPI_NVS_Writes.Ready; return; end if;
      Check (Held and Selector = 7 and Selector_Lease = Acquires and Virtual = Named_VA+16);
      NVS_Writes := NVS_Writes+1; NVS_Value := Value;
      Result := (if Mode = "write-unknown" then ACPI_NVS_Writes.Unknown
         elsif Mode = "write-fault" then ACPI_NVS_Writes.Fault else ACPI_NVS_Writes.Completed);
   end Write_NVS;
   package Data_Provider is new ACPI_Local_NVS_Provider
      (Records,Backing,Local_Scope,Allowed,Write_Access => Write_NVS);
   package PG renames ACPI_PCI_Bus_Geometry;
   package PR renames ACPI_Local_PCI_Results;
   package PQ renames ACPI_PCI_Protocol;
   PCI_First : constant Word := 16#E000_0000#;
   PCI_VA : constant Word := VA+16#200000#;
   PCI_Key : constant PG.P.Bus_Key := (17,9);
   PCI_Life : constant Word := 16#F123_4567_0000_0011#;
   PCI_ID : constant Word := 16#C123_4567_0000_0011#;
   type PCI_Window is record Description : PR.Window_Description; end record;
   function PCI_Current (Item : PCI_Window) return Boolean is (True);
   function PCI_Geometry (Item : PCI_Window) return PG.Bus_Window is (Item.Description.Geometry);
   function PCI_Virtual (Item : PCI_Window) return Word is (PCI_VA);
   procedure PCI_Clock (Now : out B.Tick; Available : out Boolean) is
   begin Now := 1; Available := True; end PCI_Clock;
   procedure PCI_Transfer (Virtual : Word; Width : L.T.Access_Width; Value : out Word; Completed : out Boolean) is
   begin
      Check (Held and Virtual = PCI_VA+PG.P.Bus_Bytes-PG.P.Function_Bytes and Width = L.T.Byte_Access);
      if not Logical_Read then Check (Selector = 11 and Selector_Lease = Acquires); end if;
      PCI_Reads := PCI_Reads+1; Value := 16#42#; Completed := Mode /= "pci-unknown";
   end PCI_Transfer;
   package PCI_Loads is new ACPI_Local_PCI_Load (PCI_Window,PCI_Current,PCI_Geometry,PCI_Virtual,PCI_Clock,PCI_Transfer);
   PCI_Reader : PCI_Loads.Context;
   function PCI_Pending return Boolean is (PCI_Loads.Pending (PCI_Reader));
   function PCI_Setup_Allowed return Boolean is (not Held);
   procedure PCI_Deadlines (Setup,Data : out Word; Available : out Boolean) is
   begin Setup := 50; Data := 100; Available := True; end PCI_Deadlines;
   function PCI_Description return PR.Window_Description is ((PQ.Ready,PCI_Life,PCI_ID,(17,9,PCI_First,PG.P.Bus_Bytes)));
   procedure PCI_Prepare (Key : PG.P.Bus_Key; Deadline : Word; Result : out PR.Window_Description) is
      use type PG.P.Bus_Key;
   begin
      Check (not Held and Key = PCI_Key and Deadline = 50);
      PCI_Prepares := PCI_Prepares+1; Result := PCI_Description;
   end PCI_Prepare;
   procedure PCI_Inspect (Epoch,ID : Word; Result : out PR.Window_Description) is
   begin Check (Epoch = PCI_Life and ID = PCI_ID); Result := PCI_Description; end PCI_Inspect;
   procedure PCI_Data (Epoch,ID : Word; Region : PG.P.Region; Offset : Word;
      Width : L.T.Access_Width; Deadline : Word; Guard : not null access function return Boolean;
      Result : out PR.Result) is
   begin
      Check (Epoch = PCI_Life and ID = PCI_ID);
      PCI_Loads.Read (PCI_Reader,(Description => PCI_Description),Region,Offset,Width,B.Tick (Deadline),Guard,Result);
   end PCI_Data;
   procedure PCI_Alias (Geometry : G.Register_Geometry; Key : in out PG.P.Bus_Key; Accepted : in out Boolean) is
   begin
      Accepted := Geometry.Region.Base >= PCI_First and Geometry.Region.Base < PCI_First+PG.P.Bus_Bytes;
      if Accepted then Key := PCI_Key; end if;
   end PCI_Alias;
   package Configuration is new ACPI_Local_PCI_Provider
      (PCI_Prepare,PCI_Inspect,PCI_Data,PCI_Pending,PCI_Setup_Allowed,Allowed,PCI_Deadlines,PCI_Alias);
   package Mux is new ACPI_Local_Field_Mux (Declaration,Named,Platform,Data_Provider,Configuration,Allowed);
   IO : Mux.Context;
   procedure Begin_Field (Context : in out Mux.Context; Requires_Lock,For_Write : Boolean; Accepted : in out Boolean) is
   begin
      Accepted := False;
      if Mux.Is_Ready (Context) then Guard.Begin_Field (Transaction,Requires_Lock,For_Write,Accepted); end if;
   end Begin_Field;
   procedure End_Field (Context : in out Mux.Context; Accepted : in out Boolean) is
   begin
      if Mux.Pending (Context) then Guard.Mark_Fault (Transaction); end if;
      Guard.End_Field (Transaction,Accepted); Accepted := Accepted and then not Mux.Pending (Context);
   end End_Field;
   procedure Resolve_Module (Context : in out Mux.Context; Geometry : G.Register_Geometry;
      Target : in out A.Target; Accepted : in out Boolean) is
   begin
      Mux.Select_Target (Context,Geometry,False,Target,Accepted);
      if Accepted and Geometry.Region.Base = NVS_Physical+16 then Saved_NVS := Target; end if;
      if not Accepted and Geometry.Region.Space = G.PCI_Config_Space then Mux.Resolve_PCI (Context,Geometry,True,Target,Accepted); end if;
   end Resolve_Module;
   package View is new ACPI_Field_Provider_View
     (Mux.Context,Mux.Is_Ready,Mux.Identity,Mux.Pending,Mux.Register_Count,
      Mux.Export_Target,Mux.Select_Target,Mux.Describe,Mux.Authorize,Mux.Dispatch,
      Mux.Reconciled,Resolve_Target => Resolve_Module,Begin_Transaction => Begin_Field,End_Transaction => End_Field,Allow_Locked_Fields => True);
   procedure Resolve (Context : in out Mux.Context; Region : Core.Namespace.Operation_Region_Attributes;
      Name : AML_Names.Segment; Node : Core.Namespace.Node_ID; Geometry : G.Register_Geometry;
      Target : in out A.Target; Accepted : in out Boolean) is
   begin
      Callbacks := Callbacks+1;
      Mux.Resolve (Context,(Region,Name,Node),Geometry,100,Target,Accepted);
      if Accepted and Name = "NVRS" then Saved_NVS := Target;
      elsif Accepted and Name = "MBAR" then Saved_Pool := Target; end if;
   end Resolve;
   procedure Legacy (Context : in out Mux.Context; Region : Core.Namespace.Operation_Region_Attributes;
      Geometry : G.Register_Geometry; Target : in out A.Target; Accepted : in out Boolean) is
      pragma Unreferenced (Context,Region,Geometry);
   begin
      Target := A.No_Target; Accepted := False;
      Check (False); -- Computed declarations must never reach this old hook.
   end Legacy;
   package Modules is new ACPI_Field_Module_Startup (Core,View,32,
      Enable_Startup_Firmware_Resolution => Mode /= "disabled",Resolve_Startup_Firmware_Target => Legacy,
      Enable_Startup_Platform_Resolution => Enabled,Resolve_Startup_Platform_Target => Resolve);
   package Evaluation is new ACPI_Field_Evaluation (Core,View,32,
      Enable_Startup_Platform_Resolution => Enabled,Resolve_Startup_Platform_Target => Resolve);
   Run : Evaluation.Context(Evaluation.Default_Capacity);
   Service : aliased Core.State (1,4096,4096,1,Value_Object_Capacity => AML_Objects.Max_Objects,
      Value_Byte_Capacity => AML_Objects.Max_Bytes,Value_Element_Capacity => AML_Objects.Max_Elements);
   Data : Firmware_Tables.Bytes := PCI_Mux_Fixture.Data;
   Sum : Firmware_Tables.Byte := 0;
   use type Firmware_Tables.Byte,Core.Stage_Status,Core.Freeze_Status,Core.Values.Access_Status,
      Core.Values.Module_Outcome,Core.Namespace.Lookup_Status,AML_Execute.Execution_Status;
   Staged : Core.Stage_Status;
   Frozen : Core.Freeze_Status;
   Report : Core.Execution_Report;
   Status : Core.Values.Access_Status;
   Members : Core.Namespace.Initialization_Report;
   Found : Core.Namespace.Lookup_Result;
   Description : Core.Node_Descriptor;
   Result : Core.Values.Result;
   Handle : Core.Values.Value_Handle;
   Read_Bytes : AML_Decode.Bytes (1 .. 8);
   Copied : Natural;
   OK : Boolean;
   Bound : B.Report;
   Added : Catalog.Add_Report;
   use type B.Outcome,Catalog.Add_Code;
   procedure Invoke (Name : String; Value : Word) is
      At_Name : Positive := Name'First;
      Parent : Core.Namespace.Node_ID := Core.Namespace.Root;
   begin
      loop
         Check (At_Name+3 <= Name'Last);
         Core.Find_Object (Service,Parent,AML_Names.Segment (Name (At_Name .. At_Name+3)),Found,Status);
         Check (Status = Core.Values.Available and Found.Status = Core.Namespace.Found);
         exit when At_Name+3 = Name'Last;
         Check (At_Name+4 <= Name'Last and then Name (At_Name+4) = '.');
         Parent := Found.Node; At_Name := At_Name+5;
      end loop;
      Check (Status = Core.Values.Available and Found.Status = Core.Namespace.Found);
      Evaluation.Invoke_Retained (Run,Service,IO,Found.Node,[0 => Value,others => 0],1,100_000,Result,Status);
   end Invoke;
begin
   Data (9) := (if Width = "32" then 1 else 2); Data (10) := 0;
   for B of Data loop Sum := Sum+B; end loop; Data (10) := 0-Sum;
   Catalog.Bind_Ports (Hardware,(1,Catalog.P.IO_Space,16#600#,1,True,True,True),1,100,Bound); Check (Bound.Status = B.Bound);
   Catalog.Bind_Memory (Hardware,(2,Catalog.P.Memory_Space,NVS_Physical,8192,True,Mode /= "readonly",False),2,100,Bound); Check (Bound.Status = B.Bound);
   Catalog.Authorize_Scope (Hardware,Local_Scope,OK); Check (OK);
   Catalog.Add (Hardware,1,(Tag => Local_Scope,Base => 16#600#,Length => 1,Space => Catalog.P.IO_Space,
      Readable => True,Writable => True,Widths => [L.T.Byte_Access => True,others => False],
      Register_Authority => (1,True,True,False,16#FF#),Register_Offset => 0,Register_Width => L.T.Byte_Access),
      Catalog.W.Exact_Writes_Admitted,Added); Check (Added.Status = Catalog.Added);
   Catalog.Seal (Hardware,OK); Check (OK);
   Records.Configure (Backing,Local_Scope,Backing_Status); Check (Backing_Status = Records.Configured);
   Records.Add (Backing,1,2,Backing_Status); Check (Backing_Status = Records.Added);
   Records.Seal (Backing,Backing_Status); Check (Backing_Status = Records.Sealed);
   Cache.Initialize (Memory,1,VA,VA+16*L.Page_Bytes,OK); Check (OK);
   Mux.Initialize (IO,16#F000_0000_0000_0001#,OK); Check (OK);
   Core.Stage_Table (Service,1,Core.DSDT,Data,Staged); Check (Staged = Core.Staged);
   Core.Freeze_Catalog (Service,Frozen); Check (Frozen = Core.Catalog_Frozen_OK);
   Mux.Export_Target (IO,1,Saved_Named,OK); Check (OK);
   Modules.Execute_Frozen (Service,IO,100_000,Report,Status);
   if Mode not in "enabled" | "readonly" | "write-unknown" | "write-fault"
     | "bank-completion-unknown" | "index-completion-unknown" then
      Check (Report.Modules.Outcome /= Core.Values.Modules_Completed);
      if Mode in "disabled" | "legacy-only" then Check (Callbacks = 0 and Maps = 0 and Effects = 0);
      elsif Mode = "bad-declaration" then Check (Callbacks > 0 and Maps = 0 and Effects = 0);
      elsif Mode = "refused" then Check (Callbacks > 0 and Maps = 1 and Effects = 0);
      elsif Mode = "unknown" then Check (Mux.Uncertain (IO).Present and Effects = 1);
      elsif Mode = "nvs-unknown" then Check (Mux.Uncertain (IO).Present and NVS_Reads = 1 and Effects > 0);
      elsif Mode = "pci-unknown" then
         Check (Mux.Uncertain (IO).Present and PCI_Reads = 1 and PCI_Prepares = 1);
         Check (Saved_NVS /= A.No_Target and Saved_Pool /= A.No_Target and Saved_Named /= A.No_Target);
         Before_NVS := NVS_Reads; Before_Named := Named_Effects; Before_PCI := Effects;
         Mux.Dispatch (IO,(Saved_NVS,False,0),Reply); Check (Reply.Status = A.Denied and NVS_Reads = Before_NVS);
         Mux.Dispatch (IO,(Saved_Named,False,0),Reply); Check (Reply.Status = A.Denied and Named_Effects = Before_Named);
         Mux.Dispatch (IO,(Saved_Pool,False,0),Reply); Check (Reply.Status = A.Denied and Effects = Before_PCI);
      else Check (False); end if;
   else
      Check (Status = Core.Values.Available and Report.Modules.Outcome = Core.Values.Modules_Completed);
      Check (Maps = 1 and Callbacks > 0 and Bytes (16) = 16#5A# and Bytes (23) = 16#A#);
      Check (Cache.Read_Profile (Memory,1).Origin.Name = "MBAR" and Cache.Read_Profile (Memory,1).Origin.Node = First_Node);
      Core.Initialize_Members (Service,Members,Status); Check (Status = Core.Values.Available);
      Evaluation.Prepare (Run,Service,IO,OK); Check (OK);
      if Mode in "bank-completion-unknown" | "index-completion-unknown" then
         declare
            Before_Effects : constant Natural := Effects;
            Before_Acquires : constant Natural := Acquires;
            Saved_Completion : A.Observation;
            Expected_Value : constant Word :=
              (if Mode = "bank-completion-unknown" then 16#25# else 16#35#);
         begin
            Completion_Armed := True;
            Invoke ((if Mode = "bank-completion-unknown" then "DBW0" else "DIW0"),Expected_Value);
            Check (Status /= Core.Values.Available or else Result.Status /= AML_Execute.Returned);
            Check (Effects = Before_Effects + 1 and then Acquires = Before_Acquires + 1);
            Check (Acquires = Releases and then not Held and then Maps = 1);
            Check (Core.Valid (Service) and then Core.Retained_Results (Service) = 0);
            Saved_Completion := Mux.Uncertain (IO);
            Check (Saved_Completion.Present and then Saved_Completion.Item.For_Write
              and then Saved_Completion.Item.Value = Expected_Value);
            Before_Named := Named_Effects; Before_NVS := NVS_Reads; Before_PCI := PCI_Reads;
            Invoke ((if Mode = "bank-completion-unknown" then "DBW0" else "DIW0"),Expected_Value);
            Check (Status /= Core.Values.Available or else Result.Status /= AML_Execute.Returned);
            Invoke ("FRD0",16#66#);
            Check (Status /= Core.Values.Available or else Result.Status /= AML_Execute.Returned);
            Check (Effects = Before_Effects + 1 and then Acquires = Before_Acquires + 1);
            Check (Named_Effects = Before_Named and then NVS_Reads = Before_NVS and then PCI_Reads = Before_PCI);
            Mux.Dispatch (IO,Saved_Completion.Item,Reply);
            Check (Reply.Status = A.Denied and then Effects = Before_Effects + 1);
            Mux.Dispatch (IO,(Saved_Named,False,0),Reply);
            Check (Reply.Status = A.Denied and then Named_Effects = Before_Named);
            Mux.Dispatch (IO,(Saved_NVS,False,0),Reply);
            Check (Reply.Status = A.Denied and then NVS_Reads = Before_NVS);
            Check (A."=" (Mux.Uncertain (IO), Saved_Completion));
            Check (Core.Valid (Service) and then Acquires = Releases and then not Held
              and then Core.Retained_Results (Service) = 0);
         end;
         Ada.Text_IO.Put_Line ("PCI MUX CORE width=" & Width & " mode=" & Mode & " checks" & Checks'Image);
         return;
      end if;
      Core.Find_Object (Service,Core.Namespace.Root,"ANS0",Found,Status);
      Check (Status = Core.Values.Available and Found.Status = Core.Namespace.Found);
      Core.Describe_Node (Service,Found.Node,Description,Status); Check (Status = Core.Values.Available);
      Evaluation.Evaluate_Named (Run,Service,IO,Description,100_000,Result,Status);
      Check (Status = Core.Values.Available and Result.Status = AML_Execute.Returned and Result.Number = 16#5A#);
      Invoke ("WRW0",16#12345678#); Check (Status = Core.Values.Available);
      if Width = "32" then
         Check (Result.Status = AML_Execute.Object_Returned); Handle := Result.Handle;
         Core.Read_Result_Bytes (Service,Handle,0,Read_Bytes,Copied,Status);
         Check (Status = Core.Values.Available and Copied = 8 and Read_Bytes = AML_Decode.Bytes'[16#78#,16#56#,16#34#,16#12#,0,0,0,0]);
         Core.Release_Result (Service,Handle,Status); Check (Status = Core.Values.Available);
      else Check (Result.Status = AML_Execute.Returned and Result.Number = 16#12345678#); end if;
      Invoke ("BRW0",16#CD#);
      Check (Status = Core.Values.Available and Result.Status = AML_Execute.Returned and Result.Number = 16#CD#);
      Check (Selector = 3 and Named_Effects > 0 and Bytes (32) = 16#CD#);
      Invoke ("FRD0",16#66#);
      Check (Status = Core.Values.Available and Result.Status = AML_Execute.Returned and Result.Number = 16#66#);
      Check (Maps = 1 and Bytes (19) = 16#66#);
      Invoke ("FBK0",16#77#);
      Check (Status = Core.Values.Available and Result.Status = AML_Execute.Returned and Result.Number = 16#77#);
      Check (Maps = 1 and Selector = 13 and Bytes (34) = 16#77#);
      Invoke ("FIR0",16#88#);
      Check (Status = Core.Values.Available and Result.Status = AML_Execute.Returned and Result.Number = 16#88#);
      Check (Maps = 1 and Selector = 0 and Bytes (36) = 16#88#);
      for Attempt in 1 .. 20 loop
         Invoke ("DBW0",Word (Attempt));
         Check (Status = Core.Values.Available and Result.Status = AML_Execute.Returned
           and Result.Number = Word (Attempt));
         Check (Selector = Word (Attempt) and Bytes (38) = Unsigned_8 (Attempt));
         declare
            Temporary : Core.Namespace.Lookup_Result;
         begin
            Core.Find_Object (Service,Found.Node,"DBNK",Temporary,Status);
            Check (Status = Core.Values.Available and Temporary.Status = Core.Namespace.Not_Found);
            Core.Find_Object (Service,Core.Namespace.Root,"DBNK",Temporary,Status);
            Check (Status = Core.Values.Available and Temporary.Status = Core.Namespace.Not_Found);
         end;
         Invoke ("DIW0",Word (Attempt + 32));
         Check (Status = Core.Values.Available and Result.Status = AML_Execute.Returned
           and Result.Number = Word (Attempt + 32));
         Check (Selector = 0 and Bytes (36) = Unsigned_8 (Attempt + 32));
         declare
            Temporary : Core.Namespace.Lookup_Result;
         begin
            Core.Find_Object (Service,Found.Node,"DIDX",Temporary,Status);
            Check (Status = Core.Values.Available and Temporary.Status = Core.Namespace.Not_Found);
            Core.Find_Object (Service,Core.Namespace.Root,"DIDX",Temporary,Status);
            Check (Status = Core.Values.Available and Temporary.Status = Core.Namespace.Not_Found);
         end;
         Check (Maps = 1 and not Held and Acquires = Releases and Core.Retained_Results (Service) = 0);
      end loop;
      Before_PCI := PCI_Reads; Mux.Close_Setup (IO);
      Invoke ("PRD0",0);
      Check (Status = Core.Values.Available and Result.Status = AML_Execute.Returned and Result.Number = 16#42#);
      Check (PCI_Reads = Before_PCI+1 and PCI_Prepares = 1);
      Before_PCI := PCI_Reads; Before_Named := Named_Effects;
      Invoke ("PWR0",1);
      Check (Status /= Core.Values.Available or else Result.Status /= AML_Execute.Returned);
      Check (PCI_Reads = Before_PCI and Named_Effects = Before_Named and PCI_Prepares = 1);
      Before_PCI := PCI_Reads;
      Invoke ("CUTR",0); Check (Status /= Core.Values.Available or else Result.Status /= AML_Execute.Returned);
      Invoke ("CUTE",0); Check (Status /= Core.Values.Available or else Result.Status /= AML_Execute.Returned);
      Check (PCI_Reads = Before_PCI and PCI_Prepares = 1 and Named_Effects = Before_Named);
      Logical_Read := True; Invoke ("PCI0.DEV0.LRD0",0); Logical_Read := False;
      Check (Status = Core.Values.Available and Result.Status = AML_Execute.Returned and Result.Number = 16#42#);
      Check (PCI_Reads = Before_PCI+1 and PCI_Prepares = 1);
      Invoke ("NRD0",0);
      Check (Status = Core.Values.Available and Result.Status = AML_Execute.Returned and Result.Number = 16#42#);
      Check (Selector = 7 and NVS_Maps = 1 and NVS_Reads >= 2);
      Before_NVS := NVS_Reads; Before_Named := Named_Effects;
      Invoke ("NWR0",16#55#);
      if Mode = "readonly" then
         Check (Status /= Core.Values.Available or else Result.Status /= AML_Execute.Returned);
         Check (NVS_Writes = 0 and NVS_Reads = Before_NVS and Named_Effects = Before_Named);
      elsif Mode = "write-unknown" then
         Check (Status /= Core.Values.Available or else Result.Status /= AML_Execute.Returned);
         Check (NVS_Writes = 1 and Mux.Uncertain (IO).Present and Mux.Uncertain (IO).Item.For_Write
            and Mux.Uncertain (IO).Item.Value = 16#55#);
         Mux.Dispatch (IO,Mux.Uncertain (IO).Item,Reply); Check (Reply.Status = A.Denied and NVS_Writes = 1);
      elsif Mode = "write-fault" then
         Check (Status /= Core.Values.Available or else Result.Status /= AML_Execute.Returned);
         Check (NVS_Writes = 1 and Mux.Completed_Fault (IO).Present);
      else
         Check (Status = Core.Values.Available and Result.Status = AML_Execute.Returned and Result.Number = 16#55#);
         Check (NVS_Writes = 1 and NVS_Reads = Before_NVS+1 and Named_Effects > Before_Named);
         Before_NVS := NVS_Reads; Before_Named := Named_Effects;
         Invoke ("CFWR",1);
         Check (Status /= Core.Values.Available or else Result.Status /= AML_Execute.Returned);
         Check (NVS_Writes = 1 and NVS_Reads = Before_NVS and Named_Effects = Before_Named);
      end if;
      if Mode not in "write-unknown" | "write-fault" then
         Invoke ("PART",5); Check (Status = Core.Values.Available and Result.Status = AML_Execute.Returned and Result.Number = 5);
      end if;
      Check (Maps = 1 and Acquires = Releases and not Held and Core.Retained_Results (Service) = 0);
   end if;
   Check (not Held and Acquires = Releases);
   Ada.Text_IO.Put_Line ("PCI MUX CORE width=" & Width & " mode=" & Mode & " checks" & Checks'Image);
end PCI_Mux_Core_Tests;
