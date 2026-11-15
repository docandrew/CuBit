with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Interfaces;
with AML_Objects;
with ACPI_Service_Core;
with ACPI_Battery_Information;
with ACPI_Battery_Information_Reader;
with ACPI_Battery_Legacy_Information;
with ACPI_Battery_Legacy_Information_Reader;
with ACPI_Battery_Information_Selection;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with Firmware_Tables;
procedure Selection_Tests is
   package C is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 128,
      Aggregate_Method_Capacity => 8192, Retained_Result_Capacity => 8);
   package F is new ACPI_Battery_Information (8);
   package G is new ACPI_Battery_Legacy_Information (16);
   use type C.Stage_Status, C.Freeze_Status, C.Values.Access_Status;
   use type C.Values.Module_Outcome, AML_Execute.Execution_Status;
   use type F.BIX_Revision;
   use type Interfaces.Unsigned_32, Interfaces.Unsigned_8;
   Service, Other : aliased C.State (1, 4096, 4096, 1,
     Value_Object_Capacity => AML_Objects.Max_Objects,
     Value_Byte_Capacity => AML_Objects.Max_Bytes,
     Value_Element_Capacity => AML_Objects.Max_Elements);
   Checks, Extended_Calls, Legacy_Calls : Natural := 0;
   type Fault is (Normal, Overcharge, Unknown_Field);
   Extended_Fault, Legacy_Fault : Fault := Normal;
   procedure Check (OK : Boolean; Label_Text : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Label_Text; end if; end Check;
   procedure Evaluate (State : aliased in out C.State; Item : C.Node_Descriptor;
      Budget : Natural; Value : out C.Values.Result; Status : out C.Values.Access_Status;
      Mode : Fault) is
   begin
      if Mode = Unknown_Field then
         Value := (Status => AML_Execute.Field_Indeterminate, Charged => 1);
         Status := C.Values.Available; return;
      end if;
      C.Evaluate_Named (State, Item, Budget, Value, Status);
      if Mode = Overcharge then Value.Charged := Budget + 1; end if;
   end Evaluate;
   procedure Extended_Evaluate (State : aliased in out C.State; Item : C.Node_Descriptor;
      Budget : Natural; Value : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin Extended_Calls := Extended_Calls + 1; Evaluate (State, Item, Budget, Value, Status, Extended_Fault); end Extended_Evaluate;
   procedure Legacy_Evaluate (State : aliased in out C.State; Item : C.Node_Descriptor;
      Budget : Natural; Value : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin Legacy_Calls := Legacy_Calls + 1; Evaluate (State, Item, Budget, Value, Status, Legacy_Fault); end Legacy_Evaluate;
   package E is new ACPI_Battery_Information_Reader (C, F, Extended_Evaluate);
   package L is new ACPI_Battery_Legacy_Information_Reader (C, G, Legacy_Evaluate);
   package S is new ACPI_Battery_Information_Selection (C, F, G, E, L);
   use type S.Outcome, S.Source_Kind;
   use type S.Unit_Kind, S.Technology_Kind;
   Report : S.Report;
   Extended, Legacy, Missing, Foreign, Setter : C.Node_Descriptor;
   function Find (State : C.State; Name : String) return C.Node_Descriptor is
      Node : C.Namespace.Node_ID; Item : C.Node_Descriptor; Status : C.Values.Access_Status;
   begin
      C.Find_Child (State, C.Namespace.Root, Name, Node, Status);
      Check (Status = C.Values.Available, "find " & Name);
      C.Describe_Node (State, Node, Item, Status);
      Check (Status = C.Values.Available, "describe " & Name); return Item;
   end Find;
   procedure Prepare (State : aliased in out C.State; Data : Firmware_Tables.Bytes) is
      Stage : C.Stage_Status; Freeze : C.Freeze_Status; Execution : C.Execution_Report;
      Members : C.Namespace.Initialization_Report; Status : C.Values.Access_Status;
   begin
      C.Stage_Table (State, 1, C.DSDT, Data, Stage); Check (Stage = C.Staged, "stage");
      C.Freeze_Catalog (State, Freeze); Check (Freeze = C.Catalog_Frozen_OK, "freeze");
      C.Execute_Frozen (State, 1000, Execution, Status);
      Check (Status = C.Values.Available and then Execution.Modules.Outcome = C.Values.Modules_Completed, "modules");
      C.Initialize_Members (State, Members, Status); Check (Status = C.Values.Available, "members");
   end Prepare;
   procedure Mode (Number : Natural) is
      Value : C.Values.Result; Status : C.Values.Access_Status;
   begin
      C.Invoke_Retained (Service, C.Values.Position (Setter),
        [0 => AML_Decode.Integer_Value (Number), others => 0], 1, 1000, Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.No_Return, "mode setter");
   end Mode;
   procedure Query (Item : C.Node_Descriptor; Budget : Natural := 1000) is
   begin
      Extended_Calls := 0; Legacy_Calls := 0;
      S.Query (Service, Item, Budget, Report);
      Check (Report.Charged <= Budget and then C.Retained_Results (Service) = 0, "budget and root preservation");
      Check ((if Report.Status = S.Sampled then Report.Value.Source /= S.None_Reported
        else Report.Value.Source = S.None_Reported), "tagged source matches outcome");
      declare Projection : constant S.Common_Information := S.Common (Report.Value); begin
         Check (Projection.Source = Report.Value.Source, "projection preserves source");
         if Report.Status = S.Sampled then
            Check (Projection.Units = S.Charge_And_Current and then
              Projection.Technology = S.Secondary and then Projection.Design.Value = 42000
              and then Projection.Last_Full.Value = 40000 and then Projection.Voltage.Value = 11000
              and then Projection.Warning = 2000 and then Projection.Low = 1000
              and then Projection.Granularity_One = 1 and then Projection.Granularity_Two = 10
              and then Projection.Identifiers (S.Model_Number).Length = 5
              and then Projection.Identifiers (S.Serial_Number).Length = 4
              and then Projection.Identifiers (S.Battery_Type).Length = 4
              and then Projection.Identifiers (S.OEM_Information).Length = 0,
              "explicit common projection");
         else
            Check (not Projection.Design.Known and then Projection.Design.Value = 0 and then
              Projection.Units = S.Unspecified_Unit and then
              Projection.Identifiers (S.Model_Number).Length = 0, "no failure projection payload");
         end if;
      end;
   end Query;
   Baseline : Natural;
begin
   declare
      Input : Ada.Streams.Stream_IO.File_Type; Bytes : Ada.Streams.Stream_Element_Array (1 .. 4096);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Stream_IO.Open (Input, Ada.Streams.Stream_IO.In_File, Ada.Command_Line.Argument (1));
      Ada.Streams.Stream_IO.Read (Input, Bytes, Last); Ada.Streams.Stream_IO.Close (Input);
      declare Data : Firmware_Tables.Bytes (1 .. Natural (Last)); begin
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Bytes (Ada.Streams.Stream_Element_Offset (I))); end loop;
         Prepare (Service, Data); Prepare (Other, Data);
      end;
   end;
   Extended := Find (Service, "EXT0"); Legacy := Find (Service, "BAT0");
   Missing := Find (Service, "NON0"); Setter := Find (Service, "SELM"); Foreign := Find (Other, "BAT0");
   Query (Extended);
   Check (Report.Status = S.Sampled and then Report.Value.Source = S.Extended_Reported
     and then Extended_Calls = 1 and then Legacy_Calls = 0 and then not Report.Legacy_Attempted
     and then Report.Value.Extended.Version = F.Revision_Zero, "prefer actual BIX");
   Query (Legacy);
   Check (Report.Status = S.Sampled and then Report.Value.Source = S.Legacy_Reported
     and then Extended_Calls = 0 and then Legacy_Calls = 1 and then Report.Legacy_Attempted
     and then Report.Value.Legacy.Design.Value = 42000, "missing-only BIF selection");
   Baseline := Report.Charged;
   Query (Missing); Check (Report.Status = S.Missing_Information and then Legacy_Calls = 0, "both missing");
   Query (Foreign); Check (Report.Status = S.Extended_Failed and then not Report.Legacy_Attempted, "foreign context");
   for Bad in 1 .. 2 loop
      Mode (Bad); Query (Extended);
      Check (Report.Status = S.Extended_Failed and then Legacy_Calls = 0 and then not Report.Legacy_Attempted,
        "malformed BIX never falls back");
   end loop;
   Mode (1); Query (Legacy); Check (Report.Status = S.Legacy_Failed, "malformed selected BIF");
   Mode (0);
   for Budget in 0 .. Baseline loop
      Query (Legacy, Budget);
      Check (Report.Status = (if Budget = Baseline then S.Sampled else S.Budget_Exhausted), "shared budget boundary");
      Check (Extended_Calls = 0 and then Legacy_Calls = (if Budget = 0 then 0 else 1), "no replay");
   end loop;
   Extended_Fault := Overcharge; Query (Extended);
   Check (Report.Status = S.Recovery_Required and then Legacy_Calls = 0, "BIX contract failure stops");
   Extended_Fault := Unknown_Field; Query (Extended);
   Check (Report.Status = S.Recovery_Required and then Legacy_Calls = 0, "BIX uncertainty stops");
   Extended_Fault := Normal; Legacy_Fault := Overcharge; Query (Legacy);
   Check (Report.Status = S.Recovery_Required, "BIF contract failure stops");
   Legacy_Fault := Unknown_Field; Query (Legacy);
   Check (Report.Status = S.Recovery_Required, "BIF uncertainty stops");
   declare
      Value : G.Information := G.Empty_Information;
      Projection : S.Common_Information;
   begin
      Value.Design := (Known => False, Value => 999);
      Value.Identifiers (G.Model_Number).Length := G.Text_Capacity;
      Value.Identifiers (G.Model_Number).Data := [others => Character'Pos ('L')];
      Projection := S.Common ((Source => S.Legacy_Reported, Legacy => Value));
      Check (Projection.Source = S.Legacy_Reported and then
        Projection.Identifiers (S.Model_Number).Length = G.Text_Capacity and then
        Projection.Identifiers (S.Model_Number).Data (G.Text_Capacity) = Character'Pos ('L'),
        "different text capacities never truncate");
      Check (not Projection.Design.Known and then Projection.Design.Value = 0,
        "unknown common quantity has zero payload");
   end;
   Put_Line ("Battery information selection checks" & Checks'Image);
end Selection_Tests;
