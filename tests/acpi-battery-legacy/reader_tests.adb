with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with AML_Objects;
with ACPI_Service_Core;
with ACPI_Battery_Legacy_Information;
with ACPI_Battery_Legacy_Information_Reader;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with Firmware_Tables;
procedure Reader_Tests is
   package C is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 128,
      Aggregate_Method_Capacity => 8192, Retained_Result_Capacity => 8);
   package F is new ACPI_Battery_Legacy_Information (8);
   use type C.Stage_Status, C.Freeze_Status, C.Values.Access_Status;
   use type C.Values.Module_Outcome, AML_Execute.Execution_Status;
   use type F.Outcome, F.Information, F.Unit_Kind;
   Service, Other : aliased C.State (1, 4096, 4096, 1,
     Value_Object_Capacity => AML_Objects.Max_Objects,
     Value_Byte_Capacity => AML_Objects.Max_Bytes,
     Value_Element_Capacity => AML_Objects.Max_Elements);
   Checks, Calls : Natural := 0;
   Overcharge : Boolean := False;
   procedure Check (OK : Boolean; Text : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Text; end if; end Check;
   procedure Evaluate (State : aliased in out C.State; Item : C.Node_Descriptor;
      Budget : Natural; Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin
      Calls := Calls + 1; C.Evaluate_Named (State, Item, Budget, Result, Status);
      if Overcharge then Result.Charged := Budget + 1; end if;
   end Evaluate;
   package B is new ACPI_Battery_Legacy_Information_Reader (C, F, Evaluate);
   use type B.Query_Outcome;
   Report : B.Query_Report;
   function Matches (Value : F.Text; Expected : String) return Boolean is
     (Value.Length = Expected'Length and then
       (for all I in 1 .. Value.Length => Value.Data (I) =
         Character'Pos (Expected (Expected'First + I - 1))));
   Device, Alias_Device, Missing, Setter, Foreign : C.Node_Descriptor;
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
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.No_Return, "mode"); Calls := 0;
   end Mode;
   procedure Query (Item : C.Node_Descriptor; Budget : Natural := 1000) is
   begin
      B.Query (Service, Item, Budget, Report);
      Check (Report.Charged <= Budget and then C.Retained_Results (Service) = 0, "charge and retained cleanup");
      Check (Report.Outcome = B.Sampled or else Report.Value = F.Empty_Information, "no partial information");
   end Query;
   Baseline : Natural;
begin
   declare
      Input : Ada.Streams.Stream_IO.File_Type;
      Stream : Ada.Streams.Stream_Element_Array (1 .. 4096);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Stream_IO.Open (Input, Ada.Streams.Stream_IO.In_File, Ada.Command_Line.Argument (1));
      Ada.Streams.Stream_IO.Read (Input, Stream, Last); Ada.Streams.Stream_IO.Close (Input);
      declare Data : Firmware_Tables.Bytes (1 .. Natural (Last)); begin
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Stream (Ada.Streams.Stream_Element_Offset (I))); end loop;
         Prepare (Service, Data); Prepare (Other, Data);
      end;
   end;
   Device := Find (Service, "BAT0"); Alias_Device := Find (Service, "ALS0");
   Missing := Find (Service, "NON0"); Setter := Find (Service, "SELM"); Foreign := Find (Other, "BAT0");
   Mode (0); Query (Device);
   Check (Report.Outcome = B.Sampled and then Calls = 1 and then Report.Fields_Read = 13, "canonical BIF once");
   Check (Report.Value.Units = F.Charge_And_Current and then Report.Value.Design.Value = 42000
     and then Report.Value.Last_Full.Value = 40000 and then Report.Value.Voltage.Value = 11000,
     "legacy fields extracted in order");
   Check (Matches (Report.Value.Identifiers (F.Model_Number), "CUBIT") and then
     Matches (Report.Value.Identifiers (F.Serial_Number), "SER1") and then
     Matches (Report.Value.Identifiers (F.Battery_Type), "LION") and then
     Matches (Report.Value.Identifiers (F.OEM_Information), ""), "owned text");
   Check (Report.Value.Warning = 2000 and then Report.Value.Low = 1000 and then
     Report.Value.Granularity_One = 1 and then Report.Value.Granularity_Two = 10,
     "all numeric wire fields");
   Baseline := Report.Charged;
   Query (Alias_Device); Check (Report.Outcome = B.Sampled, "method alias");
   Calls := 0; Query (Missing); Check (Report.Outcome = B.Missing_BIF and then Calls = 0, "no inherited BIF");
   Query (Foreign); Check (Report.Outcome = B.Invalid_Device and then Calls = 0, "foreign owner");
   for Action in 1 .. 7 loop
      Mode (Action); Query (Device); Check (Calls = 1, "one method evaluation");
      case Action is
         when 1 => Check (Report.Outcome = B.Invalid_Package_Value and then Report.Decode_Status = F.Invalid_Units, "invalid unit");
         when 2 => Check (Report.Outcome = B.Wrong_Element_Count, "wrong package length");
         when 3 => Check (Report.Outcome = B.Wrong_Result_Type, "wrong root");
         when 4 => Check (Report.Outcome = B.Wrong_Element_Type, "wrong numeric field");
         when 5 => Check (Report.Outcome = B.Text_Too_Long, "no string truncation");
         when 6 => Check (Report.Outcome = B.Sampled and then not Report.Value.Design.Known
           and then not Report.Value.Last_Full.Known and then not Report.Value.Voltage.Known, "unknown fields");
         when 7 => Check (Report.Outcome = B.Sampled and then Report.Value.Design.Known
           and then Report.Value.Design.Value = 0, "known zero");
      end case;
   end loop;
   for Budget in 0 .. Baseline loop
      Mode (0); Query (Device, Budget);
      Check (Report.Outcome = (if Budget = Baseline then B.Sampled else B.Budget_Exhausted), "every budget boundary");
      Check (Calls = (if Budget = 0 then 0 else 1), "no budget replay");
   end loop;
   Mode (0); Overcharge := True; Query (Device);
   Check (Report.Outcome = B.Provider_Contract_Error and then Report.Charged = 0, "overcharge cleanup");
   Put_Line ("Legacy battery reader checks" & Checks'Image);
end Reader_Tests;
