pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with ACPI_Service_Core;
with ACPI_Power_Source;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with Firmware_Tables;
procedure Power_Source_Tests is
   package C is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 128,
      Aggregate_Method_Capacity => 4096, Retained_Result_Capacity => 8);
   package Q is new ACPI_Power_Source (C);
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type AML_Execute.Execution_Status;
   use type AML_Decode.Integer_Value;
   use type Q.Query_Outcome;
   use type Q.Source_State;
   use type Q.Identification_Outcome;
   Service, Other : aliased C.State (1, 4096, 4096, 1);
   Checks, Calls : Natural := 0;
   Overcharge : Boolean := False;
   procedure Check (OK : Boolean; Label_Text : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Label_Text; end if;
   end Check;
   procedure Evaluate
     (State : aliased in out C.State; Item : C.Node_Descriptor;
      Budget : Natural; Result : out C.Values.Result;
      Status : out C.Values.Access_Status) is
   begin
      Calls := Calls + 1;
      C.Evaluate_Named (State, Item, Budget, Result, Status);
      if Overcharge and then Budget < Natural'Last then Result.Charged := Budget + 1; end if;
   end Evaluate;
   package B is new ACPI_Power_Source (C, Evaluate);
   use type B.Query_Outcome;
   use type B.Identification_Outcome;
   function Find (State : C.State; Name : String) return C.Node_Descriptor is
      Node : C.Namespace.Node_ID;
      Item : C.Node_Descriptor;
      Status : C.Values.Access_Status;
   begin
      C.Find_Child (State, C.Namespace.Root, Name, Node, Status);
      Check (Status = C.Values.Available, "find " & Name);
      C.Describe_Node (State, Node, Item, Status);
      Check (Status = C.Values.Available, "describe " & Name);
      return Item;
   end Find;
   procedure Prepare (State : aliased in out C.State; Data : Firmware_Tables.Bytes) is
      Stage : C.Stage_Status;
      Freeze : C.Freeze_Status;
      Execution : C.Execution_Report;
      Members : C.Namespace.Initialization_Report;
      Status : C.Values.Access_Status;
   begin
      C.Stage_Table (State, 1, C.DSDT, Data, Stage); Check (Stage = C.Staged, "stage");
      C.Freeze_Catalog (State, Freeze); Check (Freeze = C.Catalog_Frozen_OK, "freeze");
      C.Execute_Frozen (State, 1000, Execution, Status);
      Check (Status = C.Values.Available and then Execution.Modules.Outcome = C.Values.Modules_Completed, "modules");
      C.Initialize_Members (State, Members, Status); Check (Status = C.Values.Available, "members");
   end Prepare;
   Device, Setter, Foreign : C.Node_Descriptor;
   Report : B.Query_Report;
   ID : B.Identification_Report;
   Plain : Q.Query_Report;
   Plain_ID : Q.Identification_Report;
   procedure Set_Mode (Mode, HID_Mode : Natural) is
      Value : C.Values.Result;
      Status : C.Values.Access_Status;
   begin
      C.Invoke_Retained (Service, C.Values.Position (Setter),
        [0 => AML_Decode.Integer_Value (Mode), 1 => AML_Decode.Integer_Value (HID_Mode), others => 0],
        2, 1000, Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.No_Return, "set mode");
      Calls := 0;
   end Set_Mode;
   function Number (Name : String) return Natural is
      Value : C.Values.Result;
      Status : C.Values.Access_Status;
   begin
      C.Evaluate_Named (Service, Find (Service, Name), 1000, Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.Returned, "counter");
      return Natural (Value.Number);
   end Number;
begin
   Check (Ada.Command_Line.Argument_Count = 1, "fixture path");
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
   Device := Find (Service, "AC00"); Setter := Find (Service, "SETM"); Foreign := Find (Other, "AC00");
   Q.Query (Service, Device, 1000, Plain);
   Check (Plain.Outcome = Q.Sampled and then Plain.State = Q.Offline, "default offline sample");
   Q.Identify (Service, Device, 1000, Plain_ID);
   Check (Plain_ID.Outcome = Q.Matched, "default exact identity");
   for Mode in 0 .. 7 loop
      Set_Mode (Mode, 0);
      B.Query (Service, Device, (if Mode = 7 then 100 else 1000), Report);
      case Mode is
         when 0 | 1 => Check (Report.Outcome = B.Sampled and then Report.Raw = AML_Decode.Integer_Value (Mode)
           and then Report.State = (if Mode = 0 then B.Offline else B.Online), "typed source use");
         when 2 | 3 => Check (Report.Outcome = B.Invalid_Status_Value and then Report.State = B.Unknown
           and then Report.Raw = (if Mode = 2 then 2 else 16#100000001#), "invalid integer preserved without narrowing");
         when 4 | 5 | 6 => Check (Report.Outcome = B.Wrong_Result_Type and then Report.State = B.Unknown, "no status coercion");
         when 7 => Check (Report.Outcome = B.Budget_Exhausted and then Report.State = B.Unknown, "bounded status method");
      end case;
      Check (Calls = 1 and then Report.Charged <= 1000 and then C.Retained_Results (Service) = 0, "query accounting/cleanup");
      Check (Number ("RCNT") = 0, "no ancestor evaluation");
   end loop;
   for Mode in 0 .. 7 loop
      Set_Mode (1, Mode);
      B.Identify (Service, Device, (if Mode = 6 then 100 else 1000), ID);
      case Mode is
         when 0 => Check (ID.Outcome = B.Matched, "exact ACPI0003");
         when 1 | 2 | 5 | 7 => Check (ID.Outcome = B.Not_Power_Source, "nonmatch not prefix or integer coercion");
         when 3 | 4 => Check (ID.Outcome = B.Wrong_HID_Type, "typed hardware identity");
         when 6 => Check (ID.Outcome = B.Budget_Exhausted, "bounded identity method");
      end case;
      Check (Calls = 1 and then ID.Charged <= 1000 and then C.Retained_Results (Service) = 0, "identity accounting/cleanup");
   end loop;
   for I in 1 .. 3 loop
      declare Node : constant C.Node_Descriptor := Find (Service, (if I = 1 then "NAM0" elsif I = 2 then "ALS0" else "ALH0")); begin
         B.Identify (Service, Node, 1000, ID); Check (ID.Outcome = B.Matched, "named/aliased HID");
         B.Query (Service, Node, 1000, Report);
         Check (Report.Outcome = B.Sampled and then Report.State = (if I = 3 then B.Offline else B.Online), "named/aliased source status");
      end;
   end loop;
   B.Query (Service, Find (Service, "MIS0"), 1000, Report);
   Check (Report.Outcome = B.Missing_PSR and then Report.State = B.Unknown and then Number ("RCNT") = 0, "missing is not offline or ancestor");
   B.Identify (Service, Find (Service, "MIH0"), 1000, ID);
   Check (ID.Outcome = B.Missing_HID and then Number ("RCNT") = 0, "no HID ancestor fallback");
   Calls := 0;
   B.Query (Service, Foreign, 1000, Report); Check (Report.Outcome = B.Invalid_Device and then Calls = 0, "foreign query");
   B.Identify (Service, Foreign, 1000, ID); Check (ID.Outcome = B.Invalid_Device and then Calls = 0, "foreign identity");
   B.Query (Service, Setter, 1000, Report); Check (Report.Outcome = B.Invalid_Device and then Calls = 0, "nondevice query");
   B.Query (Service, Device, 0, Report); Check (Report.Outcome = B.Budget_Exhausted and then Calls = 0, "zero budget");
   Set_Mode (1, 0); Overcharge := True;
   B.Query (Service, Device, 1000, Report);
   Check (Report.Outcome = B.Provider_Contract_Error and then Report.Charged = 0
     and then Report.Last_Charge = 1001 and then Report.State = B.Unknown and then Report.Raw = 0, "query overcharge");
   B.Identify (Service, Device, 1000, ID);
   Check (ID.Outcome = B.Provider_Contract_Error and then ID.Charged = 0 and then ID.Last_Charge = 1001
     and then C.Retained_Results (Service) = 0, "identity overcharge releases returned string");
   Overcharge := False;
   Check (C.Retained_Results (Service) = 0 and then Number ("RCNT") = 0, "final ownership");
   Ada.Text_IO.Put_Line ("ACPI power source checks" & Checks'Image);
end Power_Source_Tests;
