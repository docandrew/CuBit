pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with ACPI_Service_Core;
with ACPI_HID_I2C;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with Firmware_Tables;
procedure HID_Identity_Tests is
   package C is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 128,
      Aggregate_Method_Capacity => 4096, Retained_Result_Capacity => 8);
   package Q is new ACPI_HID_I2C (C);
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type AML_Execute.Execution_Status;
   use type AML_Decode.Integer_Value;
   use type Q.Identification_Outcome;
   Service, Other : aliased C.State (1, 4096, 4096, 1);
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Label_Text : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Label_Text; end if;
   end Check;
   Calls : Natural := 0;
   Overcharge : Boolean := False;
   procedure Evaluate
     (S : aliased in out C.State; Item : C.Node_Descriptor;
      Budget : Natural; Result : out C.Values.Result;
      Status : out C.Values.Access_Status) is
   begin
      Calls := Calls + 1;
      C.Evaluate_Named (S, Item, Budget, Result, Status);
      if Overcharge then Result.Charged := Natural'Last; end if;
   end Evaluate;
   package B is new ACPI_HID_I2C (Core => C, Evaluate_Named => Evaluate);
   use type B.Identification_Outcome;
   Report : B.Identification_Report;
   Plain : Q.Identification_Report;
   Device, Missing, Foreign, Setter : C.Node_Descriptor;
   function Find (S : C.State; Name : String) return C.Node_Descriptor is
      Node : C.Namespace.Node_ID;
      Item : C.Node_Descriptor;
      Status : C.Values.Access_Status;
   begin
      C.Find_Child (S, C.Namespace.Root, Name, Node, Status);
      Check (Status = C.Values.Available, "find " & Name);
      C.Describe_Node (S, Node, Item, Status);
      Check (Status = C.Values.Available, "describe " & Name);
      return Item;
   end Find;
   procedure Prepare (S : aliased in out C.State; Data : Firmware_Tables.Bytes) is
      Staged : C.Stage_Status;
      Frozen : C.Freeze_Status;
      Execution : C.Execution_Report;
      Status : C.Values.Access_Status;
      Members : C.Namespace.Initialization_Report;
   begin
      C.Stage_Table (S, 1, C.DSDT, Data, Staged); Check (Staged = C.Staged, "stage");
      C.Freeze_Catalog (S, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze");
      C.Execute_Frozen (S, 1000, Execution, Status);
      Check (Status = C.Values.Available and then Execution.Modules.Outcome = C.Values.Modules_Completed, "execute");
      C.Initialize_Members (S, Members, Status); Check (Status = C.Values.Available, "members");
   end Prepare;
   procedure Set_Mode (Mode : Natural) is
      R : C.Values.Result;
      Status : C.Values.Access_Status;
   begin
      C.Invoke_Retained (Service, C.Values.Position (Setter), [0 => AML_Decode.Integer_Value (Mode), others => 0], 1, 1000, R, Status);
      Check (Status = C.Values.Available and then R.Status = AML_Execute.No_Return, "set mode" & Mode'Image & " " & Status'Image & " " & R.Status'Image);
      Calls := 0;
   end Set_Mode;
   function Number (Name : String) return AML_Decode.Integer_Value is
      R : C.Values.Result;
      Status : C.Values.Access_Status;
   begin
      C.Evaluate_Named (Service, Find (Service, Name), 1000, R, Status);
      Check (Status = C.Values.Available and then R.Status = AML_Execute.Returned, "counter");
      return R.Number;
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
      declare
         Data : Firmware_Tables.Bytes (1 .. Natural (Last));
      begin
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Stream (Ada.Streams.Stream_Element_Offset (I))); end loop;
         Prepare (Service, Data); Prepare (Other, Data);
      end;
   end;
   Device := Find (Service, "IDC0"); Missing := Find (Service, "NOC0");
   Foreign := Find (Other, "IDC0"); Setter := Find (Service, "SETM");
   Set_Mode (0);
   Q.Identify (Service, Device, 1000, Plain);
   Check (Plain.Outcome = Q.Compatible and then Plain.IDs_Examined = 1, "default evaluator");
   for Mode in 0 .. 14 loop
      Set_Mode (Mode);
      B.Identify (Service, Device, (if Mode = 8 then 100 else 1000), Report);
      case Mode is
         when 0 | 1 | 2 | 13 | 14 => Check (Report.Outcome = B.Compatible, "compatible ID forms" & Mode'Image & " " & Report.Outcome'Image);
         when 3 | 4 | 9 | 10 | 11 | 12 => Check (Report.Outcome = B.Not_Compatible, "no match or narrowing");
         when 5 | 6 | 7 => Check (Report.Outcome = B.Wrong_Result_Type, "flat typed ID list including tail");
         when 8 => Check (Report.Outcome = B.Budget_Exhausted, "AML budget");
      end case;
      if Mode in 2 | 13 then Check (Report.IDs_Examined = 3, "scan all entries"); end if;
      if Mode = 4 then Check (Report.IDs_Examined = 0, "empty package"); end if;
      Check (Calls = 1 and then Report.Charged = Report.AML_Charged + Report.IDs_Examined
        and then Report.Charged <= 1000, "one evaluation and combined work accounting");
      Check (C.Retained_Results (Service) = 0, "cleanup per mode");
      Check (Number ("RCNT") = 0, "no ancestor evaluation");
   end loop;
   for I in 1 .. 4 loop
      B.Identify (Service, Find (Service, (if I = 1 then "NAM0" elsif I = 2 then "INT0" elsif I = 3 then "PKG0" else "ALC0")), 1000, Report);
      Check (Report.Outcome = B.Compatible and then C.Retained_Results (Service) = 0, "named values");
   end loop;
   Set_Mode (0);
   B.Identify (Service, Missing, 1000, Report);
   Check (Report.Outcome = B.Missing_CID and then Calls = 0 and then Number ("RCNT") = 0, "no ancestor fallback");
   B.Identify (Service, Foreign, 1000, Report);
   Check (Report.Outcome = B.Invalid_Device and then Calls = 0, "foreign descriptor");
   B.Identify (Service, Setter, 1000, Report);
   Check (Report.Outcome = B.Invalid_Device and then Calls = 0, "non-device");
   B.Identify (Service, Device, 0, Report);
   Check (Report.Outcome = B.Budget_Exhausted and then Calls = 0, "zero budget");
   B.Identify (Service, Device, 1, Report);
   Check (Report.Outcome = B.Budget_Exhausted and then Report.Charged <= 1
     and then C.Retained_Results (Service) = 0, "tiny budget");
   Set_Mode (2);
   B.Identify (Service, Device, 1000, Report);
   declare
      AML_Cost : constant Natural := Report.AML_Charged;
   begin
      Set_Mode (2);
      B.Identify (Service, Device, AML_Cost + 1, Report);
      Check (Report.Outcome = B.Budget_Exhausted and then Report.IDs_Examined = 1
        and then Report.Charged = AML_Cost + 1 and then C.Retained_Results (Service) = 0,
        "host scan shares budget and releases partial walk");
   end;
   Set_Mode (0); Overcharge := True;
   B.Identify (Service, Device, 1000, Report);
   Check (Report.Outcome = B.Provider_Contract_Error and then Report.Charged = 0
     and then Report.AML_Charged = Natural'Last and then C.Retained_Results (Service) = 0,
     "overcharge recorded without overflow and result released");
   Overcharge := False;
   for Held in 7 .. 8 loop
      Set_Mode (2);
      declare
         Handles : array (1 .. Held) of C.Values.Value_Handle;
         R : C.Values.Result;
         Status : C.Values.Access_Status;
      begin
         for H of Handles loop
            C.Create_Empty_Package_Result (Service, R, Status);
            Check (Status = C.Values.Available and then R.Status = AML_Execute.Object_Returned, "hold root");
            H := R.Handle;
         end loop;
         B.Identify (Service, Device, 1000, Report);
         Check (Report.Outcome = B.Evaluation_Failed and then C.Retained_Results (Service) = Held,
           "capacity refusal preserves existing roots");
         for H of Handles loop C.Release_Result (Service, H, Status); Check (Status = C.Values.Available, "release root"); end loop;
      end;
   end loop;
   Check (C.Retained_Results (Service) = 0, "final roots");
   Ada.Text_IO.Put_Line ("HID I2C identity checks" & Checks'Image);
end HID_Identity_Tests;
