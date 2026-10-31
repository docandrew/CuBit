pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with ACPI_Service_Core;
with ACPI_HID_I2C;
with ACPI_Device_Policy;
with AML_Delays;
with AML_Execute;
with Firmware_Tables;
with Interfaces;
procedure HID_Status_Tests is
   package C is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 128, Aggregate_Method_Capacity => 4096,
      Retained_Result_Capacity => 8);
   package Q is new ACPI_HID_I2C (C);
   Service, Other : aliased C.State (1, 4096, 4096, 1);
   Checks, Calls : Natural := 0;
   Overcharge : Boolean := False;
   Reject_Execution : Boolean := False;
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type AML_Execute.Execution_Status;
   use type Q.Status_Outcome;
   use type Interfaces.Unsigned_64;
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
      if Reject_Execution and then Result.Status = AML_Execute.Returned then
         Result := (Status => AML_Execute.Unsupported, Charged => Result.Charged);
      end if;
      if Overcharge and then Budget < Natural'Last then Result.Charged := Budget + 1; end if;
   end Evaluate;
   package B is new ACPI_HID_I2C (C, Evaluate_Named => Evaluate);
   use type B.Status_Outcome;
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
   Report : B.Status_Report;
   Plain : Q.Status_Report;
   procedure Set_Mode (Mode : Natural) is
      Value : C.Values.Result;
      Status : C.Values.Access_Status;
   begin
      C.Invoke_Retained (Service, C.Values.Position (Setter), [0 => Interfaces.Unsigned_64 (Mode), others => 0], 1, 1000, Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.No_Return, "set mode");
      Calls := 0;
   end Set_Mode;
   function Number (Name : String) return Interfaces.Unsigned_64 is
      Value : C.Values.Result;
      Status : C.Values.Access_Status;
   begin
      C.Evaluate_Named (Service, Find (Service, Name), 1000, Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.Returned, "counter");
      return Interfaces.Unsigned_64 (Value.Number);
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
   Device := Find (Service, "STA0"); Setter := Find (Service, "SETM");
   Foreign := Find (Other, "STA0");
   Q.Observe_Status (Service, Device, 1000, Plain);
   Check (Plain.Outcome = Q.Status_Available and then Plain.Present and then Plain.Enabled
     and then Plain.Functioning and then not Plain.Defaulted, "default adapter");
   for Mode in 0 .. 8 loop
      Set_Mode (Mode);
      B.Observe_Status (Service, Device, (if Mode = 7 then 100 else 1000), Report);
      case Mode is
         when 0 | 1 | 2 | 3 | 8 =>
            Check (Report.Outcome = B.Status_Available, "status outcome" & Mode'Image);
            Check (Report.Flags = (case Mode is when 0 => 16#0F#, when 1 => 16#0D#,
              when 2 => 8, when 3 => 0, when others => 16#10000000F#), "raw status without narrowing");
            Check (Report.Present = (Mode in 0 | 1 | 8)
              and Report.Enabled = (Mode in 0 | 8)
              and Report.Functioning = (Mode /= 3), "independent status flags");
         when 4 | 5 | 6 => Check (Report.Outcome = B.Wrong_Result_Type, "typed status");
         when 7 => Check (Report.Outcome = B.Budget_Exhausted, "bounded status evaluation");
      end case;
      Check (Calls = 1 and then Report.Charged <= 1000 and then not Report.Defaulted, "one evaluation and budget");
      Check (C.Retained_Results (Service) = 0, "temporary cleanup");
      Check (Number ("RCNT") = 0, "no ancestor evaluation");
   end loop;
   Calls := 0;
   B.Observe_Status (Service, Find (Service, "MISS"), 1000, Report);
   Check (Report.Outcome = B.Status_Available and Report.Defaulted
     and Report.Flags = ACPI_Device_Policy.Missing_Status_Default and Calls = 0,
     "missing status default without ancestor evaluation");
   B.Observe_Status (Service, Find (Service, "NAM0"), 1000, Report);
   Check (Report.Outcome = B.Status_Available and Report.Flags = 16#0D#
     and Report.Present and not Report.Enabled and Report.Functioning, "named status");
   B.Observe_Status (Service, Find (Service, "ALIA"), 1000, Report);
   Check (Report.Outcome = B.Status_Available and Report.Flags = 16#0F#, "aliased status");
   Calls := 0;
   B.Observe_Status (Service, Foreign, 1000, Report);
   Check (Report.Outcome = B.Invalid_Device and Calls = 0, "foreign device");
   B.Observe_Status (Service, Setter, 1000, Report);
   Check (Report.Outcome = B.Invalid_Device and Calls = 0, "non-device");
   B.Observe_Status (Service, Device, 0, Report);
   Check (Report.Outcome = B.Budget_Exhausted and Calls = 0, "zero budget");
   Set_Mode (0); Overcharge := True;
   B.Observe_Status (Service, Device, 1000, Report);
   Check (Report.Outcome = B.Provider_Contract_Error and Report.Charged = 0
     and Report.Last_Charge = 1001 and Report.Flags = 0 and not Report.Present
     and not Report.Enabled and not Report.Functioning and not Report.Defaulted,
     "overcharge preserves accounting and clears observations");
   Overcharge := False;
   Reject_Execution := True;
   B.Observe_Status (Service, Device, 1000, Report);
   Check (Report.Outcome = B.Evaluation_Failed and Report.Execution = AML_Execute.Unsupported
     and Report.Flags = 0 and not Report.Present, "execution failure retains typed reason");
   Reject_Execution := False;
   Check (C.Retained_Results (Service) = 0 and Number ("RCNT") = 0, "final cleanup");
   Ada.Text_IO.Put_Line ("HID I2C status checks" & Checks'Image);
end HID_Status_Tests;
