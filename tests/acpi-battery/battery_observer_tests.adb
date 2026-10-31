pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with ACPI_Service_Core;
with ACPI_Battery;
with ACPI_Battery_Status;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with Firmware_Tables;
procedure Battery_Observer_Tests is
   package C is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 128,
      Aggregate_Method_Capacity => 4096, Retained_Result_Capacity => 8);
   package Q is new ACPI_Battery (C);
   package S renames ACPI_Battery_Status;
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type C.Values.Value_Handle;
   use type C.Namespace.Lookup_Status;
   use type AML_Execute.Execution_Status;
   use type AML_Decode.Integer_Value;
   use type S.Direction;
   use type S.Quantity_Value;
   use type S.Sample;
   use type S.Field;
   use type S.Outcome;
   use type Q.Query_Outcome;
   use type Q.Identification_Outcome;
   use type Q.Presence_Outcome;
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
   package B is new ACPI_Battery (C, Evaluate);
   use type B.Query_Outcome;
   use type B.Identification_Outcome;
   use type B.Presence_Outcome;
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
   Presence : B.Presence_Report;
   Plain_Presence : Q.Presence_Report;
   procedure Set_Presence (Mode : Natural) is
      Value : C.Values.Result;
      Status : C.Values.Access_Status;
      Node : constant C.Node_Descriptor := Find (Service, "SETP");
   begin
      C.Invoke_Retained (Service, C.Values.Position (Node),
        [0 => AML_Decode.Integer_Value (Mode), others => 0],
        1, 1000, Value, Status);
      Check (Status = C.Values.Available and Value.Status = AML_Execute.No_Return, "set presence mode");
      Calls := 0;
   end Set_Presence;
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
   Baseline_Charge : Natural;
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
   Device := Find (Service, "BAT0"); Setter := Find (Service, "SETM"); Foreign := Find (Other, "BAT0");
   Q.Query (Service, Device, 1000, Plain);
   Check (Plain.Outcome = Q.Sampled and Plain.Value.Flow = S.Charging
     and Plain.Value.Charge_Limiting, "default battery charge direction");
   Check (Plain.Value.Rate.Value = 1234 and Plain.Value.Remaining_Capacity.Value = 5678
     and Plain.Value.Voltage.Value = 11000, "ACPICA dynamic values match");
   Check (Plain.Fields_Read = 4 and Plain.Charged >= 4, "package reading charged");
   Q.Identify (Service, Device, 1000, Plain_ID);
   Check (Plain_ID.Outcome = Q.Matched, "compressed EISA identity");
   Q.Query_Presence (Service, Device, 1000, Plain_Presence);
   Check (Plain_Presence.Outcome = Q.Observed and Plain_Presence.Device_Present
     and Plain_Presence.Battery_Present and Plain_Presence.Enabled
     and Plain_Presence.Functioning and not Plain_Presence.Defaulted, "default presence callback");
   Baseline_Charge := Plain.Charged;
   for Mode in 0 .. 10 loop
      Set_Mode (Mode, 0);
      B.Query (Service, Device, (if Mode = 6 then 100 else 1000), Report);
      case Mode is
         when 0 => Check (Report.Outcome = B.Sampled and Report.Value.Flow = S.Charging
           and Report.Value.Charge_Limiting, "valid battery");
         when 1 => Check (Report.Outcome = B.Invalid_Package_Value
           and Report.Decode_Status = S.Conflicting_Directions
           and Report.Failure_Field = S.State_Field, "contradictory directions");
         when 2 | 10 => Check (Report.Outcome = B.Wrong_Element_Count
           and Report.Elements = (if Mode = 2 then 3 else 5) and Report.Fields_Read = 0, "exact package shape");
         when 3 => Check (Report.Outcome = B.Wrong_Element_Type and Report.Fields_Read = 2
           and Report.Failure_Field = S.Capacity_Field, "element type no coercion");
         when 4 => Check (Report.Outcome = B.Invalid_Package_Value
           and Report.Decode_Status = S.Invalid_Measurement
           and Report.Failure_Field = S.Rate_Field, "reserved quantity range");
         when 5 => Check (Report.Outcome = B.Invalid_Package_Value
           and Report.Decode_Status = S.Integer_Too_Wide
           and Report.Failure_Field = S.Voltage_Field and Report.Raw (4) = 16#1_0000_0000#, "wide integer not narrowed");
         when 6 => Check (Report.Outcome = B.Budget_Exhausted, "bounded looping method");
         when 7 | 8 => Check (Report.Outcome = B.Wrong_Result_Type, "nonpackage not coerced");
         when 9 => Check (Report.Outcome = B.Sampled and not Report.Value.Rate.Known
           and not Report.Value.Remaining_Capacity.Known and not Report.Value.Voltage.Known
           and Report.Value.Charge_Limiting, "unknown measurements preserved");
      end case;
      if Report.Outcome /= B.Sampled then Check (Report.Value = S.Empty_Sample, "refusal discards sample"); end if;
      Check (Calls = 1 and Report.Charged <= 1000 and C.Retained_Results (Service) = 0, "query ownership/budget");
      Check (Number ("RCNT") = 0, "no ancestor execution");
   end loop;
   for Mode in 0 .. 6 loop
      Set_Mode (0, Mode);
      B.Identify (Service, Device, (if Mode = 5 then 100 else 1000), ID);
      case Mode is
         when 0 | 1 => Check (ID.Outcome = B.Matched, "integer/string identity");
         when 2 | 4 | 6 => Check (ID.Outcome = B.Not_Battery, "nonmatching identity");
         when 3 => Check (ID.Outcome = B.Wrong_HID_Type, "buffer HID not coerced");
         when 5 => Check (ID.Outcome = B.Budget_Exhausted, "bounded HID method");
      end case;
      Check (Calls = 1 and ID.Charged <= 1000 and C.Retained_Results (Service) = 0, "identity cleanup/budget");
   end loop;
   for I in 1 .. 3 loop
      declare Node : constant C.Node_Descriptor := Find (Service, (if I = 1 then "NAM0" elsif I = 2 then "ALS0" else "ALH0")); begin
         B.Identify (Service, Node, 1000, ID); Check (ID.Outcome = B.Matched, "named/aliased HID");
         B.Query (Service, Node, 1000, Report);
         Check (Report.Outcome = B.Sampled and Report.Value.Flow = S.Discharging
           and Report.Value.Remaining_Capacity.Value = 200, "named/aliased battery package");
      end;
   end loop;
   B.Query (Service, Find (Service, "MIS0"), 1000, Report);
   Check (Report.Outcome = B.Missing_BST and Number ("RCNT") = 0, "missing no ancestor");
   B.Identify (Service, Find (Service, "MIH0"), 1000, ID);
   Check (ID.Outcome = B.Missing_HID and Number ("RCNT") = 0, "missing HID no ancestor");
   Calls := 0;
   B.Query (Service, Foreign, 1000, Report); Check (Report.Outcome = B.Invalid_Device and Calls = 0, "foreign query");
   B.Identify (Service, Foreign, 1000, ID); Check (ID.Outcome = B.Invalid_Device and Calls = 0, "foreign identity");
   B.Query (Service, Setter, 1000, Report); Check (Report.Outcome = B.Invalid_Device and Calls = 0, "nondevice query");
   B.Query (Service, Device, 0, Report); Check (Report.Outcome = B.Budget_Exhausted and Calls = 0, "zero budget query");
   B.Identify (Service, Device, 0, ID); Check (ID.Outcome = B.Budget_Exhausted and Calls = 0, "zero budget identity");
   Set_Mode (0, 1);
   --  Same deterministic AML method cost, then four individually charged host
   --  element reads. Every partial observation releases both element and root.
   for Extra in Natural range 0 .. 4 loop
      declare Budget : constant Natural := Baseline_Charge - 4 + Extra; begin
         B.Query (Service, Device, Budget, Report);
         Check (Report.Charged = Budget and Report.Fields_Read = Extra, "per-element budget boundary");
         Check (Report.Outcome = (if Extra = 4 then B.Sampled else B.Budget_Exhausted), "budget completion boundary");
         Check (C.Retained_Results (Service) = 0, "partial budget cleanup");
      end;
   end loop;
   Overcharge := True;
   B.Query (Service, Device, 1000, Report);
   Check (Report.Outcome = B.Provider_Contract_Error and Report.Charged = 0
     and Report.Last_Charge = 1001 and Report.Value = S.Empty_Sample, "overcharge no sample");
   Check (C.Retained_Results (Service) = 0, "overcharged package released");
   B.Identify (Service, Device, 1000, ID);
   Check (ID.Outcome = B.Provider_Contract_Error and ID.Charged = 0
     and ID.Last_Charge = 1001 and C.Retained_Results (Service) = 0, "overcharged HID string released");
   Overcharge := False;
   for Mode in 0 .. 7 loop
      Set_Presence (Mode);
      B.Query_Presence (Service, Device, (if Mode = 6 then 100 else 1000), Presence);
      case Mode is
         when 0 | 4 => Check (Presence.Outcome = B.Observed
           and Presence.Device_Present and Presence.Battery_Present
           and Presence.Enabled and Presence.Functioning
           and Presence.Flags = (if Mode = 0 then 16#1F# else 16#1_0000_001F#), "present and full width flags");
         when 1 => Check (Presence.Outcome = B.Observed and Presence.Device_Present
           and not Presence.Battery_Present, "device exists battery absent");
         when 2 => Check (Presence.Outcome = B.Observed and not Presence.Device_Present
           and Presence.Battery_Present and not Presence.Enabled
           and not Presence.Functioning, "separate raw battery presence");
         when 3 => Check (Presence.Outcome = B.Observed and not Presence.Device_Present
           and not Presence.Battery_Present and Presence.Functioning, "descend-only flags");
         when 5 | 7 => Check (Presence.Outcome = B.Wrong_Result_Type, "status no coercion");
         when 6 => Check (Presence.Outcome = B.Budget_Exhausted, "bounded presence method");
      end case;
      if Presence.Outcome /= B.Observed then
         Check (not Presence.Device_Present and not Presence.Battery_Present
           and not Presence.Defaulted and Presence.Flags = 0, "failure unknown not observed absent");
      end if;
      Check (Calls = 1 and Presence.Charged <= 1000
        and C.Retained_Results (Service) = 0, "presence budget/cleanup");
      Check (Number ("RCNT") = 0, "presence no ancestor");
   end loop;
   Calls := 0;
   B.Query_Presence (Service, Find (Service, "NAM0"), 1000, Presence);
   Check (Presence.Outcome = B.Observed and Presence.Defaulted and Calls = 0
     and Presence.Flags = 16#FFFF_FFFF# and Presence.Battery_Present, "missing STA uses specified default");
   Calls := 0;
   B.Query_Presence (Service, Foreign, 1000, Presence);
   Check (Presence.Outcome = B.Invalid_Device and Calls = 0, "foreign presence");
   B.Query_Presence (Service, Setter, 1000, Presence);
   Check (Presence.Outcome = B.Invalid_Device and Calls = 0, "nondevice presence");
   B.Query_Presence (Service, Device, 0, Presence);
   Check (Presence.Outcome = B.Budget_Exhausted and Calls = 0, "zero budget presence");
   Set_Presence (7); Overcharge := True;
   B.Query_Presence (Service, Device, 1000, Presence);
   Check (Presence.Outcome = B.Provider_Contract_Error and Presence.Charged = 0
     and Presence.Last_Charge = 1001 and not Presence.Battery_Present
     and C.Retained_Results (Service) = 0, "overcharge releases presence object");
   Overcharge := False;
   --  Existing retained results must survive sampling. Six occupied slots
   --  permit root+element; seven force an element-retain failure; eight force
   --  a root-retain failure. The observer must release only its own handles.
   for Keep in Natural range 6 .. 8 loop
      declare
         Handles : array (1 .. Keep) of C.Values.Value_Handle := [others => C.Values.No_Value];
         Found : C.Namespace.Lookup_Result;
         Node : C.Node_Descriptor;
         Value : C.Values.Result;
         Status : C.Values.Access_Status;
         Parent : constant C.Node_Descriptor := Find (Service, "NAM0");
      begin
         C.Find_Object (Service, C.Values.Position (Parent), "_BST", Found, Status);
         Check (Status = C.Values.Available and Found.Status = C.Namespace.Found, "retain fixture lookup");
         C.Describe_Node (Service, Found.Node, Node, Status);
         Check (Status = C.Values.Available, "retain fixture descriptor");
         for I in Handles'Range loop
            C.Evaluate_Named (Service, Node, 1000, Value, Status);
            Check (Status = C.Values.Available and Value.Status = AML_Execute.Object_Returned, "retain existing package");
            Handles (I) := Value.Handle;
         end loop;
         Check (C.Retained_Results (Service) = Keep, "existing handles counted");
         B.Query (Service, Device, 1000, Report);
         Check ((Report.Outcome = B.Sampled) = (Keep = 6), "retained capacity refusal");
         Check (C.Retained_Results (Service) = Keep, "existing handles preserved");
         for I in Handles'Range loop
            C.Release_Result (Service, Handles (I), Status);
            Check (Status = C.Values.Available and Handles (I) = C.Values.No_Value, "existing handle still releasable");
         end loop;
         Check (C.Retained_Results (Service) = 0, "capacity scenario cleanup");
      end;
   end loop;
   Check (Number ("RCNT") = 0 and C.Retained_Results (Service) = 0, "final ownership");
   Ada.Text_IO.Put_Line ("ACPI battery observer checks" & Checks'Image);
end Battery_Observer_Tests;
