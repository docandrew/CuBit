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
with AML_Field_Access;
with AML_Field_Layout;
with Firmware_Tables;
procedure HID_Query_Tests is
   package C is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 64,
      Aggregate_Method_Capacity => 4096, Retained_Result_Capacity => 8);
   package Q is new ACPI_HID_I2C (C);
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type C.Values.Value_Kind;
   use type C.Values.Argument_Kind;
   use type AML_Execute.Execution_Status;
   use type AML_Decode.Integer_Value;
   use type AML_Decode.Bytes;
   use type Q.Query_Outcome;
   use type Q.Descriptor_Register;
   Service, Other : aliased C.State (1, 4096, 4096);
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Label_Text : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Label_Text; end if;
   end Check;
   type Context is record
      Is_Pending : Boolean := False;
      Calls : Natural := 0;
   end record;
   IO : Context;
   function Pending (Item : Context) return Boolean is (Item.Is_Pending);
   procedure Describe (Item : Context; Target : AML_Field_Access.Target;
     Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean) is
      pragma Unreferenced (Item, Target);
   begin Geometry := (others => <>); Accepted := False; end Describe;
   procedure Dispatch (Item : in out Context; Request : AML_Field_Access.Request;
     Reply : out AML_Field_Access.Response) is
      pragma Unreferenced (Request);
   begin Item.Calls := Item.Calls + 1; Reply := (others => <>); end Dispatch;
   procedure Confirm (Item : Context; Expected : AML_Field_Access.Observation; Accepted : out Boolean) is
      pragma Unreferenced (Item, Expected);
   begin Accepted := False; end Confirm;
   procedure Authorize (Item : Context; Target : AML_Field_Access.Target;
     For_Write : Boolean; Accepted : out Boolean) is
      pragma Unreferenced (Item, Target, For_Write);
   begin Accepted := False; end Authorize;
   package F is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirm, Authorize_Target => Authorize);
   Bindings : F.Binding_Set;
   Invokes : Natural := 0;
   Budgets, Charges : array (1 .. 2) of Natural := [others => 0];
   procedure Invoke_Bound
     (S : aliased in out C.State; Node : C.Namespace.Node_ID;
      Args : C.Values.Arguments; Count : C.Values.Argument_Count;
      Budget : Natural; Result : out C.Values.Result; Status : out C.Values.Access_Status) is
      Description : C.Values.Value_Description;
      Data : AML_Decode.Bytes (1 .. 16);
      Copied : Natural;
   begin
      Check (Invokes < 2 and then Count = 4, "bounded fixed invocation");
      Invokes := Invokes + 1; Budgets (Invokes) := Budget;
      Check (Args (0).Kind = C.Values.Retained_Argument and then
        Args (1).Kind = C.Values.Immediate_Argument and then Args (1).Number = 1 and then
        Args (2).Kind = C.Values.Immediate_Argument and then Args (2).Number = AML_Decode.Integer_Value (Invokes - 1)
        and then Args (3).Kind = C.Values.Retained_Argument, "protocol arguments");
      C.Read_Result_Bytes (S, Args (0).Handle, 0, Data, Copied, Status);
      Check (Status = C.Values.Available and then Copied = 16 and then Data =
        [16#F7#,16#F6#,16#DF#,16#3C#,16#67#,16#42#,16#55#,16#45#,
         16#AD#,16#05#,16#B3#,16#0A#,16#3D#,16#89#,16#38#,16#DE#], "UUID buffer");
      C.Describe_Result (S, Args (3).Handle, Description, Status);
      Check (Status = C.Values.Available and then Description.Kind = C.Values.Package_Description
        and then Description.Length = 0, "empty argument package");
      if Invokes = 2 then Check (Budget = Budgets (1) - Charges (1), "shared remaining budget"); end if;
      F.Invoke_With_Arguments (S, IO, Bindings, Node, Args, Count, Budget, Result, Status);
      Charges (Invokes) := Result.Charged;
   end Invoke_Bound;
   package B is new ACPI_HID_I2C (C, Invoke_Bound);
   use type B.Query_Outcome;
   use type B.Descriptor_Register;
   Report : B.Query_Report;
   Plain : Q.Query_Report;
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
      Check (Status = C.Values.Available and then R.Status = AML_Execute.No_Return, "set mode");
      Invokes := 0; Budgets := [others => 0]; Charges := [others => 0];
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
   F.Clear (Bindings);
   Device := Find (Service, "TPD0"); Missing := Find (Service, "NDSM");
   Foreign := Find (Other, "TPD0"); Setter := Find (Service, "SETM");
   Set_Mode (0);
   Q.Query_Descriptor (Service, Device, 1000, Plain);
   Check (Plain.Outcome = Q.Descriptor_Available and then Plain.Address = 16#1234#,
     "default invocation path");
   Set_Mode (0);
   B.Query_Descriptor (Service, Find (Service, "ALDM"), 1000, Report);
   Check (Report.Outcome = B.Descriptor_Available and then Report.Address = 16#1234#,
     "aliased device-local DSM" & Report.Outcome'Image);
   for Mode in 0 .. 12 loop
      Set_Mode (Mode);
      B.Query_Descriptor (Service, Device, (if Mode = 8 then 100 else 1000), Report);
      case Mode is
         when 0 | 12 => Check (Report.Outcome = B.Descriptor_Available and then Report.Address = 16#1234#, "normal address with fresh inputs");
         when 1 | 2 | 4 | 11 =>
            Check (Report.Outcome = B.Function_Unsupported and then Invokes = 1 and then Number ("CNT1") = 0, "unsupported skips descriptor call");
         when 3 | 5 | 7 => Check (Report.Outcome = B.Wrong_Result_Type, "typed response required");
         when 6 => Check (Report.Outcome = B.Invalid_Register, "no address narrowing");
         when 8 => Check (Report.Outcome = B.Budget_Exhausted and then Report.Charged <= 100, "bounded method");
         when 9 => Check (Report.Outcome = B.Descriptor_Available and then Report.Address = B.Descriptor_Register'Last, "wide support buffer and maximum register");
         when 10 => Check (Report.Outcome = B.Descriptor_Available and then Report.Address = 0, "zero register is explicit success");
      end case;
      Check (C.Retained_Results (Service) = 0 and then Report.Charged <= 1000, "temporary results released");
      Check (Report.Outcome = B.Descriptor_Available or else Report.Address = 0, "no failure address");
      Check (Number ("RCNT") = 0 and then IO.Calls = 0, "no ancestor method or hardware");
   end loop;
   Set_Mode (0);
   B.Query_Descriptor (Service, Missing, 1000, Report);
   Check (Report.Outcome = B.Missing_DSM and then Invokes = 0 and then Number ("RCNT") = 0, "no ancestor fallback");
   B.Query_Descriptor (Service, Foreign, 1000, Report);
   Check (Report.Outcome = B.Invalid_Device and then Invokes = 0, "current service device required");
   B.Query_Descriptor (Service, Setter, 1000, Report);
   Check (Report.Outcome = B.Invalid_Device and then Invokes = 0, "device kind required");
   B.Query_Descriptor (Service, Device, 0, Report);
   Check (Report.Outcome = B.Budget_Exhausted and then Invokes = 0, "zero budget");
   B.Query_Descriptor (Service, Device, 1, Report);
   Check (Report.Outcome = B.Budget_Exhausted and then Invokes = 1 and then Report.Charged <= 1, "tiny budget");
   for Held in 6 .. 8 loop
      Set_Mode (0);
      declare
         Handles : array (1 .. Held) of C.Values.Value_Handle;
         R : C.Values.Result;
         Status : C.Values.Access_Status;
      begin
         for I in Handles'Range loop
            C.Create_Empty_Package_Result (Service, R, Status);
            Check (Status = C.Values.Available and then R.Status = AML_Execute.Object_Returned, "hold capacity");
            Handles (I) := R.Handle;
         end loop;
         B.Query_Descriptor (Service, Device, 1000, Report);
         Check (Report.Outcome = (if Held = 6 then B.Evaluation_Failed else B.Allocation_Failed)
           and then C.Retained_Results (Service) = Held, "quota cleanup keeps caller roots");
         for I in Handles'Range loop
            C.Release_Result (Service, Handles (I), Status); Check (Status = C.Values.Available, "release held root");
         end loop;
         Check (Number ("CNT0") = 0 and then Number ("CNT1") = 0, "capacity refusal before AML effects");
      end;
   end loop;
   Check (C.Retained_Results (Service) = 0, "final roots");
   Ada.Text_IO.Put_Line ("HID I2C query checks" & Checks'Image);
end HID_Query_Tests;
