pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with ACPI_Service_Core;
with ACPI_HID_I2C_Discovery;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with Firmware_Tables;
procedure HID_Discovery_Tests is
   package C is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 128,
      Aggregate_Method_Capacity => 4096, Retained_Result_Capacity => 8);
   package D is new ACPI_HID_I2C_Discovery (C);
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Values.Access_Status;
   use type C.Values.Value_Handle;
   use type C.Values.Module_Outcome;
   use type AML_Execute.Execution_Status;
   use type D.Action_Result;
   use type D.Outcome;
   use type D.Stage;
   use type D.Reader.Copy_Status;
   use type D.Queries.Descriptor_Register;
   use type D.Queries.Call_Count;
   use type D.Resources.R.Byte_Count;
   use type Firmware_Tables.Byte;
   Service, Other : aliased C.State (1, 4096, 4096, 1);
   Run : D.Session;
   Action : D.Action_Result;
   Info : D.Report;
   Checks, Prefix_Charge : Natural := 0;
   procedure Check (OK : Boolean; Label_Text : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Label_Text; end if;
   end Check;
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
      Staged : C.Stage_Status;
      Frozen : C.Freeze_Status;
      Execution : C.Execution_Report;
      Members : C.Namespace.Initialization_Report;
      Status : C.Values.Access_Status;
   begin
      C.Stage_Table (State, 1, C.DSDT, Data, Staged); Check (Staged = C.Staged, "stage");
      C.Freeze_Catalog (State, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze");
      C.Execute_Frozen (State, 1000, Execution, Status);
      Check (Status = C.Values.Available and then Execution.Modules.Outcome = C.Values.Modules_Completed, "modules");
      C.Initialize_Members (State, Members, Status); Check (Status = C.Values.Available, "members");
   end Prepare;
   Device, Setter, Foreign : C.Node_Descriptor;
   procedure Set_Mode (Mode : Natural) is
      Value : C.Values.Result;
      Status : C.Values.Access_Status;
   begin
      C.Invoke_Retained (Service, C.Values.Position (Setter), [0 => AML_Decode.Integer_Value (Mode), others => 0], 1, 1000, Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.No_Return, "set mode");
   end Set_Mode;
   function Number (Name : String) return Natural is
      Value : C.Values.Result;
      Status : C.Values.Access_Status;
   begin
      C.Evaluate_Named (Service, Find (Service, Name), 1000, Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.Returned, "counter");
      return Natural (Value.Number);
   end Number;
   Small : Firmware_Tables.Bytes (1 .. 1);
   Data : Firmware_Tables.Bytes (Natural'Last - 511 .. Natural'Last);
begin
   Check (Ada.Command_Line.Argument_Count = 1, "fixture path");
   declare
      Input : Ada.Streams.Stream_IO.File_Type;
      Stream : Ada.Streams.Stream_Element_Array (1 .. 4096);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Stream_IO.Open (Input, Ada.Streams.Stream_IO.In_File, Ada.Command_Line.Argument (1));
      Ada.Streams.Stream_IO.Read (Input, Stream, Last); Ada.Streams.Stream_IO.Close (Input);
      declare Bytes : Firmware_Tables.Bytes (1 .. Natural (Last)); begin
         for I in Bytes'Range loop Bytes (I) := Firmware_Tables.Byte (Stream (Ada.Streams.Stream_Element_Offset (I))); end loop;
         Prepare (Service, Bytes); Prepare (Other, Bytes);
      end;
   end;
   Device := Find (Service, "TPD0"); Setter := Find (Service, "SETM"); Foreign := Find (Other, "TPD0");
   for Mode in 0 .. 8 loop
      Set_Mode (Mode);
      D.Begin_Discovery (Run, Service, Device, 1000, Action);
      Info := D.Observe (Run);
      Check (Action = D.Accepted and then D.Pending (Run) and then Info.Result = D.Awaiting_Buffer
        and then C.Retained_Results (Service) = 1, "retained resource evaluation");
      declare Before_Charge : constant Natural := Info.Charged; begin
         D.Begin_Discovery (Run, Other, Foreign, 1000, Action);
         Check (Action = D.Busy and then D.Observe (Run).Charged = Before_Charge, "busy preserves owner");
         D.Copy_And_Finish (Run, Other, Small, Action);
         Check (Action = D.Wrong_Context and then Small (1) = 0 and then D.Pending (Run)
           and then C.Retained_Results (Service) = 1 and then C.Retained_Results (Other) = 0,
           "foreign copy preserves original context");
         D.Cancel (Run, Other, Action);
         Check (Action = D.Wrong_Context and then D.Pending (Run), "foreign cancel preserves original context");
         for Attempt in 1 .. 2 loop
            D.Copy_And_Finish (Run, Service, Small, Action); Info := D.Observe (Run);
            Check (Action = D.Accepted and then Info.Result = D.Awaiting_Buffer
              and then Info.Copy.Status = D.Reader.Insufficient_Capacity
              and then Info.Copy.Required > 1 and then Info.Charged = Before_Charge
              and then C.Retained_Results (Service) = 1 and then Small (1) = 0,
              "copy sizing retry without evaluation");
         end loop;
      end;
      D.Copy_And_Finish (Run, Service, Data, Action); Info := D.Observe (Run);
      Check (Action = D.Accepted and then not D.Pending (Run) and then C.Retained_Results (Service) = 0,
        "finish releases resource ownership");
      if Mode in 0 | 5 | 8 then
         Check (Info.Result = D.Observed and then Info.Step = D.Finished
           and then Info.Descriptor.Address = 16#20# and then Info.Charged <= 1000,
           "typed discovery observations");
         Check (Info.Status.Enabled = (Mode /= 8), "disabled status remains separate from observations");
         if Mode = 0 then Prefix_Charge := Info.Charged - Info.Descriptor.Charged; end if;
      else
         Check (Info.Result = D.Analysis_Failed and then Info.Descriptor.Calls = 0,
           "resource alternatives or missing/duplicate connections stop descriptor query");
      end if;
      Check (Number ("CALL") = 1 and then Number ("RCNT") = 0, "one CRS evaluation and no ancestor fallback");
      Check ((for all I in Data'Range =>
        (if D.Resources.R.Byte_Count (I - Data'First) >= Info.Copy.Copied then Data (I) = 0)),
        "copied exact prefix with zero suffix at high bounds");
      D.Copy_And_Finish (Run, Service, Small, Action);
      Check (Action = D.Not_Pending and then Small (1) = 0, "no replay after terminal observation");
   end loop;
   Set_Mode (0);
   D.Begin_Discovery (Run, Service, Device, 1000, Action);
   D.Cancel (Run, Service, Action);
   Check (Action = D.Accepted and then D.Observe (Run).Result = D.Cancelled
     and then not D.Pending (Run) and then C.Retained_Results (Service) = 0, "cancel releases original result");
   D.Cancel (Run, Service, Action); Check (Action = D.Not_Pending, "cancel does not repeat cleanup");
   D.Begin_Discovery (Run, Service, Foreign, 1000, Action);
   Check (D.Observe (Run).Result = D.Invalid_Device and then not D.Pending (Run), "foreign device");
   D.Begin_Discovery (Run, Service, Device, 0, Action);
   Check (D.Observe (Run).Result = D.Budget_Exhausted and then not D.Pending (Run), "zero shared budget");
   D.Begin_Discovery (Run, Service, Find (Service, "MRES"), 1000, Action);
   Check (D.Observe (Run).Result = D.Missing_Resources and then not D.Pending (Run)
     and then Number ("RCNT") = 0, "missing CRS does not evaluate ancestor");
   D.Begin_Discovery (Run, Service, Find (Service, "ALRS"), 1000, Action);
   D.Copy_And_Finish (Run, Service, Data, Action);
   Check (D.Observe (Run).Result = D.Observed and then not D.Pending (Run), "canonical aliased resource buffer");
   declare
      Value : C.Values.Result;
      Held : C.Values.Value_Handle;
      Status : C.Values.Access_Status;
   begin
      Set_Mode (0);
      C.Create_Empty_Package_Result (Service, Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.Object_Returned, "caller-owned root");
      Held := Value.Handle;
      D.Begin_Discovery (Run, Service, Device, 1000, Action);
      Check (D.Pending (Run) and then C.Retained_Results (Service) = 2, "session adds exactly its own root");
      D.Copy_And_Finish (Run, Service, Data, Action);
      Check (D.Observe (Run).Result = D.Observed and then C.Retained_Results (Service) = 1,
        "finish preserves independent caller ownership");
      C.Release_Result (Service, Held, Status);
      Check (Status = C.Values.Available and then Held = C.Values.No_Value, "release caller root");
   end;
   Set_Mode (0);
   D.Begin_Discovery (Run, Service, Device, Prefix_Charge, Action);
   Check (D.Pending (Run), "resource result retained at exact prefix budget");
   D.Copy_And_Finish (Run, Service, Data, Action); Info := D.Observe (Run);
   Check (Info.Result = D.Budget_Exhausted and then Info.Charged = Prefix_Charge
     and then Info.Descriptor.Calls = 0 and then not D.Pending (Run), "descriptor shares remaining budget");
   Check (C.Retained_Results (Service) = 0 and then C.Retained_Results (Other) = 0, "final roots");
   Ada.Text_IO.Put_Line ("HID I2C discovery checks" & Checks'Image);
end HID_Discovery_Tests;
