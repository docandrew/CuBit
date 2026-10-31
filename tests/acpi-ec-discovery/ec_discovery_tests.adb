with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Interfaces;
with ACPI_Service_Core;
with ACPI_EC_Discovery;
with ACPI_EC_Resources;
with ACPI_Resource_Data;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with AML_Names;
with Firmware_Tables;
procedure EC_Discovery_Tests is
   package C is new ACPI_Service_Core
     (AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 256,
      Aggregate_Method_Capacity => 8192, Retained_Result_Capacity => 4);
   use type C.Stage_Status, C.Freeze_Status, C.Values.Access_Status, C.Values.Module_Outcome;
   use type C.Values.Value_Handle, C.Values.Value_Kind;
   use type AML_Execute.Execution_Status, AML_Decode.Integer_Value;
   use type Interfaces.Unsigned_8;
   Service, Other : aliased C.State (1, 8192, 8192, 1);
   Checks, Calls, Releases : Natural := 0;
   Fail_Release : Natural := 0;
   Overcharge, Empty_Handle, Force_Unknown : Boolean := False;
   type Charge_Array is array (Positive range 1 .. 16) of Natural;
   Budgets, Charges : Charge_Array := [others => 0];
   procedure Check (Condition : Boolean) is
   begin Checks := Checks + 1; if not Condition then raise Program_Error with "EC discovery check" & Checks'Image; end if; end Check;
   function Find (State : C.State; Parent : C.Namespace.Node_ID;
                  Name : AML_Names.Segment) return C.Node_Descriptor is
      Node : C.Namespace.Node_ID; Descriptor : C.Node_Descriptor; Status : C.Values.Access_Status;
   begin
      C.Find_Child (State, Parent, Name, Node, Status); Check (Status = C.Values.Available);
      C.Describe_Node (State, Node, Descriptor, Status); Check (Status = C.Values.Available);
      return Descriptor;
   end Find;
   procedure Evaluate
     (State : aliased in out C.State; Item : C.Node_Descriptor;
      Budget : Natural; Result : out C.Values.Result; Status : out C.Values.Access_Status) is
      Handle : C.Values.Value_Handle;
      Released : C.Values.Access_Status;
   begin
      Calls := Calls + 1; Check (Calls <= Budgets'Last); Budgets (Calls) := Budget;
      if Force_Unknown then
         Result := (Status => AML_Execute.Field_Indeterminate, Charged => 1);
         Status := C.Values.Available;
      else
         C.Evaluate_Named (State, Item, Budget, Result, Status);
         if Empty_Handle and then Result.Status = AML_Execute.Object_Returned then
            Handle := Result.Handle; C.Release_Result (State, Handle, Released);
            Check (Released = C.Values.Available and Handle = C.Values.No_Value);
            Result := (Status => AML_Execute.Object_Returned, Charged => Result.Charged, Handle => C.Values.No_Value);
         end if;
      end if;
      if Overcharge then Result.Charged := Budget + 1; end if;
      Charges (Calls) := Result.Charged;
   end Evaluate;
   procedure Release
     (State : in out C.State; Handle : in out C.Values.Value_Handle;
      Status : out C.Values.Access_Status) is
   begin
      Releases := Releases + 1;
      if Releases = Fail_Release then Status := C.Values.Invalid_Value;
      else C.Release_Result (State, Handle, Status); end if;
   end Release;
   package D is new ACPI_EC_Discovery (C, Evaluate, Release);
   use type D.Outcome, D.Source_Kind, D.Action_Result, D.Event_Number;
   use type ACPI_Resource_Data.Byte_Count;
   use type C.Node_Descriptor;
   procedure Reset is
   begin
      Check (C.Retained_Results (Service) = 0 and C.Retained_Results (Other) = 0);
      Calls := 0; Releases := 0; Fail_Release := 0; Overcharge := False;
      Empty_Handle := False; Force_Unknown := False;
      Budgets := [others => 0]; Charges := [others => 0];
   end Reset;
   procedure Budget_Check (Run : D.Session; Initial : Natural) is
      Report : constant D.Report := D.Observe (Run);
   begin
      Check (Report.Initial_Budget = Initial and Report.Charged <= Initial
             and Report.Remaining = Initial - Report.Charged);
      for I in 2 .. Calls loop Check (Budgets (I) = Budgets (I - 1) - Charges (I - 1)); end loop;
   end Budget_Check;
   procedure Prepare (State : aliased in out C.State; Image : Firmware_Tables.Bytes) is
      Stage : C.Stage_Status; Freeze : C.Freeze_Status; Report : C.Execution_Report;
      Members : C.Namespace.Initialization_Report; Status : C.Values.Access_Status;
   begin
      C.Stage_Table (State, 1, C.DSDT, Image, Stage); Check (Stage = C.Staged);
      C.Freeze_Catalog (State, Freeze); Check (Freeze = C.Catalog_Frozen_OK);
      C.Execute_Frozen (State, 1000, Report, Status);
      Check (Status = C.Values.Available and Report.Modules.Outcome = C.Values.Modules_Completed);
      C.Initialize_Members (State, Members, Status); Check (Status = C.Values.Available);
   end Prepare;
   procedure Case_Run (Name : AML_Names.Segment; Expected : D.Outcome;
                       Reduced : Boolean := False; Budget : Natural := 1000;
                       Package_Form : Boolean := False) is
      Run : D.Session; Action : D.Action_Result; Buffer : Firmware_Tables.Bytes (1 .. 1024);
      Meta : D.Metadata; Before : Natural;
   begin
      Reset;
      D.Discover (Run, Service, Find (Service, C.Namespace.Root, Name), Reduced, Budget, Buffer, Action);
      Check (Action = D.Accepted and D.Consumed (Run) and D.Observe (Run).Status = Expected);
      Check (not D.Pending (Run) and C.Retained_Results (Service) = 0);
      Budget_Check (Run, Budget); Meta := D.Description (Run);
      if Expected = D.Described then
         Check (Meta.Device /= C.Values.No_Node and D.Is_Current (Run, Service) and not D.Is_Current (Run, Other));
         if Reduced then Check (Meta.Source = D.GPIO_Interrupt and Meta.GPIO_Present);
         elsif Package_Form then Check (Meta.Source = D.Device_GPE and Meta.Event = 17 and C.Values.Node_Name (Meta.Block_Device) = "GPEB");
         else Check (Meta.Source = D.FADT_GPE); end if;
         if Name = "ECIO" then
            Check (Meta.Lock_Declared and Meta.Lock_Required and Meta.Event = 9);
            Check (Meta.Registers (ACPI_EC_Resources.Data_Register).Address.Address = 16#62#
                   and Meta.Registers (ACPI_EC_Resources.Control_Register).Address.Address = 16#66#);
         elsif Name = "ECFI" then Check (not Meta.Lock_Declared and not Meta.Lock_Required);
         end if;
      else
         Check (Meta.Source = D.No_Source and Meta.Device = C.Values.No_Node and not D.Is_Current (Run, Service));
         Check ((for all B of Buffer => B = 0));
      end if;
      Before := Calls;
      D.Discover (Run, Service, Find (Service, C.Namespace.Root, Name), Reduced, Budget, Buffer, Action);
      Check (Action = D.Already_Consumed and Calls = Before);
      D.Close (Run, Service, Action); Check (Action = D.No_Pending_Result);
   end Case_Run;
begin
   Check (Ada.Command_Line.Argument_Count = 1);
   declare
      File : Ada.Streams.Stream_IO.File_Type;
      Bytes : Ada.Streams.Stream_Element_Array (1 .. 8192); Last : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Ada.Command_Line.Argument (1));
      Ada.Streams.Stream_IO.Read (File, Bytes, Last); Ada.Streams.Stream_IO.Close (File);
      declare Image : Firmware_Tables.Bytes (1 .. Natural (Last)); begin
         for I in Image'Range loop Image (I) := Firmware_Tables.Byte (Bytes (Ada.Streams.Stream_Element_Offset (I))); end loop;
         Prepare (Service, Image); Prepare (Other, Image);
      end;
   end;
   Case_Run ("ECIO", D.Described); Case_Run ("ECFI", D.Described);
   Case_Run ("ECMM", D.Described, Package_Form => True);
   Case_Run ("ECGI", D.Described, Reduced => True);
   Case_Run ("ECGI", D.Missing_Member); Case_Run ("ECIO", D.Unsupported_Platform, Reduced => True);
   Case_Run ("ECFM", D.Unsupported_Platform, Reduced => True);
   Case_Run ("ECXX", D.Resource_Error); Case_Run ("GPEB", D.Not_An_EC);
   Case_Run ("CNT0", D.Invalid_Device);
   Case_Run ("ECIO", D.Budget_Exhausted, Budget => 0);
   Case_Run ("ECIO", D.Budget_Exhausted, Budget => 1);
   Reset;
   declare
      Run : D.Session; Action : D.Action_Result;
      Small : Firmware_Tables.Bytes (37 .. 37);
      Large : Firmware_Tables.Bytes (37 .. 1060);
      Before_Calls, Before_Releases : Natural;
   begin
      D.Discover (Run, Service, Find (Service, C.Namespace.Root, "ECIO"), False, 1000, Small, Action);
      Check (Action = D.Accepted and D.Observe (Run).Status = D.Capacity_Required and D.Pending (Run));
      Check (D.Observe (Run).Required = 18 and C.Retained_Results (Service) = 1);
      Check (D.Description (Run).Source = D.No_Source);
      Before_Calls := Calls; Before_Releases := Releases;
      D.Copy_Resources (Run, Other, Large, Action);
      Check (Action = D.Wrong_Context and Calls = Before_Calls and Releases = Before_Releases and D.Pending (Run));
      D.Copy_Resources (Run, Service, Small, Action);
      Check (Action = D.Accepted and D.Observe (Run).Status = D.Capacity_Required and Calls = Before_Calls);
      D.Copy_Resources (Run, Service, Large, Action);
      Check (Action = D.Accepted and D.Observe (Run).Status = D.Described and Calls = Before_Calls);
      Check (not D.Pending (Run) and C.Retained_Results (Service) = 0 and D.Is_Current (Run, Service));
      Budget_Check (Run, 1000);
      D.Copy_Resources (Run, Service, Large, Action); Check (Action = D.Not_Awaiting_Capacity and Calls = Before_Calls);
   end;
   for Failure in 1 .. 3 loop
      Reset;
      declare
         Run : D.Session; Action : D.Action_Result; Buffer : Firmware_Tables.Bytes (1 .. 1024);
         Before : Natural;
      begin
         if Failure = 1 then Fail_Release := 1;
         elsif Failure = 2 then Fail_Release := 2;
         else Fail_Release := 2;
         end if;
         D.Discover (Run, Service, Find (Service, C.Namespace.Root, (if Failure = 3 then "ECMM" else "ECIO")), False, 1000, Buffer, Action);
         Check (Action = D.Accepted and D.Observe (Run).Status = D.Cleanup_Failed and D.Pending (Run));
         Check (D.Description (Run).Source = D.No_Source and (for all B of Buffer => B = 0));
         Check (C.Retained_Results (Service) = (if Failure = 3 then 2 else 1));
         Before := Releases; D.Close (Run, Other, Action);
         Check (Action = D.Wrong_Context and Releases = Before and D.Pending (Run));
         Before := Calls; D.Close (Run, Service, Action);
         Check (Action = D.Accepted and D.Observe (Run).Status = D.Closed and not D.Pending (Run));
         Check (C.Retained_Results (Service) = 0 and Calls = Before);
      end;
   end loop;
   for Failure in 1 .. 4 loop
      Reset;
      declare Run : D.Session; Action : D.Action_Result; Buffer : Firmware_Tables.Bytes (1 .. 1024); begin
         if Failure = 1 then Overcharge := True;
         elsif Failure = 2 then Empty_Handle := True;
         elsif Failure = 3 then Force_Unknown := True;
         else Force_Unknown := True; Overcharge := True; end if;
         D.Discover (Run, Service, Find (Service, C.Namespace.Root, "ECIO"), False, 1000, Buffer, Action);
         Check (D.Observe (Run).Status = (if Failure = 3 then D.Indeterminate else D.Provider_Contract_Error));
         Check (Calls = 1 and not D.Pending (Run) and C.Retained_Results (Service) = 0);
         Check (D.Description (Run).Source = D.No_Source);
         Check (D.Observe (Run).Field_Uncertainty = (Failure >= 3));
      end;
   end loop;
   Reset;
   declare Run : D.Session; Action : D.Action_Result; Small : Firmware_Tables.Bytes (1 .. 1); Before : Natural; begin
      D.Discover (Run, Service, Find (Service, C.Namespace.Root, "ECIO"), False, 1000, Small, Action);
      Check (D.Pending (Run)); Before := Calls; D.Close (Run, Service, Action);
      Check (Action = D.Accepted and D.Observe (Run).Status = D.Closed and not D.Pending (Run) and Calls = Before);
   end;
   Reset;
   declare
      Run : D.Session; Action : D.Action_Result; Buffer : Firmware_Tables.Bytes (1 .. 1024);
      Value : C.Values.Result; Status : C.Values.Access_Status;
      Unrelated : C.Values.Value_Handle;
      Shape : C.Values.Value_Description;
   begin
      C.Evaluate_Named (Service, Find (Service, C.Values.Position (Find (Service, C.Namespace.Root, "ECIO")), "_HID"), 100, Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.Object_Returned);
      Unrelated := Value.Handle; Check (C.Retained_Results (Service) = 1);
      D.Discover (Run, Service, Find (Service, C.Namespace.Root, "ECFI"), False, 1000, Buffer, Action);
      Check (Action = D.Accepted and D.Observe (Run).Status = D.Described and not D.Pending (Run));
      Check (C.Retained_Results (Service) = 1);
      C.Describe_Result (Service, Unrelated, Shape, Status);
      Check (Status = C.Values.Available and then Shape.Kind = C.Values.String_Description and then Shape.Length = 7);
      C.Release_Result (Service, Unrelated, Status);
      Check (Status = C.Values.Available and Unrelated = C.Values.No_Value and C.Retained_Results (Service) = 0);
   end;
   Check (C.Retained_Results (Service) = 0 and C.Valid (Service));
   Ada.Text_IO.Put_Line ("EC discovery checks:" & Checks'Image);
end EC_Discovery_Tests;
